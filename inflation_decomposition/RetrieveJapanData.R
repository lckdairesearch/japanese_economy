library(httr)
library(jsonlite)
library(tidyverse)
library(zeallot)

app_id <- Sys.getenv("ESTAT_APP_ID")


# Function to fetch JSON data from the API with error checking
fetch_data <- function(app_id, stats_data_id, start_position = 1, extra_params = list()) {
  base_url <- "http://api.e-stat.go.jp/rest/3.0/app/json/getStatsData"
  params <- list(
    appId = app_id,
    lang = "J",
    statsDataId = stats_data_id,
    metaGetFlg = "Y",
    cntGetFlg = "N",
    explanationGetFlg = "Y",
    annotationGetFlg = "Y",
    sectionHeaderFlg = "1",
    replaceSpChars = "0",
    startPosition = start_position
  )
  
  params <- c(params, extra_params)
  
  response <- GET(base_url, query = params)
  if (response$status_code != 200) {
    stop("Error: API call failed with status code ", response$status_code)
  }
  json_text <- content(response, as = "text", encoding = "UTF-8")
  fromJSON(json_text, simplifyVector = FALSE)$GET_STATS_DATA$STATISTICAL_DATA
}


# Function to process value data
process_value_data <- function(data_inf) {
  map_dfr(data_inf$VALUE, ~ tibble(
    tab   = .x$`@tab`,
    cat01 = .x$`@cat01`,
    cat02 = .x$`@cat02`,
    area  = .x$`@area`,
    time  = .x$`@time`,
    unit  = .x$`@unit`,
    value = .x$`$`
  ))
}

# Function to process class descriptions
process_class_desc <- function(class_obj) {
  class_id <- class_obj$`@id`
  class_list_classes <- if (is.list(class_obj$CLASS[[1]]) == FALSE) list(class_obj$CLASS) else class_obj$CLASS
  map_dfr(class_list_classes, ~ tibble(
    code  = .x$`@code`,
    name  = .x$`@name`,
    level = .x$`@level`
  )) %>%
    rename(!!class_id := code) %>%
    rename_with(~ paste0(class_id, "_", .), .cols = -!!class_id)
}


# Function for getting Japan e.stat data
get_japan_estat <- function(app_id, stats_data_id, extra_params = list()){
  start_position <- 1
  call_no <- 1
  value_dfs <- list()
  class_descriptions <- list()
  
  repeat {
    stat_data <- fetch_data(app_id, stats_data_id, start_position, extra_params)
  
    value <- process_value_data(stat_data$DATA_INF)
    value_dfs[[call_no]] <- value

    if (call_no == 1) {
      classes = sapply(stat_data$CLASS_INF$CLASS_OBJ, function(x) x$`@id`)
      for (i in as.list(1:length(classes))){
        class_descriptions[[ classes[i] ]] <- process_class_desc(stat_data$CLASS_INF$CLASS_OBJ[[i]])
      }
    }
    
    total_number <- stat_data$RESULT_INF[['TOTAL_NUMBER']]
    from_number <- stat_data$RESULT_INF[['FROM_NUMBER']]
    to_number <- stat_data$RESULT_INF[['TO_NUMBER']]
    print(sprintf("%s/%s fetched and processed", to_number, total_number))
    
    next_key <- stat_data$RESULT_INF[['NEXT_KEY']]
    if (is.null(next_key)) break
      
    start_position <- next_key
    call_no <- call_no + 1
  }
  
  value <- bind_rows(value_dfs)
  
  list(data = value, data_classes = class_descriptions)
}

# Household Consumption --------------------------------------------------------
# 家庭調查 / 家庭收支篇 總住戶
# Household Consumption by Category, Quarterly, Not Seasonally Adjusted
# https://www.e-stat.go.jp/stat-search/database?page=1&layout=datalist&toukei=00200561&bunya_l=07&bunya_s=0704&tstat=000000330001&cycle=2&tclass1=000000330001&tclass2=000000330019&statdisp_id=0003348231&tclass3val=0&metadata=1&data=0
hh_consumption_id = "0003348231"
c(hh_consumption_, hh_consumption_classes) %<-% get_japan_estat(app_id, hh_consumption_id)

hh_consumption <- hh_consumption_ %>%
  select(-c("tab", "area")) %>%
  # Value
  mutate(value = as.numeric(value))%>%
  # Time
  mutate(
    year = substr(time, 1, 4),
    year_type = substr(time, 5, 6),
    month_start = substr(time, 7, 8),
    month_end = substr(time, 9, 10),
    temp_date = as.Date(paste0(year, "-", month_end, "-01")),
    date = as.Date(ceiling_date(temp_date, "month") - days(1))
  ) %>%
  select(-temp_date) %>%
  # Commodity
  left_join(hh_consumption_classes$cat01, by = "cat01") %>%
  mutate(
    is_commodity = as.numeric(substr(cat01, 1, 2)) > 0 & as.numeric(substr(cat01, 1, 2)) <= 9,
    com_lv1 = substr(cat01, 1, 2),
    com_lv2 = substr(cat01, 3, 4),
    com_lv3 = substr(cat01, 5, 5),
    com_desc = trimws(gsub("^[0-9\\.]+)?\\s*", "", cat01_name))
  )


# Price Level ------------------------------------------------------------------
# 消費者物価指数 / 2020年基準消費者物価指数 / 長期時系列データ 品目別価格指数 全国 月次
# Consumer Price Index, 2020 base, by Category, Seasonally Adjusted
# https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00200573&bunya_l=07&bunya_s=0703&tstat=000001150147&cycle=0&tclass1=000001150151&tclass2=000001150152&tclass3=000001150153&tclass4=000001150156&stat_infid=000032103846&tclass5val=0&metadata=1&data=0
cpi_id <- "0003427113"
cpi_extra_params = list(
  cdTab = "1",
  cdArea="00000",
  lvTime = "4"
)
c(cpi_, cpi_classes) %<-% get_japan_estat(app_id, cpi_id, cpi_extra_params)

cpi <- cpi_ %>%
  # Time
  mutate(
    year = substr(time, 1, 4),
    year_type = substr(time, 5, 6),
    month_start = substr(time, 7, 8),
    month_end = substr(time, 9, 10),
    temp_date = as.Date(paste0(year, "-", month_end, "-01")),
    date = as.Date(ceiling_date(temp_date, "month") - days(1))
  ) %>%
  select(-temp_date) %>%
  # Commodity
  left_join(cpi_classes$cat01, by="cat01") %>%
  mutate(
    cat01_head = substr(cat01, 1, 2),
    is_commodity = case_when(
      cat01 == "0001" ~ F,
      (as.numeric(cat01_head) < 2) & cat01 != "0001" ~ T,
      (as.numeric(cat01_head) >= 2) &  (as.numeric(cat01_head) < 10) ~ F,
      (as.numeric(cat01_head) >= 10) ~ T,
      TRUE ~ F),
    com_desc = trimws(gsub("^[0-9\\.]+)?\\s*", "", cat01_name))
  )


# Final Data -------------------------------------------------------------------
rm(list=setdiff(ls(), c("hh_consumption", "hh_consumption_classes", "cpi", "cpi_classes")))

