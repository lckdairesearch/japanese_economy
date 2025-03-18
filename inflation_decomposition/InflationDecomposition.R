# INFLATION DECOMPOSITION - SUPPLY AND DEMAND COMPONENTS - JAPAN

# SETUP ----
# install.packages('lubridate')
# install.packages('seasonal')
library(tidyverse)
library(lubridate)
library(seasonal)
library(dynlm)
library(clipr)
library(scales) 

# DATA ----
source('RetrieveJapanData.R')


# Data Preprocessing -----------------------------------------------------------
## CPI ----
cpi_final <- cpi %>%
  drop_na(value) %>%
  left_join(cpi_classes$time, by= 'time') %>%
  filter(time_level == 4) %>%
  filter(is_commodity == TRUE) %>%
  filter(cat01_level %in% c(4, 5)) %>%
  rename(
    cpi_adjusted = value, 
  ) %>%
  select(cat01, cat01_name, cat01_level, com_desc, date, cpi_adjusted)

print("Non-unique values at com_desc, cat01_level, time, expected none")
cpi_final %>%
  group_by(com_desc, cat01_level, date) %>%
  tally() %>%
  filter(n > 1) %>%
  print()



## Consumption ----
hh_consumption_ = hh_consumption %>%
  drop_na(value) %>%
  filter(cat02 == "11") %>%
  filter(is_commodity == TRUE) %>%
  filter(cat01_level == 3)

### Seasonal Adjustment
hh_consumption_final = hh_consumption_ %>%
  select(starts_with("cat01"), starts_with("com"), starts_with("cat02"), date, value) %>%
  group_by(across(-c(value, date))) %>%
  group_modify(~ {
    df <- .x
    start_year <- year(min(df$date))
    start_quarter <- quarter(min(df$date))
    ts_data <- ts(df$value, start = c(start_year, start_quarter), frequency = 4)
    fit <- seas(ts_data)
    if (!is.null(final(fit))) {
      df$value_adjusted <- as.numeric(final(fit))
    } else {
      df$value_adjusted <- df$value
    }
    df
  }) %>%
  ungroup() %>%
  rename(
    hh_consumption = value,
    hh_consumption_adjusted = value_adjusted
  )

## Merging ----
desc_concordance <- data.frame(
  hh_com_desc = c("米", "パン", "麺類", "他の穀類", "生鮮魚介", "塩干魚介", "魚肉練製品", "他の魚介加工品",
                  "生鮮肉", "加工肉", "牛乳", "乳製品", "卵", "生鮮野菜", "乾物・海藻", "大豆加工品",
                  "他の野菜・海藻加工品", "生鮮果物", "果物加工品", "油脂", "調味料", "主食的調理食品",
                  "他の調理食品", "茶類", "コーヒー・ココア", "他の飲料", "一般外食", "学校給食",
                  "設備材料", "工事その他のサービス", "家事用耐久財", "冷暖房用器具", "一般家具",
                  "男子用洋服", "婦人用洋服", "子供用洋服", "男子用シャツ・セーター類", "婦人用シャツ・セーター類",
                  "子供用シャツ・セーター類", "男子用下着類", "婦人用下着類", "子供用下着類",
                  "自動車等購入", "自転車購入", "自動車等維持", "宿泊料", "パック旅行費", "月謝類",
                  "他の教養娯楽サービス"),
  cpi_com_desc = c("米類", "パン", "麺類", "他の穀類", "生鮮魚介", "塩干魚介", "魚肉練製品", "他の魚介加工品",
                   "生鮮肉", "加工肉", "牛乳", "乳製品", "卵", "生鮮野菜", "乾物・海藻", "大豆加工品",
                   "他の野菜・海藻加工品", "生鮮果物", "果物加工品", "油脂", "調味料", "主食的調理食品",
                   "他の調理食品", "茶類", "コーヒー・ココア", "他の飲料", "一般外食", "学校給食",
                   "設備材料", "工事その他のサービス", "家事用耐久財", "冷暖房用器具", "一般家具",
                   "男子用洋服", "婦人用洋服", "子供用洋服", "男子用シャツ・セーター類", "婦人用シャツ・セーター類",
                   "子供用シャツ・セーター類", "男子用下着類", "婦人用下着類", "子供用下着類",
                   "自動車", "自転車", "自動車等維持", "宿泊料", "パック旅行費", "月謝類",
                   "他の教養娯楽サービス"),
  stringsAsFactors = FALSE
)

hh_consumption_tomerge <- hh_consumption_final %>%
  rename(hh_com_desc = com_desc) %>%
  select(hh_com_desc, date, hh_consumption_adjusted) %>%
  arrange(hh_com_desc, date, hh_consumption_adjusted)

cpi_tomerge <- cpi_final %>%
  filter(com_desc %in% unique(desc_concordance$cpi_com_desc)) %>%
  rename(cpi_com_desc = com_desc) %>%
  left_join(desc_concordance, by = "cpi_com_desc") %>%
  select(hh_com_desc, date, cpi_adjusted) %>%
  arrange(hh_com_desc, date, cpi_adjusted)

inflation_decompose_data <- hh_consumption_tomerge %>%
  left_join(cpi_tomerge, by = c('hh_com_desc', 'date'))


# Inflation Decomposition ---------------------

price_var <- "price_log_diff"
quantity_var <- "quantity_log_diff"

thresh1 <- 0.1
thresh2 <- 0.2



estimate_price_and_quantity_equations <- function(df, price_var = "price_log_diff", quantity_var = "quantity_log_diff") {
  start_year <- year(min(df$date))
  start_quarter <- quarter(min(df$date))
  price_ts <- ts(df[[price_var]], start = c(start_year, start_quarter), frequency = 4)
  quantity_ts <- ts(df[[quantity_var]], start = c(start_year, start_quarter), frequency = 4)
  data_reg <- ts.union(price_ts, quantity_ts)
  trend <- as.numeric(time(price_ts))
  data_reg <- cbind(data_reg, trend = trend, trend2 = trend^2, trend3 = trend^3)
  price_model <- dynlm(price_ts ~ L(price_ts, 1:4) + L(quantity_ts, 1:4) + trend + trend2 + trend3, data = data_reg)
  price_resid_std <- residuals(price_model) / sd(residuals(price_model))
  quantity_model <- dynlm(quantity_ts ~ L(price_ts, 1:4) + L(quantity_ts, 1:4) + trend + trend2 + trend3, data = data_reg)
  quantity_resid_std <- residuals(quantity_model) / sd(residuals(quantity_model))
  lambda <- residuals(price_model) * residuals(quantity_model)
  lambda_std <- lambda / sd(lambda, na.rm = TRUE)
  weight_demand <- pt(lambda_std, df = length(lambda_std) - 2)
  weight_supply <- 1 - weight_demand
  
  
  
  res_df <- data.frame(date = as.Date(as.yearqtr(time(price_resid_std)), frac = 1),
                       price_resid = as.numeric(residuals(price_model)),
                       price_resid_std = as.numeric(price_resid_std),
                       quantity_resid = as.numeric(residuals(quantity_model)),
                       quantity_resid_std = as.numeric(quantity_resid_std),
                       lambda = as.numeric(lambda),
                       lambda_std = as.numeric(lambda_std),
                       inflation_weight_demand = as.numeric(weight_demand),
                       inflation_weight_supply = as.numeric(weight_supply)
                       )
  merge(df, res_df, by = "date", all.x = TRUE)
}



inflation_decompose_data <- inflation_decompose_data %>%
  group_by(hh_com_desc) %>%
  arrange(date) %>%
  mutate(
    price = as.numeric(cpi_adjusted),
    quantity = as.numeric(hh_consumption_adjusted) / price,
    inflation = (price/lag(price,4) -1)*100,
    price_log = log(price),
    quantity_log = log(quantity),
    price_log_diff = price_log - lag(price_log),
    quantity_log_diff = quantity_log - lag(quantity_log)
  ) %>%
  ungroup() %>%
  arrange(hh_com_desc)


inflation_decompose_data <- inflation_decompose_data %>%
  group_by(hh_com_desc) %>%
  group_modify(~ estimate_price_and_quantity_equations(.x))

# Decompose inflation
inflation_decomposed <- inflation_decompose_data %>%
  mutate(
    shock_d_s = case_when(
      (price_resid_std * quantity_resid_std) > 0 ~ "Demand",
      (price_resid_std * quantity_resid_std) < 0 ~ "Supply",
      TRUE ~ NA_character_
    ),
    shock_d_s_type = case_when(
      price_resid_std > 0 & quantity_resid_std > 0 ~ "Positive Demand",
      price_resid_std < 0 & quantity_resid_std < 0 ~ "Negative Demand",
      price_resid_std < 0 & quantity_resid_std > 0 ~ "Positive Supply",
      price_resid_std > 0 & quantity_resid_std < 0 ~ "Negative Supply",
      TRUE ~ NA_character_
    ),
    shock_d_s_thresh1 = case_when(
      (price_resid_std > thresh1 & quantity_resid_std > thresh1) |
        (price_resid_std < -thresh1 & quantity_resid_std < -thresh1) ~ "Demand",
      (price_resid_std < -thresh1 & quantity_resid_std > thresh1) |
        (price_resid_std > thresh1 & quantity_resid_std < -thresh1) ~ "Supply",
      is.na(shock_d_s) ~ NA_character_,
      TRUE ~ "Ambiguous"
    ),
    shock_d_s_thresh2 = case_when(
      (price_resid_std > thresh2 & quantity_resid_std > thresh2) |
        (price_resid_std < -thresh2 & quantity_resid_std < -thresh2) ~ "Demand",
      (price_resid_std < -thresh2 & quantity_resid_std > thresh2) |
        (price_resid_std > thresh2 & quantity_resid_std < -thresh2) ~ "Supply",
      is.na(shock_d_s) ~ NA_character_,
      TRUE ~ "Ambiguous"
    ),
  )


# Weight -----------------------------------------------------------------------
inflation_decomposed <- inflation_decomposed %>%
  group_by(date) %>%
  mutate(total_hh_consumption = sum(hh_consumption_adjusted, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(weight = hh_consumption_adjusted / total_hh_consumption) %>%
  group_by(hh_com_desc) %>%
  mutate(lag_hh_consumption = lag(hh_consumption_adjusted)) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(total_lag_hh_consumption = sum(lag_hh_consumption, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(laspeyres_weight = lag_hh_consumption / total_lag_hh_consumption) 

write_rds(inflation_decomposed, "inflation_decomposed.rds")