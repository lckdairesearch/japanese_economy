library(dplyr)
library(shiny)
library(plotly)

# setwd('/Users/leodai/Library/CloudStorage/OneDrive-Personal/Work/Blog/6 Japan/japan_an')


# data downloaded from "https://data-explorer.oecd.org/vis?lc=en&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_NAMAIN10%40DF_TABLE1_EXPENDITURE&df[ag]=OECD.SDD.NAD&df[vs]=2.0"
gdp <- read.csv('OECDGDPComponent.csv', header = TRUE, stringsAsFactors = FALSE)

oecd_countries <- c(
  "United Kingdom", "Netherlands", "Canada", "Spain", "Italy", "Israel",
  "Finland", "Chile", "Colombia", "United States", "Sweden", "Czechia",
  "Türkiye", "New Zealand", "Mexico", "Switzerland", "Iceland", "Denmark",
  "Germany", "France", "Estonia", "Lithuania", "Slovak Republic", "Australia",
  "Korea", "Slovenia", "Latvia", "Hungary", "Belgium", "Austria", "Norway",
  "Luxembourg", "Croatia", "Greece", "Ireland", "Japan"
)


gdp_category <- data.frame(
  Sector = c("S1", "S13", "S1M", "S1", "S1"),
  Transaction = c("P5", "P3", "P3", "P7", "P6"),
  GDPComponent = c("Investment", "GovernmentExpenditure", "Consumption", "Import", "Export"),
  stringsAsFactors = FALSE
)

gdp <- gdp %>%
  select(REF_AREA, Reference.area, SECTOR, TRANSACTION, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, PRICE_BASE) %>%
  rename(
    CountryCode = REF_AREA,
    Country = Reference.area,
    Sector = SECTOR,
    Transaction = TRANSACTION,
    Year = TIME_PERIOD,
    Value = OBS_VALUE,
    UnitMeasure = UNIT_MEASURE,
    PriceBase = PRICE_BASE
  ) %>%
  mutate(ValueTrillion = Value / 1e6) %>%
  mutate(
    isOECD = Country %in% oecd_countries
  ) %>%
  filter(isOECD) %>%
  left_join(gdp_category, by = c("Sector", "Transaction")) %>%
  filter(!is.na(GDPComponent)) %>%
  filter(PriceBase == "V")

gdp_2023 <- gdp %>%
  filter(Year == 2023) %>%
  filter(UnitMeasure == "USD_EXC")

write.csv(gdp_2023, 'gdp_2023.csv', row.names = FALSE)



