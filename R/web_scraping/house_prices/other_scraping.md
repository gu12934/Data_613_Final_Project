# BOC scrape interest table

library(rvest)
library(dplyr)
library(stringr)

boc_url <- "https://www.bankofcanada.ca/rates/interest-rates/canadian-interest-rates/"

page <- read_html(boc_url)

tables <- page %>% html_nodes("table")

# Usually the first table contains the policy interest rate history
rate_table <- html_table(tables[[1]], fill = TRUE)

# Clean column names
rate_table <- rate_table %>%
  rename(
    date = 1,
    rate = 2
  ) %>%
  mutate(
    date = as.Date(date),
    rate = as.numeric(str_replace(rate, "%", "")) / 100
  )

head(rate_table)

#
library(rvest)
library(dplyr)

creb_url <- "https://www.creb.com/News_and_Events/Statistics/"

page <- read_html(creb_url)

tables <- page %>% html_nodes("table")

creb_data <- html_table(tables[[1]], fill = TRUE)

head(creb_data)


creb_tables <- lapply(tables, html_table, fill = TRUE)
str(creb_tables)

# alberta real estate
library(rvest)

area_url <- "https://www.albertarealtor.ca/market-stats/"

page <- read_html(area_url)

tables <- page %>% html_nodes("table")

area_data <- html_table(tables[[1]], fill = TRUE)

head(area_data)

#stats canada
library(jsonlite)
library(dplyr)

statcan_url <- "https://www150.statcan.gc.ca/t1/wds/en/grp?pid=1810020501"

stat_raw <- fromJSON(statcan_url)

# Extract data
stat_data <- stat_raw$object$vectorDataPoint

head(stat_data)

- Scrapes Bank of Canada
- Scrapes CREB
- Scrapes AREA
- Pulls StatCan API
- Cleans and merges all datasets
- Saves them as CSVs
- Prepares them for modeling
