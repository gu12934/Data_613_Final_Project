#Bank of Canada API
library(httr)
library(jsonlite)
library(dplyr)

# Bank of Canada API endpoint for series V39079 (policy interest rate)
boc_url <- "https://www.bankofcanada.ca/valet/observations/V39079/json"

res_boc <- GET(boc_url)

boc_json <- fromJSON(rawToChar(res_boc$content))

# Extract observations
boc_rates <- boc_json$observations %>%
  mutate(
    date = as.Date(d),
    policy_rate = as.numeric(V39079$v)
  ) %>%
  select(date, policy_rate)

head(boc_rates)

# alberta open data housing price
library(httr)
library(jsonlite)
library(dplyr)

alberta_url <- "https://open.alberta.ca/api/3/action/datastore_search?resource_id=YOUR_RESOURCE_ID&limit=5000"

res_ab <- GET(alberta_url)
ab_json <- fromJSON(rawToChar(res_ab$content))

housing_data <- ab_json$result$records

head(housing_data)

---> option B, uses API key
library(httr)
library(jsonlite)
library(keyring)

# Store your Zillow API key securely
keyring_create("Zillow_project")
key_set(service = "Zillow_API", keyring = "Zillow_project")

keyring_unlock("Zillow_project")

zillow_key <- key_get("Zillow_API", keyring = "Zillow_project")

zillow_url <- "https://api.zillow.com/v1/housing-data"

res_zillow <- GET(
  zillow_url,
  query = list(
    region = "Alberta",
    metric = "median_price",
    apikey = zillow_key
  )
)

zillow_data <- fromJSON(rawToChar(res_zillow$content), flatten = TRUE)

head(zillow_data)

# general api template
library(httr)
library(jsonlite)

api_get <- function(base_url, params = list()) {
  res <- GET(base_url, query = params)
  fromJSON(rawToChar(res$content), flatten = TRUE)
}

result <- api_get(
  base_url = "https://api.publicapis.org/entries",
  params = list(category = "Finance")
)

View(result)

#compiling it all
boc_rates

housing_data   # Alberta Open Data
# or
zillow_data    # Zillow API

merged <- boc_rates %>%
  inner_join(housing_data, by = "date")

head(merged)

# extra info

library(httr)
library(rvest)
library(stringr)
library(ggplot2)
library(janitor) # Great for cleaning column names

# URL for Policy Interest Rate
boc_url <- "https://www.bankofcanada.ca/rates/interest-rates/canadian-interest-rates/"

# Read the page
boc_page <- read_html(boc_url)

# Extract the table
# Note: CSS selectors like 'table' or specific IDs are used
interest_rates <- boc_page %>%
  html_element("table") %>% 
  html_table() %>%
  clean_names()

# Basic Cleaning: Convert date strings to Date objects
# Adjust the column name based on what the scrape returns (usually 'date' or 'effective_date')
interest_rates$date <- as.Date(interest_rates$date)

print(head(interest_rates))

statcan_url <- "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1810020501"

# StatCan tables often load via JavaScript, so if a simple scrape fails, 
# you may need to use the 'Download' button URL instead.
statcan_data <- read_html(statcan_url) %>%
  html_element("#table_1") %>% # Targeting the specific table ID
  html_table()

print(head(statcan_data))

creb_url <- "https://www.creb.com/News_and_Events/Statistics/"

creb_tables <- read_html(creb_url) %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

# Usually, CREB has multiple tables; you'll need to find which index is correct
calgary_benchmark <- creb_tables[[1]]

# Mock Visualization Code
ggplot(interest_rates, aes(x = date, y = policy_rate)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Bank of Canada Interest Rate Trend",
       subtitle = "Data scraped for Housing Final Project",
       x = "Year", y = "Rate (%)") +
  theme_minimal()

response <- GET(url, add_headers(`User-Agent` = "Mozilla/5.0"))
page <- read_html(response)
