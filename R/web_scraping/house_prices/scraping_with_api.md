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
