# mirrors etherscan workflow
library(httr)
library(jsonlite)

api_get <- function(base_url, params = list()) {
  res <- GET(base_url, query = params)
  stop_for_status(res)
  fromJSON(rawToChar(res$content), flatten = TRUE)
}

result <- api_get(
  base_url = "https://api.publicapis.org/entries",
  params = list(category = "Finance")
)

View(result)

#
library(httr)
library(jsonlite)
library(dplyr)

boc_url <- "https://www.bankofcanada.ca/valet/observations/V39079/json"

boc_raw <- GET(boc_url)
boc_json <- fromJSON(rawToChar(boc_raw$content))

boc_rates <- boc_json$observations %>%
  mutate(
    date = as.Date(d),
    policy_rate = as.numeric(V39079$v)
  ) %>%
  select(date, policy_rate)

head(boc_rates)

#
library(httr)
library(jsonlite)
library(dplyr)

resource_id <- "YOUR_RESOURCE_ID"

ab_url <- paste0(
  "https://open.alberta.ca/api/3/action/datastore_search?",
  "resource_id=", resource_id,
  "&limit=5000"
)

ab_raw <- GET(ab_url)
ab_json <- fromJSON(rawToChar(ab_raw$content))

housing_data <- ab_json$result$records

head(housing_data)

#keyring
library(httr)
library(jsonlite)
library(keyring)

keyring_create("Zillow_project")
key_set(service = "Zillow_API", keyring = "Zillow_project")

keyring_unlock("Zillow_project")
zillow_key <- key_get("Zillow_API", keyring = "Zillow_project")

zillow_url <- "https://api.zillow.com/v1/housing-data"

zillow_raw <- GET(
  zillow_url,
  query = list(
    region = "Alberta",
    metric = "median_price",
    apikey = zillow_key
  )
)

zillow_data <- fromJSON(rawToChar(zillow_raw$content), flatten = TRUE)
head(zillow_data)

#
