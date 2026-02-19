# clearing global directory
rm(list = ls())
#----------------------------------------------------------
# Install missing packages if necessary
# install.packages(c("tidyverse", "rvest", "httr", "janitor"))
library(httr)
library(rvest)
library(stringr)
library(ggplot2)
library(janitor) # Great for cleaning column names
library(dplyr)
library(readr)
library(jsonlite)
library(tidyr)
library(lubridate)
library(purrr)
#--------------------------------------------------
# URL for Policy Interest Rate
boc_url <- "https://www.bankofcanada.ca/rates/interest-rates/canadian-interest-rates/"

# Read the page
boc_page <- read_html(boc_url)

# Extract the table
# Note: CSS selectors like 'table' or specific IDs are used
interest_rates <- boc_page %>%
  html_element("table") %>% 
  html_table() %>%
  clean_names() # Clean column names for easier handling

# check names, i got the first table out
names(interest_rates)

# Extract ALL tables
tables <- boc_page %>% html_elements("table")

length(tables)   # See how many tables exist

#printing each table to find the one i need
for (i in seq_along(tables)) {
  cat("\n\n=== TABLE", i, "===\n\n")
  print(html_table(tables[[i]]))
}
#--------------------------------

#---test 1----
# URL
boc_csv <- "https://www.bankofcanada.ca/valet/observations/V39079/csv"

# Step 1: Read raw text
raw <- read_lines(boc_csv)

# Step 2: Find the line number where OBSERVATIONS appears
obs_line <- grep("OBSERVATIONS", raw)

# Step 3: The actual header is the next line
header_line <- obs_line + 1

# Step 4: Read the CSV starting from the header line
interest_rates <- read_csv(boc_csv, skip = header_line - 1)

# Step 5: Clean names
interest_rates <- clean_names(interest_rates)

# Step 6: Convert date column
interest_rates$date <- as.Date(interest_rates$date)

# Preview
head(interest_rates)

# writing the file to local_drive
#write_csv(interest_rates, "C:/Users/gurmo/Downloads/interest_rates.csv")
#-------------part2---------------

#install.packages("purrr") 


# 1. Define price ranges to ensure we get all data (RentFaster limits API returns)
prices <- seq(500, 4000, 100) 

# 2. Build the API URL for Calgary
# Note: The 'area' coordinates define the bounding box for Calgary
get_rent_data <- function(p_from) {
  p_to <- p_from + 99
  url <- paste0("https://www.rentfaster.ca/api/map.json?price_range_adv%5Bfrom%5D=", p_from, 
                "&price_range_adv%5Bto%5D=", p_to, 
                "&city=Calgary")
  
  # Try to fetch data; use safely to prevent crashes if a price bracket is empty
  res <- tryCatch(fromJSON(url)$listings, error = function(e) NULL)
  return(res)
}

# 3. Scrape and Combine
all_listings <- map_df(prices, get_rent_data) %>% clean_names()

# 4. Basic Cleaning (Robust Version)
rent_clean <- all_listings %>%
  # 1. First, make sure we only keep unique listings (scraping by price can cause overlaps)
  distinct(id, .keep_all = TRUE) %>%
  
  # 2. Convert price (this is the most important one)
  mutate(price = as.numeric(price)) %>%
  
  # 3. Modern Scrape and Combine (with Flattening)
  all_listings <- map(prices, get_rent_data) %>% 
  list_rbind() 

# --- IMPORTANT TROUBLESHOOTING STEP ---
# If you get 'object not found', run this line to see the real names:
# names(all_listings)

# 4. Basic Cleaning (The "Foolproof" Version)
rent_clean <- all_listings %>%
  # Flatten if columns are nested in a 'listing' or 'attributes' column
  jsonlite::flatten() %>%
  clean_names() %>%
  # RentFaster sometimes uses 'price_month' or 'price'
  # We use any_of to select what exists
  mutate(
    final_price = as.numeric(coalesce(!!!select(., any_of(c("price", "price_month", "rent"))))),
    final_beds  = as.numeric(coalesce(!!!select(., any_of(c("bedrooms", "beds", "bedroom_number"))))),
    final_sqft  = as.numeric(coalesce(!!!select(., any_of(c("sq_feet", "square_feet", "sqft")))))
  ) %>%
  filter(!is.na(final_price)) %>%
  distinct(id, .keep_all = TRUE)

# Verification
print(paste("Success! We have", nrow(rent_clean), "listings."))
summary(rent_clean$final_price)

#analysis--------------------------------------------
# Create Segments
rent_analysis <- rent_clean %>%
  mutate(
    segment = case_when(
      type == "Room For Rent" ~ "Shared",
      type %in% c("Condo", "Apartment") ~ "Apartment/Condo",
      type %in% c("House", "Townhouse", "Duplex") ~ "Full Home",
      TRUE ~ "Other"
    )
  )

# Calculate Median Rent by Segment
segment_summary <- rent_analysis %>%
  group_by(segment) %>%
  summarise(
    count = n(),
    avg_price = mean(final_price, na.rm = TRUE),
    median_price = median(final_price, na.rm = TRUE)
  )

print(segment_summary)
#-----------------------------

# --- PART 1: INTEREST RATES (Robust Version) ---
boc_csv <- "https://www.bankofcanada.ca/valet/observations/V39079/csv"
raw_boc <- read_lines(boc_csv)
header_line <- grep("OBSERVATIONS", raw_boc) + 1

interest_rates <- read_csv(boc_csv, skip = header_line - 1) %>%
  clean_names() %>%
  mutate(date = as.Date(date)) %>%
  # Dynamically rename the first column that isn't 'date' to 'policy_rate'
  rename_with(~ "policy_rate", -date)

# Now this will work
current_rate <- interest_rates %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  pull(policy_rate)

print(paste("Current BoC Policy Rate:", current_rate, "%"))
#-----------------------------------
# Get the latest interest rate from your Part 1 data
current_rate <- interest_rates %>% arrange(desc(date)) %>% slice(1) %>% pull(policy_rate)

# Visualize the Current Market Distribution
ggplot(rent_analysis, aes(x = final_price, fill = segment)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 1650, linetype = "dashed", color = "red") +
  labs(title = paste("Calgary Rental Market in 2026 (BoC Rate:", current_rate, "%)"),
       subtitle = "Red line represents the city-wide median rent",
       x = "Monthly Rent ($)", y = "Number of Listings") +
  theme_minimal()
#---------------------------------------
