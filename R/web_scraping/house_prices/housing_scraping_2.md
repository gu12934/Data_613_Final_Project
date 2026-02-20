# Housing R Web Scraping

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
# writing the file to local_drive
#write_csv(interest_rates, "C:/Users/gurmo/Downloads/interest_rates.csv")

# --- REWRITTEN PART 2 & 3: POLITE SCRAPING ---

# 1. Update the function to include a delay
get_rent_data <- function(p_from) {
  p_to <- p_from + 99
  url <- paste0("https://www.rentfaster.ca/api/map.json?price_range_adv%5Bfrom%5D=", p_from, 
                "&price_range_adv%5Bto%5D=", p_to, 
                "&city=Calgary")
  
  # Be polite: wait 2 seconds before each request to avoid 429 errors
  Sys.sleep(2) 
  
  res <- tryCatch(fromJSON(url)$listings, error = function(e) NULL)
  return(res)
}

# 2. Scrape (Only run this ONCE to avoid being banned)
# Note: I removed the duplicate 'all_listings <-' inside the cleaning block
all_listings_raw <- map(prices, get_rent_data) %>% 
  list_rbind() 

# 3. Basic Cleaning (The "Foolproof" Version)
rent_clean <- all_listings_raw %>%
  jsonlite::flatten() %>%
  clean_names() %>%
  mutate(
    date_scraped = Sys.Date(),
    final_price = as.numeric(coalesce(!!!select(., any_of(c("price", "price_month", "rent"))))),
    final_beds  = as.numeric(coalesce(!!!select(., any_of(c("bedrooms", "beds", "bedroom_number"))))),
    final_sqft  = as.numeric(coalesce(!!!select(., any_of(c("sq_feet", "square_feet", "sqft")))))
  ) %>%
  filter(!is.na(final_price)) %>%
  distinct(id, .keep_all = TRUE)

# Verification
print(paste("Success! We have", nrow(rent_clean), "listings."))



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
# writing the file to local_drive
#write_csv(rent_clean, "C:/Users/gurmo/Downloads/rent_clean.csv")
#---------------graph 2-------------------------
# Scatter plot with Jitter and a Trend Line
ggplot(rent_analysis, aes(x = factor(final_beds), y = final_price, color = segment)) +
  geom_jitter(alpha = 0.4, width = 0.2) + 
  geom_boxplot(alpha = 0.1, color = "black", outlier.shape = NA) +
  labs(title = "Rent Price vs. Number of Bedrooms (Calgary 2026)",
       subtitle = "Checking for scarcity in the 3+ bedroom market",
       x = "Number of Bedrooms", 
       y = "Monthly Rent ($)") +
  theme_minimal()
#--------graph3--------------------
# Note: You may need to clean your 'community' or 'quadrant' names first
ggplot(rent_analysis, aes(x = reorder(segment, final_price, median), y = final_price, fill = segment)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.1, color = "white", outlier.shape = NA) +
  coord_flip() +
  labs(title = "Price Range by Property Type",
       subtitle = "Violin plot showing where the 'density' of listings sits",
       x = "Property Segment", 
       y = "Rent ($)") +
  theme_light()
#------------------creating joined data------------
# --- REWRITTEN STEP 4: CLEANING & PREP ---

rent_clean <- all_listings %>%
  # 1. Flatten the JSON structure to make columns accessible
  jsonlite::flatten() %>%
  clean_names() %>%
  # 2. Critical Mutate: Create final columns and the missing date_scraped
  mutate(
    # Create the timestamp for today (February 18, 2026)
    date_scraped = Sys.Date(),
    
    # Safely find the price, beds, and sqft regardless of column naming
    final_price = as.numeric(coalesce(!!!select(., any_of(c("price", "price_month", "rent"))))),
    final_beds  = as.numeric(coalesce(!!!select(., any_of(c("bedrooms", "beds", "beds2", "bedroom_number"))))),
    final_sqft  = as.numeric(coalesce(!!!select(., any_of(c("sq_feet", "square_feet", "sqft", "sq_feet2")))))
  ) %>%
  # 3. Remove rows without a price and remove duplicate IDs
  filter(!is.na(final_price)) %>%
  distinct(id, .keep_all = TRUE)

# --- REWRITTEN SEGMENT ANALYSIS ---

rent_analysis <- rent_clean %>%
  mutate(
    segment = case_when(
      type == "Room For Rent" ~ "Shared",
      type %in% c("Condo", "Apartment") ~ "Apartment/Condo",
      type %in% c("House", "Townhouse", "Duplex") ~ "Full Home",
      TRUE ~ "Other"
    )
  )

# Verify the data exists before joining
print(paste("Verification: Date Scraped column exists:", "date_scraped" %in% names(rent_analysis)))

#----

# 1. Aggregate Interest Rates to Monthly Averages
# (This ensures you have one interest rate per month to match your data)
monthly_interest <- interest_rates %>%
  mutate(month_yr = floor_date(date, "month")) %>%
  group_by(month_yr) %>%
  summarise(avg_policy_rate = mean(policy_rate, na.rm = TRUE))

# 2. Prepare your Scraped Data for Joining
# (Since your scrape is a snapshot from today, February 2026)
rental_snapshot <- rent_analysis %>%
  mutate(month_yr = floor_date(date_scraped, "month")) %>%
  group_by(month_yr) %>%
  summarise(
    listing_count = n(),
    median_rent = median(final_price, na.rm = TRUE)
  )

# 3. Create the 'joined_data' object
joined_data <- left_join(rental_snapshot, monthly_interest, by = "month_yr")

# View the result
print(joined_data)


#-------graph4 (FIXED)---------------------------
ggplot(joined_data, aes(x = avg_policy_rate, y = listing_count)) +
  geom_point(size = 4, color = "darkblue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Correlation: BoC Policy Rate vs. Rental Inventory",
       subtitle = "Calgary Market Analysis (2026 Snapshot)",
       x = "Average Policy Rate (%)", 
       y = "Number of Available Listings") +
  theme_minimal()
#----------------------------------------------

#write_csv(all_listings, "C:/Users/gurmo/Downloads/all_listings.csv")
#---------------

# --- STATISTICAL MODEL 1: SIMPLE REGRESSION ---
# Hypothesis: Higher interest rates lead to higher rental inventory (landlords needing to rent).
model_inventory <- lm(listing_count ~ avg_policy_rate, data = joined_data)

summary(model_inventory) # This provides your P-value and R-squared

# Visualization 1: Regression Line (Content Req 4)
ggplot(joined_data, aes(x = avg_policy_rate, y = listing_count)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Statistical Model: Interest Rate Impact on Inventory",
       x = "BoC Policy Rate (%)", y = "Available Listings")

#-----------------------------
# --- STATISTICAL MODEL 2: MULTIPLE REGRESSION ---
# Hypothesis: Bedrooms and Square Footage are the primary drivers of price.
model_price <- lm(final_price ~ final_beds + final_sqft + segment, data = rent_analysis)

summary(model_price)

# Visualization 2: Coefficient Plot (Content Req 4)
# (Visualizes which factor increases rent the most)
broom::tidy(model_price) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Marginal Price Impact of Property Features",
       x = "Feature", y = "Estimated Price Increase ($)")

#------------------------
# --- FIXED MODEL 3 PREDICTION ---

# 1. Ensure 'segment' is a factor in your main data (if not already)
rent_analysis$segment <- as.factor(rent_analysis$segment)

# 2. Create the prediction data frame
pred_data <- data.frame(
  final_price = 1800, 
  segment = "Apartment/Condo"
)

# 3. THE FIX: Match the factor levels of the main data
pred_data$segment <- factor(pred_data$segment, levels = levels(rent_analysis$segment))

# 4. Now run the prediction again
prob <- predict(model_incentive, pred_data, type = "response")

cat("\n--- ACTIONABLE RECOMMENDATION ---\n")
cat("Based on the logistic model, a $1,800 apartment has a", round(prob * 100, 1), "% chance of an incentive.\n")
#------------------------------------
