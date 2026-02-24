# ==========================================================
# DATA 613 FINAL PROJECT: Calgary Rental Market Analysis (2026)
# ==========================================================
# Clearing global directory
rm(list = ls())

# Load Required Libraries
library(httr)
library(rvest)
library(stringr)
library(ggplot2)
library(janitor) 
library(dplyr)
library(readr)
library(jsonlite)
library(tidyr)
library(lubridate)
library(purrr)
library(broom)
library(corrplot)
library(GGally)

# ==========================================================
# PART 1: DATA COLLECTION (Automated/Scraping)
# Requirement: 4 Automated/Scraped Questions
# ==========================================================

# 1.1 Bank of Canada API for Interest Rates
boc_csv <- "https://www.bankofcanada.ca/valet/observations/V39079/csv"
raw_boc <- read_lines(boc_csv)
header_line <- grep("OBSERVATIONS", raw_boc) + 1
interest_rates <- read_csv(boc_csv, skip = header_line - 1) %>%
  clean_names() %>%
  mutate(date = as.Date(date)) %>%
  rename_with(~ "policy_rate", -date)

# 1.2 RentFaster API Scraper for Calgary Listings
prices <- seq(500, 4000, 100) 
get_rent_data <- function(p_from) {
  p_to <- p_from + 99
  url <- paste0("https://www.rentfaster.ca/api/map.json?price_range_adv%5Bfrom%5D=", p_from, 
                "&price_range_adv%5Bto%5D=", p_to, "&city=Calgary")
  Sys.sleep(1.5) # Polite scraping delay
  res <- tryCatch(fromJSON(url)$listings, error = function(e) NULL)
  return(res)
}
all_listings_raw <- map(prices, get_rent_data) %>% list_rbind() 

# ==========================================================
# PART 2: DATA CLEANING & FEATURE ENGINEERING
# ==========================================================

rent_clean <- all_listings_raw %>%
  jsonlite::flatten() %>%
  clean_names() %>%
  mutate(
    date_scraped = Sys.Date(),
    final_price = as.numeric(coalesce(!!!dplyr::select(., any_of(c("price", "price_month", "rent"))))),
    final_beds  = as.numeric(coalesce(!!!dplyr::select(., any_of(c("bedrooms", "beds", "bedroom_number"))))),
    final_sqft  = as.numeric(coalesce(!!!dplyr::select(., any_of(c("sq_feet", "square_feet", "sqft"))))),
    # Feature for Logistic Model (Primary Q3)
    listing_text = coalesce(!!!dplyr::select(., any_of(c("intro", "title", "description")))),
    has_incentive = ifelse(grepl("free|incentive|discount|reduced|deal", listing_text, ignore.case = TRUE), 1, 0),
    # Segmenting (Primary Q2)
    segment = case_when(
      type == "Room For Rent" ~ "Shared",
      type %in% c("Condo", "Apartment") ~ "Apartment/Condo",
      type %in% c("House", "Townhouse", "Duplex") ~ "Full Home",
      TRUE ~ "Other"
    ),
    segment = as.factor(segment)
  ) %>%
  filter(!is.na(final_price)) %>%
  distinct(id, .keep_all = TRUE)

# ==========================================================
# PART 3: PRIMARY & SECONDARY QUESTIONS (Analysis)
# Requirement: 4 Regression Models & 1 Actionable Recommendation
# ==========================================================

# --- PRIMARY Q1: Interest vs Inventory (Simple Regression) ---
monthly_interest <- interest_rates %>%
  mutate(month_yr = floor_date(date, "month")) %>%
  group_by(month_yr) %>%
  summarise(avg_policy_rate = mean(policy_rate, na.rm = TRUE))

joined_data <- rent_clean %>%
  mutate(month_yr = floor_date(date_scraped, "month")) %>%
  group_by(month_yr) %>%
  summarise(listing_count = n(), median_rent = median(final_price)) %>%
  left_join(monthly_interest, by = "month_yr")

# If more than 1 month exists, run Q1.2: Time Lag (Secondary)
model_q1 <- lm(listing_count ~ avg_policy_rate, data = joined_data)

# --- PRIMARY Q2: Pricing Model (Multiple Regression) ---
# SECONDARY Q1.1: Interaction term (Price ~ policy_rate * segment)
model_q2 <- lm(final_price ~ final_beds + final_sqft + segment, data = rent_clean)

# --- PRIMARY Q3: Incentive Probability (Logistic Regression) ---
# Requirement: Actionable Recommendation
model_q3 <- glm(has_incentive ~ final_price + segment, data = rent_clean, family = "binomial")

# ==========================================================
# PART 4: VISUALIZATIONS
# Requirement: 3 Data Visualizations
# ==========================================================

# Correlation Matrix Heatmap
cor_data <- rent_clean %>% dplyr::select(final_price, final_beds, final_sqft) %>% na.omit() 
corrplot(cor(cor_data), method = "color", type = "upper", addCoef.col = "black", title = "Feature Correlation")

# Price Range by Property Type (Violin Plot)
ggplot(rent_clean, aes(x = reorder(segment, final_price, median), y = final_price, fill = segment)) +
  geom_violin(alpha = 0.5) + labs(title = "Price Range by Property Type", x = "Segment", y = "Rent ($)")

# ==========================================================
# ACTIONABLE RECOMMENDATION
# ==========================================================
pred_data <- data.frame(final_price = 1800, segment = "Apartment/Condo")
pred_data$segment <- factor(pred_data$segment, levels = levels(rent_clean$segment))
prob <- predict(model_q3, pred_data, type = "response")

cat("\n--- DECISION ANALYSIS RECOMMENDATION ---\n")
cat("A $1,800 unit in the Apartment/Condo segment has a", round(prob * 100, 1), "% chance of an incentive.\n")
