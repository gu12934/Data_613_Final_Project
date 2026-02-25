# ==========================================================
# DATA 613 FINAL PROJECT: Calgary Rental Market Analysis (2026)
# ==========================================================
# Clearing global directory
# rm(list = ls())

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

# Get the latest rate for reporting
current_rate <- interest_rates %>% arrange(desc(date)) %>% slice(1) %>% pull(policy_rate)
print(paste("Current BoC Policy Rate:", current_rate, "%"))

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
    # Robust column mapping using coalesce
    final_price = as.numeric(coalesce(!!!dplyr::select(., any_of(c("price", "price_month", "rent"))))),
    final_beds  = as.numeric(coalesce(!!!dplyr::select(., any_of(c("bedrooms", "beds", "bedroom_number"))))),
    final_sqft  = as.numeric(coalesce(!!!dplyr::select(., any_of(c("sq_feet", "square_feet", "sqft"))))),
    # Feature for Logistic Model (Primary Q3): Identifying Incentives
    listing_text = coalesce(!!!dplyr::select(., any_of(c("intro", "title", "description")))),
    has_incentive = ifelse(grepl("free|incentive|discount|reduced|deal", listing_text, ignore.case = TRUE), 1, 0),
    # Segmenting for Primary Question 2
    segment = case_when(
      type == "Room For Rent" ~ "Shared",
      type %in% c("Condo", "Apartment") ~ "Apartment/Condo",
      type %in% c("House", "Townhouse", "Duplex") ~ "Full Home",
      TRUE ~ "Other"
    ),
    segment = as.factor(segment) # Convert to factor for regression
  ) %>%
  filter(!is.na(final_price)) %>%
  distinct(id, .keep_all = TRUE)

# ==========================================================
# PART 3: DATA JOINING (TimeSeries Prep)
# ==========================================================

# Aggregate Interest Rates to Monthly
monthly_interest <- interest_rates %>%
  mutate(month_yr = floor_date(date, "month")) %>%
  group_by(month_yr) %>%
  summarise(avg_policy_rate = mean(policy_rate, na.rm = TRUE))

# Prepare Scraped Snapshots
rental_snapshot <- rent_clean %>%
  mutate(month_yr = floor_date(date_scraped, "month")) %>%
  group_by(month_yr) %>%
  summarise(
    listing_count = n(),
    median_rent = median(final_price, na.rm = TRUE)
  )

# Create final analytical dataset for Macro Correlation
joined_data <- left_join(rental_snapshot, monthly_interest, by = "month_yr")

# ==========================================================
# PART 4: STATISTICAL MODELS (Skill #3)
# Requirement: 4 Regression Models
# ==========================================================

# MODEL 1: Simple Linear Regression (Primary Q1)
# Hypothesis: Higher interest rates impact total rental inventory.
model_inventory <- lm(listing_count ~ avg_policy_rate, data = joined_data)

# MODEL 2: Multiple Linear Regression (Primary Q2)
# Hypothesis: Beds, Square Footage, and Segment are significant price drivers.
model_price <- lm(final_price ~ final_beds + final_sqft + segment, data = rent_clean)

# MODEL 3: Interaction Model (Secondary Q1.1)
# Hypothesis: The effect of price varies based on property segment interaction.
model_interaction <- lm(final_price ~ final_beds * segment, data = rent_clean)

# MODEL 4: Logistic Regression (Primary Q3)
# Goal: Predicting likelihood of finding a move-in 'Deal/Incentive'
model_incentive <- glm(has_incentive ~ final_price + segment, 
                       data = rent_clean, family = "binomial")

# Print summaries for documentation
summary(model_inventory)
summary(model_price)
summary(model_incentive)

# ==========================================================
# PART 5: VISUALIZATIONS
# Requirement: 3+ Data Visualizations
# ==========================================================

# --- GRAPH 1: Price Distribution (EDA) ---
ggplot(rent_clean, aes(x = final_price, fill = segment)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = median(rent_clean$final_price), linetype = "dashed", color = "red") +
  labs(title = paste("Calgary Rental Market 2026 (Rate:", current_rate, "%)"),
       x = "Monthly Rent ($)", y = "Count") + theme_minimal()

# --- GRAPH 2: Bedroom Scarcity (Follow-up Q2) ---
ggplot(rent_clean, aes(x = factor(final_beds), y = final_price, color = segment)) +
  geom_jitter(alpha = 0.4, width = 0.2) + 
  geom_boxplot(alpha = 0.1, color = "black", outlier.shape = NA) +
  labs(title = "Rent Price vs. Bedrooms", y = "Monthly Rent ($)") + theme_minimal()

# --- GRAPH 3: Feature Correlation Matrix Heatmap ---
cor_data <- rent_clean %>% dplyr::select(final_price, final_beds, final_sqft) %>% na.omit() 
M <- cor(cor_data)
corrplot(M, method = "color", type = "upper", addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, title = "\nCorrelation of Rental Features", diag = FALSE)

# --- GRAPH 4: Price Range by Property Type (Violin Plot) ---
ggplot(rent_clean, aes(x = reorder(segment, final_price, median), y = final_price, fill = segment)) +
  geom_violin(alpha = 0.5) + labs(title = "Price Range by Property Type", x = "Segment", y = "Rent ($)") +
  theme_minimal()

# --- GRAPH 5: Coefficient Plot for Model 2 ---
tidy(model_price) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Impact of Features on Rent Price", x = "Variable", y = "Coefficient Estimate") + 
  theme_minimal()

# ==========================================================
# PART 6: ACTIONABLE RECOMMENDATION & INSIGHTS
# ==========================================================

# Prediction logic for a specific scenario
pred_data <- data.frame(final_price = 1800, segment = "Apartment/Condo")
pred_data$segment <- factor(pred_data$segment, levels = levels(rent_clean$segment))
prob <- predict(model_incentive, pred_data, type = "response")

cat("\n--- FINAL PROJECT INSIGHTS ---\n")
cat("1. Actionable Recommendation: A $1,800 Apartment has a", round(prob * 100, 1), 
    "% probability of having a move-in incentive.\n")
cat("2. Inventory Impact: For every 1% increase in BoC rates, the model predicts a change of", 
    round(coef(model_inventory)[2], 0), "listings.\n")
cat("3. Key Pricing Driver: The data suggests that", 
    tidy(model_price) %>% arrange(desc(abs(estimate))) %>% slice(1) %>% pull(term),
    "is the strongest predictor of rent price.\n")

# ==========================================================
# END OF SCRIPT
# ==========================================================