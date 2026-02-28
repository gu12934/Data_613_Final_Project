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
library(car) # For Multicollinearity (VIF)

# ==========================================================
# PART 1: DATA COLLECTION (Automated/Scraping)
# ==========================================================

# 1.1 Bank of Canada API for Interest Rates
boc_csv <- "https://www.bankofcanada.ca/valet/observations/V39079/csv"
raw_boc <- read_lines(boc_csv)
header_line <- grep("OBSERVATIONS", raw_boc) + 1
interest_rates <- read_csv(boc_csv, skip = header_line - 1) %>%
  clean_names() %>%
  mutate(date = as.Date(date)) %>%
  rename_with(~ "policy_rate", -date)

current_rate <- interest_rates %>% arrange(desc(date)) %>% slice(1) %>% pull(policy_rate)

# 1.2 RentFaster API Scraper for Calgary Listings
prices <- seq(500, 4000, 100) 
get_rent_data <- function(p_from) {
  p_to <- p_from + 99
  url <- paste0("https://www.rentfaster.ca/api/map.json?price_range_adv%5Bfrom%5D=", p_from, 
                "&price_range_adv%5Bto%5D=", p_to, "&city=Calgary")
  Sys.sleep(1.5) 
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
    listing_text = coalesce(!!!dplyr::select(., any_of(c("intro", "title", "description")))),
    has_incentive = ifelse(grepl("free|incentive|discount|reduced|deal", listing_text, ignore.case = TRUE), 1, 0),
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
# PART 3: DATA JOINING (TimeSeries Prep)
# ==========================================================

monthly_interest <- interest_rates %>%
  mutate(month_yr = floor_date(date, "month")) %>%
  group_by(month_yr) %>%
  summarise(avg_policy_rate = mean(policy_rate, na.rm = TRUE))

rental_snapshot <- rent_clean %>%
  mutate(month_yr = floor_date(date_scraped, "month")) %>%
  group_by(month_yr) %>%
  summarise(listing_count = n(), median_rent = median(final_price, na.rm = TRUE))

joined_data <- left_join(rental_snapshot, monthly_interest, by = "month_yr")

# ==========================================================
# PART 4: STATISTICAL MODELS & DIAGNOSTICS (Prof Feedback)
# ==========================================================

# --- Q1 FIX: Address Overfitting / Insufficient Observations ---
if (nrow(joined_data) > 2) {
  model_inventory <- lm(listing_count ~ avg_policy_rate, data = joined_data)
  print(summary(model_inventory))
} else {
  cat("\n--- NOTE FOR Q1 (OVERFITTING) ---\n")
  cat("Only", nrow(joined_data), "time-series point(s) available. Regression is inappropriate.\n")
  cat("Treating results as a Descriptive Snapshot rather than a predictive model.\n")
}

# --- Q2: Multiple Regression (The Pricing Model) ---
model_price <- lm(final_price ~ final_beds + final_sqft + segment, data = rent_clean)
print(summary(model_price))

# --- DIAGNOSTIC 1: Normality Assumption (QQ Plot) ---
# Check if residuals are normally distributed
plot(model_price, which = 2, main = "Q-Q Plot: Normality of Residuals")



# --- DIAGNOSTIC 2: Multicollinearity (VIF) ---
# Target VIF < 5 to ensure predictors aren't double-counting
vif_values <- vif(model_price)
print("Multicollinearity Diagnostics (VIF):")
print(vif_values)



# --- MODEL 3: Logistic Regression (Incentives) ---
model_incentive <- glm(has_incentive ~ final_price + segment, data = rent_clean, family = "binomial")

# ==========================================================
# PART 5: ADVANCED CORRELATION VISUALIZATIONS
# ==========================================================

# Pre-filter for GGally to prevent 'not enough finite observations' error
plot_data_features <- rent_clean %>%
  dplyr::select(final_price, final_beds, final_sqft, segment) %>%
  filter(!is.na(final_price), !is.na(final_beds), !is.na(final_sqft)) %>%
  group_by(segment) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  mutate(segment = factor(segment))

# 5.1 Multi-Panel Feature Analysis
if(nrow(plot_data_features) > 0) {
  p_market <- ggpairs(plot_data_features, 
                      columns = 1:3, 
                      mapping = aes(color = segment, alpha = 0.5),
                      title = "Correlation Matrix: Calgary Rental Features") + 
    theme_minimal()
  print(p_market)
}



# 5.2 Macroeconomic Correlations (Safe Check)
if(nrow(joined_data) >= 2) {
  p_macro <- ggpairs(joined_data %>% dplyr::select(listing_count, median_rent, avg_policy_rate),
                     title = "Macroeconomic Correlations: Rent vs. BoC Rates") +
    theme_gray()
  print(p_macro)
}

# 5.3 Correlation Heatmap (corrplot)
cor_data <- rent_clean %>%
  dplyr::select(final_price, final_beds, final_sqft) %>%
  drop_na() 

M <- cor(cor_data)
corrplot(M, method = "color", type = "upper", addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, title = "\nHeatmap: Calgary Rental Features", diag = FALSE)



# ==========================================================
# PART 6: STANDARD PLOTS & EDA
# ==========================================================

# Price Distribution
ggplot(rent_clean, aes(x = final_price, fill = segment)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Calgary Rent Distribution", x = "Monthly Rent ($)") + theme_minimal()

# Q1 Plot: Address Slide 12 Feedback (Single Point Issue)
p1 <- ggplot(joined_data, aes(x = avg_policy_rate, y = listing_count)) +
  geom_point(size = 4, color = "darkblue") +
  labs(title = "Inventory vs. Interest Rate Snapshot")

if(nrow(joined_data) > 2) {
  p1 <- p1 + geom_smooth(method = "lm", color = "red")
} else {
  p1 <- p1 + annotate("text", x = mean(joined_data$avg_policy_rate), 
                      y = min(joined_data$listing_count), 
                      label = "Trendline requires more time-series observations")
}
print(p1)

# ==========================================================
# PART 7: ACTIONABLE RECOMMENDATION
# ==========================================================

pred_data <- data.frame(final_price = 1800, segment = "Apartment/Condo")
pred_data$segment <- factor(pred_data$segment, levels = levels(rent_clean$segment))
prob <- predict(model_incentive, pred_data, type = "response")

cat("\n--- FINAL PROJECT OUTPUTS ---\n")
cat("1. Incentive Probability ($1800 Apt):", round(prob * 100, 1), "%\n")
cat("2. Note on Prof Feedback: Added VIF checks and QQ Plots to validate Pricing Model.\n")
cat("3. Overfitting Fix: Q1 Regression deactivated until >2 time periods are present.\n")
