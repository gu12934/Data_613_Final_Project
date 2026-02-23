# Housing R Web Scraping & Statistical Analysis
# Calgary Rental Market vs. BoC Interest Rates (2026)

# Clearing global directory
rm(list = ls())

# Load all required libraries at the start
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
# PART 1: DATA COLLECTION (Interest Rates)
# ==========================================================

boc_csv <- "https://www.bankofcanada.ca/valet/observations/V39079/csv"
raw_boc <- read_lines(boc_csv)
header_line <- grep("OBSERVATIONS", raw_boc) + 1

interest_rates <- read_csv(boc_csv, skip = header_line - 1) %>%
  clean_names() %>%
  mutate(date = as.Date(date)) %>%
  rename_with(~ "policy_rate", -date)

current_rate <- interest_rates %>% arrange(desc(date)) %>% slice(1) %>% pull(policy_rate)
print(paste("Current BoC Policy Rate:", current_rate, "%"))

# ==========================================================
# PART 2: DATA COLLECTION (RentFaster Scraper)
# ==========================================================

prices <- seq(500, 4000, 100) 

get_rent_data <- function(p_from) {
  p_to <- p_from + 99
  url <- paste0("https://www.rentfaster.ca/api/map.json?price_range_adv%5Bfrom%5D=", p_from, 
                "&price_range_adv%5Bto%5D=", p_to, 
                "&city=Calgary")
  Sys.sleep(1) # Reduced delay slightly for testing, keep at 2 if blocked
  res <- tryCatch(fromJSON(url)$listings, error = function(e) NULL)
  return(res)
}

all_listings_raw <- map(prices, get_rent_data) %>% list_rbind() 

# ==========================================================
# PART 3: DATA CLEANING & FEATURE ENGINEERING
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
    has_incentive = ifelse(grepl("free|incentive|discount|reduced|deal", listing_text, ignore.case = TRUE), 1, 0)
  ) %>%
  filter(!is.na(final_price)) %>%
  distinct(id, .keep_all = TRUE)

# DEFINE rent_analysis here so it's available for all following steps
rent_analysis <- rent_clean %>%
  mutate(
    segment = case_when(
      type == "Room For Rent" ~ "Shared",
      type %in% c("Condo", "Apartment") ~ "Apartment/Condo",
      type %in% c("House", "Townhouse", "Duplex") ~ "Full Home",
      TRUE ~ "Other"
    ),
    segment = as.factor(segment)
  )

# ==========================================================
# PART 4: DATA JOINING
# ==========================================================

monthly_interest <- interest_rates %>%
  mutate(month_yr = floor_date(date, "month")) %>%
  group_by(month_yr) %>%
  summarise(avg_policy_rate = mean(policy_rate, na.rm = TRUE))

rental_snapshot <- rent_analysis %>%
  mutate(month_yr = floor_date(date_scraped, "month")) %>%
  group_by(month_yr) %>%
  summarise(
    listing_count = n(),
    median_rent = median(final_price, na.rm = TRUE)
  )

joined_data <- left_join(rental_snapshot, monthly_interest, by = "month_yr")

# ==========================================================
# PART 5: VISUALIZATIONS & CORRELATION MATRIX
# ==========================================================

# 5.1 Correlation Matrix Heatmap
cor_data <- rent_analysis %>%
  dplyr::select(final_price, final_beds, final_sqft) %>%
  na.omit() 

M <- cor(cor_data)
corrplot(M, method = "color", type = "upper", addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, title = "Correlation Heatmap", diag = FALSE)

# 5.2 Inter-Correlation Pairs Plot (GGally)
ggpairs(rent_analysis, 
        columns = c("final_price", "final_beds", "final_sqft"),
        mapping = aes(color = segment, alpha = 0.5),
        title = "Calgary Market Inter-Correlations") + theme_minimal()

# 5.3 Macroeconomic Correlations
ggpairs(joined_data %>% dplyr::select(listing_count, median_rent, avg_policy_rate),
        title = "Macro Correlations: Rent vs. BoC Rates") + theme_gray()

# ==========================================================
# PART 6: STATISTICAL MODELS & RECOMMENDATION
# ==========================================================

model_inventory <- lm(listing_count ~ avg_policy_rate, data = joined_data)
model_price <- lm(final_price ~ final_beds + final_sqft + segment, data = rent_analysis)
model_incentive <- glm(has_incentive ~ final_price + segment, data = rent_analysis, family = "binomial")

# Actionable Recommendation Prediction
pred_data <- data.frame(final_price = 1800, segment = "Apartment/Condo")
pred_data$segment <- factor(pred_data$segment, levels = levels(rent_analysis$segment))
prob <- predict(model_incentive, pred_data, type = "response")

cat("\n--- FINAL PROJECT INSIGHTS ---\n")
cat("Probability of incentive for $1,800 unit:", round(prob * 100, 1), "%\n")
