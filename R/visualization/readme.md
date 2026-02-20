
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
