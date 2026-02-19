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
