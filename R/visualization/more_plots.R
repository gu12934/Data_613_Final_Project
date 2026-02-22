# Install if you haven't already
# install.packages("GGally")
library(GGally)

#--------------------------
# Filter for numeric columns and remove NAs for the correlation calculation
plot_data_features <- rent_analysis %>%
  select(final_price, final_beds, final_sqft, segment) %>%
  filter(!is.na(final_price), !is.na(final_beds), !is.na(final_sqft))

# Create the multi-panel plot
ggpairs(plot_data_features, 
        columns = 1:3, # numeric columns
        mapping = aes(color = segment, alpha = 0.5),
        title = "Correlation Matrix: Calgary Rental Features") +
  theme_minimal()
#-----------------------------
# Selecting the aggregated monthly data
plot_data_macro <- joined_data %>%
  select(listing_count, median_rent, avg_policy_rate)

# Generate the matrix
ggpairs(plot_data_macro,
        title = "Macroeconomic Correlations: Rent vs. BoC Rates") +
  theme_gray()
#--------------------------
# install.packages("corrplot")
library(corrplot)

# 1. Prepare the numeric data
cor_data <- rent_clean %>%
  select(final_price, final_beds, final_sqft) %>%
  drop_na() # Correlation requires no missing values

# 2. Calculate the correlation matrix
M <- cor(cor_data)

# 3. Create the Heatmap
corrplot(M, method = "color", 
         type = "upper", 
         addCoef.col = "black", # Show the actual R-values
         tl.col = "black", tl.srt = 45, 
         title = "\nCorrelation of Calgary Rental Features",
         diag = FALSE)
#------------------------------------
# install.packages("GGally")
library(GGally)

# We use the 'rent_analysis' data to color-code by property segment
ggpairs(rent_analysis, 
        columns = c("final_price", "final_beds", "final_sqft"),
        mapping = aes(color = segment, alpha = 0.5),
        title = "Calgary Market Feature Inter-Correlations") +
  theme_minimal()
#-------------------------------------
