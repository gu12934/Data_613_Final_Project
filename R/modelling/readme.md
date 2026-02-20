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



Call:
lm(formula = listing_count ~ avg_policy_rate, data = joined_data)

Residuals:
ALL 1 residuals are 0: no residual degrees of freedom!

Coefficients: (1 not defined because of singularities)
                Estimate Std. Error t value Pr(>|t|)
(Intercept)          235        NaN     NaN      NaN
avg_policy_rate       NA         NA      NA       NA

Residual standard error: NaN on 0 degrees of freedom

-----
Call:
lm(formula = final_price ~ final_beds + final_sqft + segment, 
    data = rent_analysis)

Residuals:
    Min      1Q  Median      3Q     Max 
-209.06  -47.02   14.56   52.31  133.76 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)   650.99876   70.68005   9.211 1.66e-14 ***
final_beds     -5.24731    4.69433  -1.118    0.267    
final_sqft      0.01416    0.01225   1.156    0.251    
segmentOther   48.78333   79.26108   0.615    0.540    
segmentShared  32.14795   71.92150   0.447    0.656    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 70.48 on 87 degrees of freedom
  (143 observations deleted due to missingness)
Multiple R-squared:  0.02953,	Adjusted R-squared:  -0.01509 
F-statistic: 0.6618 on 4 and 87 DF,  p-value: 0.6202
---------------------------------
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

---
--- ACTIONABLE RECOMMENDATION ---
> cat("Based on the logistic model, a $1,800 apartment has a", round(prob * 100, 1), "% chance of an incentive.\n")
Based on the logistic model, a $1,800 apartment has a NA % chance of an incentive.
