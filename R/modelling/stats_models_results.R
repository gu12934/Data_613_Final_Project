> # ==========================================================
> # PART 6: STATISTICAL MODELS (Skill #3)
> # ==========================================================
> 
> # MODEL 1: Simple Linear Regression (Primary Q1)
> # Hypothesis: Higher rates lead to higher rental inventory.
> model_inventory <- lm(listing_count ~ avg_policy_rate, data = joined_data)
> summary(model_inventory)

Call:
lm(formula = listing_count ~ avg_policy_rate, data = joined_data)

Residuals:
ALL 1 residuals are 0: no residual degrees of freedom!

Coefficients: (1 not defined because of singularities)
                Estimate Std. Error t value Pr(>|t|)
(Intercept)         5993        NaN     NaN      NaN
avg_policy_rate       NA         NA      NA       NA

Residual standard error: NaN on 0 degrees of freedom

> 
> # MODEL 2: Multiple Linear Regression (Primary Q2)
> # Hypothesis: Beds, Square Footage, and Segment drive the final price.
> model_price <- lm(final_price ~ final_beds + final_sqft + segment, data = rent_clean)
> summary(model_price)

Call:
lm(formula = final_price ~ final_beds + final_sqft + segment, 
    data = rent_clean)

Residuals:
     Min       1Q   Median       3Q      Max 
-2189.65  -271.66   -51.45   202.62  2315.10 

Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)       1.133e+03  2.245e+01  50.448  < 2e-16 ***
final_beds        1.469e+02  1.006e+01  14.608  < 2e-16 ***
final_sqft        3.648e-01  2.066e-02  17.656  < 2e-16 ***
segmentFull Home  1.979e+02  2.664e+01   7.429 1.35e-13 ***
segmentOther     -2.010e+01  2.079e+01  -0.967    0.334    
segmentShared    -1.056e+03  3.795e+01 -27.819  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 405.3 on 3744 degrees of freedom
  (2243 observations deleted due to missingness)
Multiple R-squared:  0.5003,	Adjusted R-squared:  0.4996 
F-statistic: 749.6 on 5 and 3744 DF,  p-value: < 2.2e-16
