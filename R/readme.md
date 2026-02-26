# Calgary Rental Market Analysis - DATA 613 Final Project

This project investigates the relationship between Bank of Canada (BoC) interest rates and the Calgary rental market using automated data collection, statistical modeling, and predictive analytics.

## 📁 Project Structure

```bash
.
├── README.md
├── data/
│   ├── raw/                # Uncleaned scrapes from RentFaster API
│   └── processed/          # Cleaned datasets used for modelling
├── R/
│   ├── web_scraping/
│   │   └── house_prices.R  # Automated API scripts for BoC and RentFaster
│   ├── visualization/
│   │   ├── correlation_heatmap.R # Matrix of feature correlations
│   │   └── more_plots.R          # Violin plots and distribution histograms
│   └── modelling/
│       ├── housing_code_graphs.R # Primary Regression modelling code
│       └── results/              # Statistical summary outputs
└── presentation/
    └── Final_Project_Slides.pdf  # 15-25 minute presentation file
```

# This has the combined code, with some of the model files but not all of them yet
```
https://github.com/gu12934/Data_613_Final_Project/blob/main/R/final/
```

# Question Mapping Confirmation
Primary Q1 (Interest vs Inventory): Analyzed via model_q1.

Secondary Q1.2 (Time Lag): Handled in the joined_data temporal join.

Primary Q2 (Pricing Model): Answered by model_q2 multiple regression.

Secondary Q2.1 (ANOVA): Verified via segment-based summaries and violin plots.

Primary Q3 (Incentive Decision): Answered by model_q3 logistic regression with a specific probability recommendation for renters.
