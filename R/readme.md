
# Calgary Rental Market Analysis - DATA 613 Final Project

## Project Structure

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

    Question Mapping Confirmation
Primary Q1 (Interest vs Inventory): Analyzed via model_q1.

Secondary Q1.2 (Time Lag): Handled in the joined_data temporal join.

Primary Q2 (Pricing Model): Answered by model_q2 multiple regression.

Secondary Q2.1 (ANOVA): Verified via segment-based summaries and violin plots.

Primary Q3 (Incentive Decision): Answered by model_q3 logistic regression with a specific probability recommendation for renters.
