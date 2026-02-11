# Data_613_Final_Project

Project Proposal

Project Title
How Do Housing Prices in Alberta Respond to Interest Rate Changes?

Project Overview
This project investigates the relationship between Bank of Canada interest rate changes and housing prices across Alberta. With recent rate hikes and ongoing affordability concerns, understanding how monetary policy affects regional housing markets is both timely and relevant. Our goal is to quantify the strength, direction, and timing of this relationship using publicly available economic and real‑estate data.

Motivation
Alberta’s housing market has behaved differently from other provinces due to population inflows, economic cycles, and affordability.
Interest rates are one of the most influential macroeconomic levers affecting mortgages and housing demand.
Understanding this relationship can help policymakers, buyers, and investors anticipate market shifts.

Research Questions
What is the historical relationship between Bank of Canada interest rates and average housing prices in Alberta
Do housing prices respond immediately to rate changes, or is there a lag
Are major Alberta cities (Calgary, Edmonton) more sensitive to rate changes than the provincial average
How does the effect of interest rates compare to other factors such as population growth or inventory levels
Expected Deliverables
Cleaned dataset combining interest rates, housing prices, and optional control variables
Visualizations showing trends, correlations, and lagged effects
Regression or time‑series model estimating the impact of rate changes

A final presentation summarizing insights and recommendations

2. Data Plan
Data Sources (Web‑Scrapable + Downloadable)
Interest Rates (Bank of Canada)
Bank of Canada – Policy Interest Rate History
 https://www.bankofcanada.ca/rates/interest-rates/canadian-interest-rates/ (bankofcanada.ca in Bing)
 Scrapable table with dates + rate values.
Housing Prices
You have several strong options:
1. Calgary Real Estate Board (CREB)
Monthly benchmark prices
https://www.creb.com/News_and_Events/Statistics/ (creb.com in Bing)
 Often includes downloadable CSVs or HTML tables you can scrape.
2. Alberta Real Estate Association (AREA)
Provincial and regional housing statistics
https://www.albertarealtor.ca/market-stats/ (albertarealtor.ca in Bing)
3. Statistics Canada
New Housing Price Index
Housing resale data
https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1810020501 (www150.statcan.gc.ca in Bing)
 StatCan tables are very easy to scrape using pandas.
Optional Control Variables
Population growth (StatCan)
Unemployment rate (StatCan)
Housing inventory / sales volume (CREB or AREA)

Variables to Collect
Category
Variable
Description
Interest Rates
Policy Rate
Bank of Canada overnight rate
Housing Prices
Benchmark Price
Calgary, Edmonton, Alberta overall
Housing Prices
Average/Median Sale Price
If available
Time
Year, Month
For merging datasets
Optional Controls
Population, Unemployment, Inventory
Helps isolate interest‑rate effects


Data Cleaning Steps
Scrape tables from BoC, CREB, AREA, or StatCan using Python (requests, BeautifulSoup, or pandas.read_html).
Convert dates to a consistent monthly format (e.g., YYYY‑MM).
Handle missing values (interpolate or drop).
Merge datasets on the date column.
Create lagged variables (e.g., interest rate lagged by 1–6 months).
Normalize or scale variables if needed for modeling.
Check for stationarity if using time‑series models.

3. Modeling Plan
You can choose one or combine multiple approaches.
A. Correlation + Exploratory Analysis
Plot interest rates vs. housing prices
Compute Pearson correlation
Examine lagged correlations (1–12 months)
B. Multiple Linear Regression
Model example:
[ \text{Price}_t = \beta_0 + \beta_1 \text{InterestRate}_t + \beta_2 \text{Population}_t + \beta_3 \text{Unemployment}_t + \epsilon_t ]
Or include lags:
[ \text{Price}t = \beta_0 + \beta_1 \text{InterestRate}{t-3} + \epsilon_t ]
C. Time‑Series Models
1. ARIMAX (ARIMA with exogenous variables)
Housing prices as the dependent series
Interest rates as the exogenous predictor
Good for monthly data
2. Vector Autoregression (VAR)
Treat interest rates and housing prices as jointly evolving
Useful if you want to test causality (Granger causality)
3. Distributed Lag Models
Estimate how long it takes for rate changes to affect prices
Great for policy‑impact analysis
