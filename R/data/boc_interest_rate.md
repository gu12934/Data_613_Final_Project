# Interest Rates (Bank of Canada)
# Bank of Canada – Policy Interest Rate History
# https://www.bankofcanada.ca/rates/interest-rates/canadian-interest-rates/
# Scrapable table with dates + rate values.


# clearing global directory
rm(list = ls())
#----------------------------------------------------------
# Install missing packages if necessary
# install.packages(c("tidyverse", "rvest", "httr", "janitor"))
library(httr)
library(rvest)
library(stringr)
library(ggplot2)
library(janitor) # Great for cleaning column names
library(dplyr)
library(readr

# URL for Policy Interest Rate
boc_url <- "https://www.bankofcanada.ca/rates/interest-rates/canadian-interest-rates/"

# Read the page
boc_page <- read_html(boc_url)

# Extract the table
# Note: CSS selectors like 'table' or specific IDs are used
interest_rates <- boc_page %>%
  html_element("table") %>% 
  html_table() %>%
  clean_names() # Clean column names for easier handling

# check names, i got the first table out
names(interest_rates)

# Extract ALL tables
tables <- boc_page %>% html_elements("table")

length(tables)   # See how many tables exist

#printing each table to find the one i need
for (i in seq_along(tables)) {
  cat("\n\n=== TABLE", i, "===\n\n")
  print(html_table(tables[[i]]))
}
#--------------------------------

#---test 1----
# URL
boc_csv <- "https://www.bankofcanada.ca/valet/observations/V39079/csv"

# Step 1: Read raw text
raw <- read_lines(boc_csv)

# Step 2: Find the line number where OBSERVATIONS appears
obs_line <- grep("OBSERVATIONS", raw)

# Step 3: The actual header is the next line
header_line <- obs_line + 1

# Step 4: Read the CSV starting from the header line
interest_rates <- read_csv(boc_csv, skip = header_line - 1)

# Step 5: Clean names
interest_rates <- clean_names(interest_rates)

# Step 6: Convert date column
interest_rates$date <- as.Date(interest_rates$date)

# Preview
head(interest_rates)

# writing the file to local_drive
#write_csv(interest_rates, "C:/Users/gurmo/Downloads/interest_rates.csv")
