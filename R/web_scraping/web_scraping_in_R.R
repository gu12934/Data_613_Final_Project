# feb 15 2026

# intro to web scraping in R
# https://d2l.ucalgary.ca/d2l/le/content/719752/viewContent/7434177/View
library(httr)
library(XML)
library(stringr)
library(ggplot2)

## pg.12 of supplementary materials

url <- 'https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger'
# browseURL(url) # View the webpage

get_wiki_HTML_file <- GET(url)

heritage_parsed <- htmlParse(get_wiki_HTML_file) 

page.tables <- readHTMLTable(heritage_parsed, stringsAsFactors = FALSE)

View(page.tables) 
# the first table is the one we want, but it has some formatting issues. We can clean it up with some string manipulation functions.
View(page.tables[[1]])
View(page.tables[[2]])
View(page.tables[[3]])

endangered_df <- page.tables[[1]] # This is a data frame.
# The first row of the data frame contains the column names, so we can set those as the column names of the data frame and then remove that first row from the data frame.
colnames(endangered_df) <- endangered_df[1, ]
endangered_df <- endangered_df[-1, ] # Take out the first row
View(endangered_df)

#
endangered_df <- endangered_df[ , 
                                c("Name", "Location" , 
                                  "Criteria",
                                  "Year (WHS)", 
                                  "Endangered")]


colnames(endangered_df) <- c("name", "location", "criteria",
                             "yearInscribed", "yearEndangered") 

View(endangered_df)

## 
endangered_df$criteria <- ifelse(grepl(pattern = "Natural", 
                                       x = endangered_df$criteria) == TRUE,
                                 "natural", "cultural")

View(endangered_df)

## conver to numeric data type
endangered_df$yearInscribed <- as.numeric(endangered_df$yearInscribed)
View(endangered_df) 
str(endangered_df) # yearInscribed is now a numeric column

# get the substring
str_sub(string = endangered_df$yearEndangered, start = 1, end = 4)

# vector to describe year endangered column
endangered_df$yearEndangered <- 
  as.numeric(str_sub(endangered_df$yearEndangered, start = 1, end = 4))

# plot distribution of years
attach(endangered_df)

p1 <- qplot(yearEndangered, binwidth = 5) +
  xlab('Year') + ylab('Frequency')

p1

# intro to data anlaytics 2
# https://d2l.ucalgary.ca/d2l/le/content/719752/viewContent/7434176/View
# plane crash info
browseURL('http://www.planecrashinfo.com/database.htm')


install.packages("xml2")
install.packages("rvest")

# installing libraries
library(rvest) # Install this if you don't have it
library(xml2)

scrape_2020Crashes_html <- read_html(
  "http://www.planecrashinfo.com/2020/2020.htm")

#making html nodes
Table.2020Crashes <- html_nodes(scrape_2020Crashes_html,
                                css = "table")

Table.2020Crashes <- scrape_2020Crashes_html %>% 
  html_nodes(css = "table")

# converting to data frame
bolded.table.data <- scrape_2020Crashes_html %>% 
  html_nodes("table") %>% 
  html_nodes("td") %>% # Table data tag
  html_nodes("b") %>% # Tag for "bold"
  as.character()

#View(bolded.table.data)

tables.as.data.frames <- html_table(Table.2020Crashes)
str(tables.as.data.frames) # This is a list of data frames

scraped_2020Crashes <- as.data.frame(html_table(Table.2020Crashes))
View(scraped_2020Crashes)

# using header=true
scraped_2020Crashes <- as.data.frame(html_table(Table.2020Crashes,
                                                header = TRUE))
View(scraped_2020Crashes)

# get html links-------------
# NOTE: this did not work
library(XML)
library(httr)

mainPageURL <- "http://www.planecrashinfo.com/database.htm"

links <- getHTMLLinks(mainPageURL) # simplest approach
links.2 <- getHTMLLinks(htmlParse(GET(mainPageURL)))
links.3 <- getHTMLLinks(as.character(read_html(mainPageURL)))

# new code to try out
library(httr)
library(rvest)

mainPageURL <- "http://www.planecrashinfo.com/database.htm"

page <- GET(
  mainPageURL,
  add_headers(
    "User-Agent" = "Mozilla/5.0"
  )
)

html <- read_html(content(page, "text"))

links <- html %>% html_nodes("a") %>% html_attr("href")
links
#grepl for certai year

grepl(pattern = '1980', x = links)
grepl(pattern = '1980', x = links) # L at the end for logical

grepl(pattern = '1980|1981', x = links)
grepl(pattern = '1980|1981|1982|1983', x = links)

# library_lubridate for current year
library(lubridate)
thisYear <- year(today()) # What year is it?

# year 
paste(1980:thisYear, collapse = "|")

#
paste(1980:2020, 'testing', collapse = ' ')
paste(1980:2020, 'testing', sep = ' ')

# paste the output into airplane crashes
grepl(paste(1980:thisYear, collapse = "|"), links)

# select the links that are true
links.recentCrashes <- links[grepl(paste(1980:thisYear, 
                                         collapse = "|"), links)]
links.recentCrashes

# append to main url
mainURL <- "http://www.planecrashinfo.com"
crash.links <- paste0(mainURL, links.recentCrashes)
crash.links

# for loop to append to each url
for(i in 1: length(crash.links)){
  url <- crash.links[i] 
  
  scraped_html <- read_html(url)
  Table.Crashes <- html_nodes(scraped_html, "table")
  
  scraped.crash.data <- as.data.frame(html_table(Table.Crashes, 
                                                 header = TRUE))
  
  assign(paste('crash_', i + 1979, sep = ''), scraped.crash.data)
}

# split and data wrangling

Fatalities <- crash_1980$Fatalities;

# Split string at anything not a number or a question mark
s <- strsplit(Fatalities, "[^[:digit:]?]") 
s

# unlist s to get a numeric vector
fatalitiesVector <- as.numeric(unlist(s))
fatalitiesVector
#
crash_1980$Deaths.Onboard <- fatalitiesVector[seq(1, 
                                                  length(fatalitiesVector), 3)]
crash_1980$Deaths.Ground <- fatalitiesVector[seq(3, 
                                                 length(fatalitiesVector), 3)]

View(crash_1980) # See how it compares to the Fatalities column

#
for(i in 2: length(crash.links)){ # Start from 2 since we already did 1980
  
  # Create a Fatalities vector from the ith data frame in our list
  eval(parse(text = paste0('Fatalities <- crash_', 
                           i + 1979,'$Fatalities')))
  
  s <- strsplit(Fatalities, "[^[:digit:]?]") # Split it into components
  fatalitiesVector <- as.numeric(unlist(s))
  
  eval(parse(text = paste0('crash_', i + 1979,'$Deaths.Onboard',
                           ' <- fatalitiesVector[seq(1, length(fatalitiesVector), 3)]')))
  
  eval(parse(text = paste0('crash_', i + 1979,'$Deaths.Ground',
                           ' <- fatalitiesVector[seq(3, length(fatalitiesVector), 3)]')))
}

# combined data set with r bind function
rbind(crash_1980, crash_1981) # Binds rows to a data frame 

combinedCrashData <- crash_1980

for(i in 2:length(crash.links)){ # Start from 2 since we already did 1980
  eval(parse(text = paste0('combinedCrashData <- rbind(combinedCrashData, 
                    crash_', i + 1979,')')))
}

#
sum(na.omit(combinedCrashData$Deaths.Onboard)) # Omit missing rows
sum(na.omit(combinedCrashData$Deaths.Ground))

#iframes tag
# This URL is split into several lines to make it fit on the slide
url <- paste0('https://irma.nps.gov/Stats/SSRSReports/',
              'Park%20Specific%20Reports/Annual%20Park%20Recreation%',
              '20Visitation%20(1904%20-%20Last%20Calendar%20Year)?Park=BLRI')

browseURL(url)

## doesnt wrk
library(rvest)
scraped_data <- read_html(url)
Table.Reports <- html_nodes(scraped_data, xpath = "//table")
Table.Reports2 <- html_nodes(scraped_data, css = "table")

#work around
link1 <- paste0('https://github.com/yusuzech/r-web-scraping-cheat-sheet/',
                'blob/master/example_script/iframe_tutorial.md')
browseURL(link1)

link2 <- paste0('https://stackoverflow.com/questions/44377443/',
                'scraping-table-within-iframe-using-r-rvest-library')
browseURL(link2)

## iframe src incomplete
iframe_src <-  session(url) %>%
  html_element("iframe") %>%
  html_attr("src")

## append src
main_url <- 'https://irma.nps.gov'
iframe_url <- paste0(main_url, iframe_src)

## get all tables
all.tables <- html_session(iframe_url) %>%
  html_elements("table") %>%
  html_table()

## rec visits
recVisits <- lapply(all.tables, function(x) grep("Recreation Visits", x))

#s.apply
table.index.to.get <- which(sapply(recVisits, function(x) length(x) > 0)*
                              sapply(all.tables, function(x) length(as.data.frame(x)) == 2) > 0)

## making a dataframe
library(magrittr)

table.df <- html_session(iframe_url) %>%
  html_elements("table") %>%
  extract(table.index.to.get) %>%
  html_table(header = TRUE) %>%
  as.data.frame()

## data wrangling
names(table.df) <- table.df[1, ]
table.df <- table.df[-1, ]
table.df <- table.df[-NROW(table.df), ]

assign(paste0('BLRI', '.df'), table.df)
View(table.df)