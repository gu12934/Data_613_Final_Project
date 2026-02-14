-------Code for web scraping-----
# pg.58 of web scraping II
browseURL('http://www.planecrashinfo.com/database.htm')

library(rvest) # Install this if you don't have it

scrape_2024Crashes_html <- read_html(
  "http://www.planecrashinfo.com/2024/2024.htm")

Table.2024Crashes <- html_nodes(scrape_2024Crashes_html,
                                css = "table")

Table.2024Crashes <- scrape_2024Crashes_html %>% 
  html_nodes(css = "table")

bolded.table.data <- scrape_2024Crashes_html %>% 
  html_nodes("table") %>% 
  html_nodes("td") %>% # Table data tag
  html_nodes("b") %>% # Tag for "bold"
  as.character()

tables.as.data.frames <- html_table(Table.2024Crashes)
str(tables.as.data.frames) # This is a list of data frames

scraped_2024Crashes <- as.data.frame(html_table(Table.2024Crashes))
View(scraped_2024Crashes)

library(XML); library(httr)

mainPageURL <- "http://www.planecrashinfo.com/database.htm"

# The first is the simplest approach, but it may not work
links <- try(getHTMLLinks(mainPageURL)) 
links.2 <- try(getHTMLLinks(htmlParse(GET(mainPageURL))))
links.3 <- try(getHTMLLinks(as.character(read_html(mainPageURL))))

links <- getHTMLLinks(htmlParse(GET(mainPageURL)))
grepl(pattern = '1980', x = links)
