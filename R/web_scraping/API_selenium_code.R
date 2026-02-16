# Using API in R

library("jsonlite")
url <- paste0('https://download.open.fda.gov/food/',
              'enforcement/food-enforcement-0001-of-0001.json.zip')

temp <- tempfile()
download.file(url, temp)
food.recalls <- fromJSON(unz(temp, 
                             "food-enforcement-0001-of-0001.json"))
unlink(temp)

#View(food.recalls)

recalls.data <- food.recalls$results
View(recalls.data)

url <- paste0("https://api.fda.gov/drug/label.json")
browseURL(url)

url2 <- paste0("https://api.fda.gov/food/enforcement.json?",
               "search=report_date:[20240101+TO+20241231]&limit=1000")
browseURL(url2)

recalls.json <- fromJSON(url2)

EtherScanAPIToken <- "Your token"

# install.packages('keyring')
library(keyring)

keyring_create(keyring = "Etherscan_project1")

keyring_unlock(keyring = "Etherscan_project1")

key_set(service = "Etherscan_API", keyring = "Etherscan_project1")

library(httr); library(rvest); library(XML); library(jsonlite)

resEtherScan1 <- GET("https://api.etherscan.io/api",
                     query = list(module="account",
                                  action = "txlist",
                                  address = "0xaa7a9ca87d3694b5755f213b5d04094b8d0f0a6f",
                                  offset = 10000, sort="desc",
                                  apikey = key_get(service = "Etherscan_API",
                                                   keyring = "Etherscan_project1")))

dataEtherScan1 <- fromJSON(rawToChar(resEtherScan1$content), 
                           flatten=TRUE)$result
View(dataEtherScan1)

##
# Used paste0() function here since url is too long to fit on one line
url <- paste0('https://api.etherscan.io/api?',
              'module=account&action=txlist&',
              'address=0xaa7a9ca87d3694b5755f213b5d04094b8d0f0a6f&',
              'offset=10000&sort=desc&',
              'apikey=AC8THKTUG8SCUG9PDJZR84XX95JXGCVRAV')
resEtherScan2 <- fromJSON(url, flatten = TRUE)
dataEtherScan2 <- resEtherScan2$result
View(dataEtherScan2)

keyring_lock(keyring = "Etherscan_project1")

library(httr2) # If needed: install.packages('httr2')

keyring_unlock(keyring = "Etherscan_project1")

etherscan_base_url <- "https://api.etherscan.io/api"

req_etherscan <- request(etherscan_base_url)

##
params <- list(module="account", action = "txlist",
               address = "0xaa7a9ca87d3694b5755f213b5d04094b8d0f0a6f",
               offset = 10000, sort="desc",
               apikey = key_get(service = "Etherscan_project1",
                                keyring = "Ethereum_project1"))
##
req_body_etherscan <- req_etherscan %>%
  req_url_query(!!!params)

resp_body_etherscan <- req_body_etherscan %>%
  req_perform() %>%
  resp_body_json()

View(resp_body_etherscan)
##
transaction.data <- resp_body_etherscan$result
View(transaction.data)
transaction.data[[1]]
transaction.data[[2]]

##
dataEtherScan2 <- bind_rows(transaction.data)
View(dataEtherScan2)

keyring_lock(keyring = "Etherscan_project1")

cat(chatGPT.output)

if(!require("tidyverse")) {install.packages("tidyverse");
  require("tidyverse")}
if(!require("httr2")) {install.packages("httr2");
  require("httr2")}
if(!require("readr")) {install.packages("readr");
  require("readr")}
if(!require("jsonlite")) {install.packages("jsonlite");
  require("jsonlite")}
if(!require("keyring")) {install.packages("keyring");
  require("keyring")}

keyring_create(keyring = "OpenAI_project1")
keyring_unlock(keyring = "OpenAI_project1")

##
key_set(service = "OpenAI_API_Key", 
        keyring = "OpenAI_project1")

##
base_url_openAI <- "https://api.openai.com/v1"
endpoint_openAI <- "/chat/completions"

##
req_openAI <- request(base_url_openAI) %>% 
  req_url_path_append(endpoint_openAI)

##
headers_openAI <- req_openAI %>%
  req_auth_bearer_token(key_get(service = "OpenAI_API_Key", 
                                keyring = "OpenAI_project1"))

publications <- read_csv("Google_Scholar_publications_HSB_march27.csv")

##
EHS_list <- c('Academy of Management Journal',
              'Academy of Management Review',
              'The Accounting Review', 
              'Administrative Science Quarterly',
              'American Economic Review',
              'American Journal of Sociology',
              'American Sociological Review',
              'Annals of Statistics',
              'Annual Review of Sociology',
              'Econometrica',
              'Information Systems Research')

##
# Focus on EHS papers
EHS_publications <- publications %>%
  filter(`Journal/Publisher` %in% EHS_list) 

publication_list <- paste0("Our publications are as follows: ",
                           paste(EHS_publications$Paper, collapse = ", "))

prompt_text <- paste("TEXT GOES HERE", publication_list)

#
response <- headers_openAI %>%
  req_body_json(list(
    model = "gpt-4-turbo",
    messages = list(list(role = "user", 
                         content = prompt_text)))) %>%
  req_perform() %>%
  resp_body_json()


##
cat(chatGPT.output)

# We can even save it if we want to for later
save(chatGPT.output, file = 'chatGPT.output.RDA')

# reselenium
install.packages('RSelenium')
library(RSelenium)

# library(devtools)
# devtools::install_github("ropensci/RSelenium")

rD <- rsDriver(browser = "firefox") # Starts Selenium server/browser
# You can use Firefox, Chrome, Safari, etc. as your browser.

remDr <- rD[["client"]]

remDr$navigate("http://pc8.su.ucalgary.ca/search/")
remDr$screenshot(display = TRUE)

webElem1 <- remDr$findElement(using = "name", value = "title")

webElem1$sendKeysToElement(list('finance', key = 'enter'))

remDr$screenshot(display = TRUE)

remDr$getTitle()

remDr$getCurrentUrl()

webElem1 <- remDr$findElement(using = "name", value = "title")
webElem1$clearElement()
webElem1$sendKeysToElement(list('information systems', key = 'enter'))
remDr$screenshot(display = TRUE) 
remDr$getCurrentUrl()

remDr$getCurrentUrl()

webElems1 <- remDr$findElements(using = 'tag name', "span")

bookTitles <- unlist(lapply(webElems1, function(x){x$getElementText()}))
View(bookTitles)

bookTitles <- bookTitles[-NROW(bookTitles)]

n <- NROW(bookTitles)

bookTitles <- as.data.frame(bookTitles)
View(bookTitles)

webElems2 <- remDr$findElements(using = 'tag name', "i")
bookAuthors <- unlist(lapply(webElems2, function(x){x$getElementText()}))
bookAuthors <- as.data.frame(bookAuthors)

books <- cbind(bookTitles, bookAuthors)
View(books)

webElems3 <- remDr$findElements(using = 'tag name', "td")
allTableData <- unlist(lapply(webElems3, function(x){x$getElementText()}))
allTableData <- as.data.frame(allTableData)
View(allTableData)

allTableData2 <- tail(allTableData, -15)
View(allTableData2)

##
allTableData3 <- as.matrix(allTableData2, nrow = 4)
dim(allTableData3) <- c(4, n + 1) # Recall n <- NROW(bookTitles)

# This tells R to make the matrix 4 rows by n columns
View(allTableData3)

allTableData3 <- t(allTableData3)
View(allTableData3)

##
allTableData3 <- as.data.frame(allTableData3[, -1])
names(allTableData3) <- c('ISBN', 'Title', 'Author')
scrapedTable <- allTableData3[-1, ]
View(scrapedTable)

##
remDr$navigate("http://pc8.su.ucalgary.ca/search/")
remDr$screenshot(display = TRUE)

##
rD[["server"]]$stop()
system("taskkill /im java.exe /f") # Stops the java.exe

rm(list = ls()) # Removes all the variables from the environment

rD <- rsDriver(browser = "firefox")
remDr <- rD[["client"]]

##
library(rvest)
library(dplyr)
library(magrittr)
library(ggplot2)

# The link below  was broken into two lines to fit on the slide.
# You need to type in one line when copy/pasting it into R

url <- "https://www.imdb.com/best-of/highest-grossing-movies-2017/
ls027611946/" 

remDr$navigate(url)

webElem0 <- remDr$findElement(using = "class", 
                              value = "ipc-icon--clear")

webElem0$clickElement()

##
# Stop your Selenium session when done
rD[["server"]]$stop()
system("taskkill /im java.exe /f") # Stops the java.exe