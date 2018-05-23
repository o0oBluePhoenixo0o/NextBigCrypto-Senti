
options("scipen"=100, "digits"=4)

# Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers")

# Clear environment
rm(list = ls())
# devtools::install_github("mkearney/rtweet")
# install packages if not available
packages <- c('jsonlite','plyr', 'dplyr', 'doSNOW','doParallel','lubridate','coinmarketcapr',
              'crypto') #starting from 30.01.2018

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

# library(devtools)
# install_github("jessevent/crypto")
# # Functions ---------------------------------------------------------------
# # Retrieve Coin Listings -----
# getCoins <- function() {
#   library(plyr)
#   today <- gsub("-", "", today())
#   json <- "https://files.coinmarketcap.com/generated/search/quick_search.json"
#   coins <- jsonlite::read_json(json, simplifyVector = TRUE)
#   coins <- data_frame(symbol = coins$symbol, name = coins$name, slug = coins$slug,
#                       rank = coins$rank)
#   length <- as.numeric(length(coins$slug))
#   range <- 1:length
#   url <- paste0("https://coinmarketcap.com/currencies/", coins$slug, "/historical-data/?start=20130428&end=",
#                 today)
#   baseurl <- c(url)
#   coins$slug <- as.character(baseurl)
#   coins$rank <- as.numeric(coins$rank)
#   return(coins)
# }
# # Scrape Historical Tables -----
# abstracts <- function(attributes) {
#   page <- read_html(attributes)
#   names <- page %>% html_nodes(css = ".col-sm-4 .text-large") %>% html_text(trim = TRUE) %>%
#     replace(!nzchar(.), NA)
#   nodes <- page %>% html_nodes(css = "table") %>% .[1] %>% html_table(fill = TRUE) %>%
#     replace(!nzchar(.), NA)
#   abstracts <- Reduce(rbind, nodes)
#   abstracts$symbol <- gsub("\\(||\\n|\\)|\\s\\s", "", names)
#   abstracts$symbol <- as.character(strsplit(abstracts$symbol, " ")[[1]][1])
#   return(abstracts)
# }
# # Cleanup results table -----
# cleanUp <- function(results) {
#   names(results) <- c("symbol", "date", "open", "high", "low", "close", "volume",
#                       "market", "name", "ranknow")
#   marketdata <- results
#   marketdata$volume <- gsub("\\,", "", marketdata$volume)
#   marketdata$market <- gsub("\\,", "", marketdata$market)
#   marketdata$volume <- gsub("\\-", "0", marketdata$volume)
#   marketdata$market <- gsub("\\-", "0", marketdata$market)
#   marketdata$close <- gsub("\\-", "0", marketdata$close)
#   marketdata$date <- format(strptime(marketdata$date, format = "%b %d,%Y"), "%Y-%m-%d")
#   marketdata$open <- as.numeric(marketdata$open)
#   marketdata$close <- as.numeric(marketdata$close)
#   marketdata$high <- as.numeric(marketdata$high)
#   marketdata$low <- as.numeric(marketdata$low)
#   marketdata$volume <- as.numeric(marketdata$volume)
#   marketdata$market <- as.numeric(marketdata$market)
#   # Percent variance between open and close rates
#   marketdata$variance <- ((marketdata$close - marketdata$open)/marketdata$close)
#   # spread variance between days high, low and closing
#   marketdata$volatility <- ((marketdata$high - marketdata$low)/marketdata$close)
#   return(marketdata)
# }
#

# # START CRYPTOCURRENCY SCRAPING SCRIPT ------------------------------------
# # Crypto Scraping Setup ---------------------------------------------------
# file <- paste0('Crypto-Markets_',Sys.Date(),'.csv')
# 
# # Get top 50 coins at Oct 7 only
# coins <- getCoins()
# coins50 <- read.csv("Top50_Oct7.csv")
# coins50 <- coins50[,-1]
# coins <- inner_join(coins,coins50)
# 
# length <- as.numeric(length(coins$slug))
# range <- 1:length
# cpucore <- as.numeric(detectCores(all.tests = FALSE, logical = TRUE))
# ptm <- proc.time()
# 
# # Uncomment for fiat exchange rate ----- 
# exchange_rate <- fromJSON('https://api.fixer.io/latest?base=USD')
# EUR <- exchange_rate$rates$EUR
# 
# # Parallel process scraping with progress bar -----------------------------
# cluster <- makeCluster(cpucore, type = "SOCK")
# registerDoSNOW(cluster)
# pb <- txtProgressBar(max = length, style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)
# attributes <- coins$slug
# 
# # Combine results and stop clusters ---------------------------------------
# results = foreach(i = range, .options.snow = opts, .combine = rbind, .packages = "rvest") %dopar%
#   abstracts(attributes[i])
# close(pb)
# stopCluster(cluster)
# 
# # Cleanup results and fix names -------------------------------------------
# coinnames <- data_frame(symbol = coins$symbol, name = coins$name, rank = coins$rank)
# results <- merge(results, coinnames)
# marketdata <- cleanUp(results)
# write.csv(marketdata, file, row.names = FALSE)
# print(proc.time() - ptm)

################################################################
# Update due to new available "crypto" package from 30.01.2018

# # Crypto Scraping Setup ---------------------------------------------------
file <- paste0('Crypto-Markets_',Sys.Date(),'.csv')

# Get top 50 coins at Oct 7 only
coins50 <- read.csv("Top50_Oct7.csv")
coins50 <- coins50[,-1]

results <- data.frame()
for (i in 1:nrow(coins50)){
    coin_df <- getCoins(coin = coins50[i,1])
    if (i == 1) {results <- coin_df
    }else{
      results <- bind_rows(results,coin_df)
    }
    print(paste0('Finish crawling historical data of coin No ',i,' ',coins50[i,1]))
}

results <- unique(results)

# need old dataset due to missing months (CMC only keeps 1 year data)
# merge both new + old dataset
# data <- read.csv('Crypto-Markets_2018-03-07.csv')
# data$slug <- as.character(data$slug)
# data$symbol <- as.character(data$symbol)
# data$name <- as.character(data$name)
# data$date <- as.Date(data$date)
# 
# finaldf <- bind_rows(results,data)      
# finaldf <- unique(finaldf)

# Save file to csv

write.csv(results, file, row.names = FALSE)
