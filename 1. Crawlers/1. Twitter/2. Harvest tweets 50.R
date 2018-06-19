# Objectives
# 1. Obtain list of coins + tickers
# 2. Crawl tweets related (2 weeks backward)
# 3. Store in separate folder for analysis

options("scipen"=100, "digits"=4)
gc()
# Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers")

# Clear environment
rm(list = ls())

# devtools::install_github("mkearney/rtweet")

# install packages if not available
packages <- c('rtweet','twitteR', #Twitter API crawlers
              'data.table','dplyr','scales','ggplot2',
              'httr','stringr','rvest','curl','lubridate','coinmarketcapr',
              'gtools','readr','botrnot')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

# devtools::install_github("mkearney/botrnot")
# 1. Get coin list --------------------------------------------
# Obtain list of coins --> extract tickers + coin names 
# devtools::install_github("amrrs/coinmarketcapr")

# latest_marketcap <- get_marketcap_ticker_all('EUR')
# 
# coins_list <- latest_marketcap[,which(names(latest_marketcap) %in% c("name","symbol"))]
# coins_list$ticker <- paste0('$',coins_list$symbol)s
# 
# # Take only top 50 (Oct 7)
# coins_list <- coins_list[1:50,]
# write.csv(coins_list,"Top50_Oct7.csv")

coins_list <- read.csv("~/GitHub/NextBigCrypto-Senti/1. Crawlers/Top50_Oct7.csv")

#---------------------------------------------------------------
# Function to swap Twitter Auth between 4 keys when entry rate runs out
# swap_auth <- function(x){
#   if (x == 1){
#     # 1
#     consumer_key <- 'cdi1LlwgzdXzR4Wxz8T3Gude6'
#     consumer_secret <- 's3hpLYXs9ULY1YwzyTRP8aRovp3rvkjUM9ue9usi8MotrvUgOG'
#     access_token <- '240771509-MQiqGMegj3B4ohmRSi7mThfprMg7j9lAYkDB3s9W'
#     access_token_secret <- 'YZGMyJ6Jz3Ncx1SG59QXJGaRrEkYjvCTC71KPsFH2eaIi'
#     setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)
#   }
#   if (x == 2){
#     # 2
#     consumer_key <- 'yO6HMXQfaAazZfdyOQpPicX0M'
#     consumer_secret <- 'wT1lq9bd7WWJjoVw3aHKfdHbpdjxd8r8RKc56fGiQPGRaJgILP'
#     access_token <- '379008223-8gPeX8OJ5wxjILXYUMxKwTSOH30UJbYdUWNqCE53'
#     access_token_secret <- 'P3anD6dTrrQb6RUP4Me6HAMpgY8RU9QuORCrGI14f1Wis'
#     setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)
#   }
#   # if (x == 3){
#   #   # 3
#   #   consumer_key="hNgXlxwQYcL71SxCddwvpTEVf"
#   #   consumer_secret="ZSXtL7Yq5QwkAvyCnm9hACaC6CosyHUOOnewv2ufL6IG8tQBCU"
#   #   access_token="838380485843763200-pAQXVTl89Dn1Pz2GnQzOacBmJnXPZz6"
#   #   access_token_secret="MtqyBbhUxM0zOTJIuRXUWtZMRmVnnjfFT0rs5X4odItdq"
#   #   setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)
#   # }
#   if (x == 4){
#     # 4
#     consumer_key <- "zow0fQ6Lv0j79gx4lDhBKVUDu"
#     consumer_secret = "fp33fr0VBkIIoPzpwgbCPemkZJ1E718TFqb8b86DKd0nVgGFEs"
#     access_token = "836598863582617600-Tjmc0MqCtcOZVjx9dto5wSBkdRgxDmh"
#     access_token_secret = "57THIHAlttLUf3y8x1P5U2JnQmcDIfDqq8xmZrwgD5Qo6"
#     setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)
#   }
# }


# Function to crawl tweets for top 50 tickers
get_tweets <- function(i,ticker,n,TW_key){
  path = '1b. Report/'
  path_weekly = '1b. Report/Weekly/'
  
  # Rotate TW_key for each crypto ticker
  # swap_auth(TW_key) # deact because new API templates
  # # Read the old data file
  # old_df_path <- paste0(path,i,'_',ticker,'_','FULL','.csv')
  # old_df <- read_csv(old_df_path)
  
  ####################################################
  # Update Jan 21st 2018 to read and combine big files when RAM is not enough
  
  setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/Weekly")
  files <- list.files(pattern = paste0('^',i,'_'))
  max_count <- length(files)
  gc() # garbage collection
  old_df <- fread(dir(pattern=paste0('^',i,'_'))[1])

  for (k in max_count:2){
    a <- fread(dir(pattern=paste0('^',i,'_'))[k])
    old_df <- bind_rows(old_df,a)
    print(paste0('Reading file number: ',k))
  }
  
  # Reset working directory
  setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers")
  ####################################################
  # Delete unrealated columns (error when saving files)
  old_df <- old_df[,-1]
  
  # convert to date-time
  old_df$created_at <- ymd_hms(old_df$created_at)
  
  since <- max(old_df$status_id)
  
  df <- search_tweets(ticker,
                      n,
                      type = 'recent',
                      lang = 'en',
                      since_id = since,
                      include_rts = FALSE,
                      retryonratelimit = TRUE)
  
  # Keep only columns that fit with old.df
  old_col <- colnames(old_df)
  
  # Due to Twitter API update Nov 2017 --> unlist columns
  df$hashtags <- sapply(df$hashtags, paste, collapse = " ")
  df$symbols <- sapply(df$symbols, paste, collapse = " ")
  df$urls_url <- sapply(df$urls_url, paste, collapse = " ")
  df$media_url <- sapply(df$media_url, paste, collapse = " ")
  df$mentions_user_id <- sapply(df$mentions_user_id, paste, collapse = " ")
  df$mentions_screen_name <- sapply(df$mentions_screen_name, paste, collapse = " ")
  df$coords_coords <- sapply(df$coords_coords, paste, collapse = " ")
  df$bbox_coords <- sapply(df$bbox_coords, paste, collapse = " ")
  df$urls_expanded_url <- sapply(df$urls_expanded_url, paste, collapse = " ")
  
  # Due to Twitter API update Nov 2017 --> has to change names of some columns
  names(df)[names(df) == 'urls_url'] <- 'urls'
  names(df)[names(df) == 'coords_coords'] <- 'coordinates'
  names(df)[names(df) == 'bbox_coords'] <- 'bounding_box_coordinates'
  names(df)[names(df) == 'urls_expanded_url'] <- 'urls_expanded'
  
  # Filter only similar columns
  df <- df[, which(names(df) %in% old_col)]
  
  # Merge with old file
  finaldf <- bind_rows(old_df,df)
  
  # Print out current ticker
  print(paste(i,ticker))
  
  # Save update weekly
  write.csv(df,paste0(path_weekly,i,'_',ticker,'_',Sys.Date(),'.csv'))
  
  # Write finaldf into newest csv file
  write.csv(finaldf,paste0(path,i,'_',ticker,'_FULL.csv'))
}

# assign Twitter Auth key before crawling
# TW_key <- 1

# Get twitter auth 14.02.2018
# twitter_token <- create_token(
#   app = 'CryptoSentiTomo',
#   consumer_key = 'cdi1LlwgzdXzR4Wxz8T3Gude6',
#   consumer_secret = 's3hpLYXs9ULY1YwzyTRP8aRovp3rvkjUM9ue9usi8MotrvUgOG')
# 
# rm(twitter_token)
# 
# twitter_token <- create_token(
#   app = '',
#   consumer_key <- "zow0fQ6Lv0j79gx4lDhBKVUDu",
#   consumer_secret = "fp33fr0VBkIIoPzpwgbCPemkZJ1E718TFqb8b86DKd0nVgGFEs")
# 

# Since normally it would stop at # 36 - 37 ==> split into 2 pack with timeout 10 mins

#28.05.2018
for (i in 1:nrow(coins_list)){  
  get_tweets(coins_list$X[i],as.character(coins_list$ticker[i]),100000, TW_key)
  gc()
  # TW_key <- TW_key +1
  # if (TW_key == 5){
  #   Sys.sleep(720)
  #   TW_key <- 1} # sleep 12 mins every 5 auth used
}

# setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/Weekly")
# files <- list.files(pattern = paste0('^',1,'_'))
# max_count <- length(files)
# 
# old_df <- fread(dir(pattern=paste0('^',1,'_'))[1])
# 
# for (k in max_count:2){
#   a <- fread(dir(pattern=paste0('^',1,'_'))[k])
#   old_df <- bind_rows(old_df,a)
#   print(paste0('Reading file number: ',k))
# }
# since <- max(old_df$status_id)
# df <- search_tweets('$BTC',
#                     10,
#                     type = 'recent',
#                     lang = 'en',
#                     since_id = since,
#                     include_rts = FALSE,
#                     retryonratelimit = TRUE)
# 
# test <- search_tweets('$BTC',10)

