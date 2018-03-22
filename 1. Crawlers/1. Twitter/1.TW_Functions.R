# Set up working directory
setwd("C:/Users/BluePhoenix/Documents/GitHub/NextBigCrypto-Senti/1. Crawlers")

# Clear environment
rm(list = ls())

# install packages if not available
packages <- c('rtweet','twitteR', #Twitter API crawlers
              'tesseract', 'magick', #image processing
              'data.table','dplyr','scales','ggplot2','taskscheduleR',
              'httr','stringr','rvest','curl','lubridate')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

# Twitter Crawler Setup --------------------------------

#Setting the Twitter authentication1
# consumer_key <- 'yO6HMXQfaAazZfdyOQpPicX0M'
# consumer_secret <- 'wT1lq9bd7WWJjoVw3aHKfdHbpdjxd8r8RKc56fGiQPGRaJgILP'
# access_token <- '379008223-8gPeX8OJ5wxjILXYUMxKwTSOH30UJbYdUWNqCE53'
# access_token_secret <- 'P3anD6dTrrQb6RUP4Me6HAMpgY8RU9QuORCrGI14f1Wis'
# setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)

# #Setting the Twitter authentication2
# consumer_key="hNgXlxwQYcL71SxCddwvpTEVf"
# consumer_secret="ZSXtL7Yq5QwkAvyCnm9hACaC6CosyHUOOnewv2ufL6IG8tQBCU"
# access_token="838380485843763200-pAQXVTl89Dn1Pz2GnQzOacBmJnXPZz6"
# access_token_secret="MtqyBbhUxM0zOTJIuRXUWtZMRmVnnjfFT0rs5X4odItdq"
# setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)
# 
# #Setting the Twitter authentication3
# consumer_key <- "zow0fQ6Lv0j79gx4lDhBKVUDu"
# consumer_secret = "fp33fr0VBkIIoPzpwgbCPemkZJ1E718TFqb8b86DKd0nVgGFEs"
# access_token = "836598863582617600-Tjmc0MqCtcOZVjx9dto5wSBkdRgxDmh"
# access_token_secret = "57THIHAlttLUf3y8x1P5U2JnQmcDIfDqq8xmZrwgD5Qo6"
# setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)

#Setting the Twitter authentication0
consumer_key <- 'cdi1LlwgzdXzR4Wxz8T3Gude6'
consumer_secret <- 's3hpLYXs9ULY1YwzyTRP8aRovp3rvkjUM9ue9usi8MotrvUgOG'
access_token <- '240771509-MQiqGMegj3B4ohmRSi7mThfprMg7j9lAYkDB3s9W'
access_token_secret <- 'YZGMyJ6Jz3Ncx1SG59QXJGaRrEkYjvCTC71KPsFH2eaIi'
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)

get_report <- function(n)
{
  #dataset with full complete data (w/o created_at)
  df <- get_timeline_('CryptoWatson',n,
                      excludeReplies = TRUE,
                      include_rts = FALSE,
                      retryonratelimit = TRUE)  
  #dataset with created_at populated
  df_time <- userTimeline('CryptoWatson',n,
                          excludeReplies = TRUE,
                          includeRts = FALSE)
  df_time <- twListToDF(df_time) 
  
  #filter only '1hr Report' tweets
  df <- df[substr(df$text,1,10) == '1hr Report',]
  df_time <- df_time[substr(df_time$text,1,10) == '1hr Report',]
  
  # extract only status_id and created_at
  colnames(df_time)[which(names(df_time) == "id")] <- "status_id"
  colnames(df_time)[which(names(df_time) == "created")] <- "created_at"
  df_time <- df_time[,c("status_id","created_at")]
  
  # Merge to get creation time
  df_final <- left_join(df,df_time, by = c("status_id"))
  df_final <- df_final[,!colnames(df_final) == "created_at.x"]
  colnames(df_final)[which(names(df_final) == "created_at.y")] <- "created_at"
  
  # Remove only-NAs columns
  df_final <- df_final[,colSums(is.na(df_final))<nrow(df_final)]
  
  # Remove NAs created_at
  df_final <- df_final[!is.na(df_final$created_at),]
  
  return(df_final)
}
