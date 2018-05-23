# Clear environment
rm(list = ls())
gc()
options("scipen"=100, "digits"=4)

# Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers")

# install packages if not available
packages <- c('jsonlite','forecast', 'dplyr',
              'anytime','lubridate','readr','openxlsx')

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

##############################################
#
# IMPORTANT - ALL TIME IS SET TO CENTRAL EUROPEAN TIME (CEST)
#

# Get list of top 50 crypto (Oct 17)
coins_list <- read.csv("~/GitHub/NextBigCrypto-Senti/1. Crawlers/Top50_Oct7.csv")

Get.Hourly.Data <- function(df,symbol,timelimit,current){
  # Get current sys.time in epoch format
  end_epoch <- as.integer(as.POSIXct(Sys.time()))
  
  # Input data for API access
  url <-'https://min-api.cryptocompare.com/data/histohour?'
  token <- paste0('fsym=',symbol)
  target.cur <- '&tsym=USD' # convert to USD
  limit <- '&limit=2000' # max responses retrieve
  hourly <- paste0('&aggregate=',timelimit) # aggregate 1-hr data
  
  # # start collect data from this day
  # # epoch 1506729600 --> 30.09.2017 00:00:00 UTC (02:00:00 CEST)
  # # for limit = 2000 ==> collect around 83.25 days (with 6hr-range)
  # # example
  range <- as_datetime('2017-09-30 02:00:00')-as_datetime('2017-07-08 20:00:00')
  # 
  # # calculate 1st epoch for 1st batch
  # end_date <- anytime(1506729600) + range
  # end_date <- as.integer(as.POSIXct(end_date)) # 22-12-2017
  # toTs <- paste0('&toTs=',end_date) # insert end epoch (until this time)
  # 
  # API.call <- paste0(url,token,target.cur,limit,hourly,toTs)
  # 
  # # Collect response from API
  # hist.token <- fromJSON(API.call)
  # hist.token.hr <- hist.token$Data
  hist.token.hr <- df
  current_epoch <- current
  flag <- 0
  
  # loop until up-to-date data
  while (end_epoch > current_epoch & flag == 0) {
    end_date <- anytime(current_epoch) + range
    # Double check end date
    if (end_date > Sys.time()){
      end_date <- as.integer(as.POSIXct(end_date))
      flag <- 1}
    
    toTs <- paste0('&toTs=',end_date) # insert end epoch (until this time)
    
    # Call API
    API.call <- paste0(url,token,target.cur,limit,hourly,toTs)
    
    # Collect response from API
    hist.token <- fromJSON(API.call)
    new.df <- hist.token$Data %>% select(time,close)
    hist.token.hr <- rbind(hist.token.hr,new.df)
    
    print(paste0('Current epoch time: ',anytime(current_epoch)))
    current_epoch <- max(hist.token.hr$time)
    gc()
  }
  
  hist.token.hr <- hist.token.hr[!duplicated(hist.token.hr$time),]
  # Convert epoch to date time
  date.time <- anytime(hist.token.hr$time)
  hist.token.hr <- cbind(date.time,symbol,hist.token.hr)
  
  return(hist.token.hr)
}

# hourly.df <- read.xlsx('~/GitHub/NextBigCrypto-Senti/1. Crawlers/Historical_Data_HR.xlsx') %>%
#   select(date.time,symbol,time,close) # to fit the function above

hourly.df <- read.xlsx(paste0('~/GitHub/NextBigCrypto-Senti/1. Crawlers/Historical_Data_HR.xlsx')) %>%
  select(symbol,time,close) # to fit the function above

# Get hourly price data
for (i in 1:nrow(coins_list)){  
  token <- as.character(coins_list$symbol[i])
  timelimit <- 1
  current <- max(hourly.df$time)
  df <- hourly.df %>% filter(symbol == token) %>% select(-symbol)
  
  if(i == 9){next} # skip
  if(i == 1){final.hr <- Get.Hourly.Data(df,
                                         token,timelimit,current)}
  if(i != 1){final.hr <- rbind(final.hr,Get.Hourly.Data(df,
                                                        token,timelimit,current))}
  
  print(paste0('Complete collect ',timelimit,'-hour data for ',token,' at position ',i))
}

bk <- final.hr

# Add column "price agaisnt BTC" 
final.BTC <- final.hr %>% filter(symbol == 'BTC')

priceBTC <- inner_join(final.hr,final.BTC, by = 'time') %>%
  mutate(priceBTC = close.x / close.y) %>% 
  select(symbol.x,time,priceBTC)
priceBTC <- unique(priceBTC)

final.df <- as.data.frame(inner_join(final.hr,priceBTC, by = c('symbol' = 'symbol.x', 'time' = 'time' )))

# filter out close =0
final.df <- final.df %>% filter(close !=0)
write.xlsx(final.df,paste0('~/GitHub/NextBigCrypto-Senti/1. Crawlers/Historical_Data_HR.xlsx'))
# backup
write.xlsx(final.df,paste0('~/GitHub/NextBigCrypto-Senti/1. Crawlers/Historical_Data_HR_',Sys.Date(),'.xlsx'))

