
# clear the environment
rm(list= ls())
gc()
# load packages and set options
options(stringsAsFactors = FALSE)

# install packages if not available
packages <- c("readr", #read data
              "lubridate", #date time conversion
              "dplyr", #date manipulation
              "data.table",
              "stringi", #string manipulation
              "stringr","ggplot2","ggfortify",
              "tm","openxlsx","qdapRegex","qdap","NLP","openNLP","h2o",
              "treemap","coinmarketcapr","crypto"
)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

#Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti")
######################################################################
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

######################################################################
# Read in coin list as Oct 17
coins_list <- read.csv("~/GitHub/NextBigCrypto-Senti/1. Crawlers/Top50_Oct7.csv")

for (i in 1:nrow(coins_list)){
  files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/',
                      pattern = paste0('^',i,'_'))
  
  # Load full dataset
  df.full <- read_csv(paste0('~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/',files),
                      locale = locale(encoding = 'latin1')) %>%
    dplyr::select(status_id)
  
  df.full$status_id <- as.character(df.full$status_id)
  df.full <- cbind(coins_list$symbol[i],df.full)
  gc()
  
  if (i == 1){df.final <- df.full}
  if (i != 1){df.final <- bind_rows(df.final,df.full)}
  print(paste0('Complete data for ',coins_list$X[i],'.',coins_list$symbol[i]))
}
# 3.5m Jun 22th
df.final = df.final[!duplicated(df.final$status_id),]

colnames(df.final) <- c('symbol','status_id')

statistic <- df.final %>% count(symbol, sort = TRUE)

openxlsx::write.xlsx(statistic,'~/GitHub/NextBigCrypto-Senti/Statistics.xlsx')

############################################################################
# Total Crypto Market cap (now)

market_today <- get_marketcap_ticker_all()

df1 <- na.omit(market_today[,c('id','market_cap_usd')])
df1$market_cap_usd <- as.numeric(df1$market_cap_usd)
df1$formatted_market_cap <-  paste0(df1$id,'\n','$',
                                    format(df1$market_cap_usd,big.mark = ',',
                                                           scientific = F, trim = T))
png(paste0('Cryptocurency_Cap_',Sys.Date(),'.png'), width = 800, height = 800)
treemap(df1, index = 'formatted_market_cap', vSize = 'market_cap_usd', title = 'Cryptocurrency Market Cap', fontsize.labels=c(12, 8), palette='RdYlGn')
dev.off()

# Historical market cap on 30th Sep 2017

for (i in 1:nrow(coins_list)){
  mkt_cap <- crypto::getCoins(coins_list$symbol[i],
                              start_date = '20170930',
                              end_date = '20170930') %>%
    select(symbol,name,market)
  if (i == 1){mkt_full <- mkt_cap}
  if (i != 1){mkt_full <- bind_rows(mkt_full,mkt_cap)}
}
gc()

df2 <- mkt_full
df2$market <- as.numeric(df2$market)
df2$formatted_market <- paste0(df2$name,'\n','$',
                               format(df2$market, big.mark = ',', scientific = F, trim = T))

jpeg(paste0('Cryptocurency_Cap','.jpg'), width = 800, height = 800)
treemap(df2, index = 'formatted_market', vSize = 'market', title = 'Cryptocurrency Market Cap', fontsize.labels=c(12, 8), palette='RdYlGn')
dev.off()

#######################################################################
# Price (timeseries)
time.slot = 24
token_name = 'BCH'

price.df <- readxl::read_xlsx('~/GitHub/NextBigCrypto-Senti/1. Crawlers/Historical_Data_HR.xlsx') %>%
  filter(symbol == token_name) %>%
  dplyr::select(-date.time)

# filter out 24-hr mark
price.df$mark <- NA

if (time.slot == 6){target <- c(0,6,12,18)}
if (time.slot == 12){target <- c(0,12)}
if (time.slot == 24){target <- c(0)}

for (i in 1:nrow(price.df)){
  if (lubridate::hour(price.df$time[i]) %in% target){price.df$mark[i] <- 1}  
}

price.df <- price.df %>% 
  filter(mark == 1) %>%
  dplyr::select(time,close,priceBTC)

price.df$time <- as.Date(price.df$time)

p1 <- ggplot(price.df, aes(time, close)) + geom_line() + 
      xlab("Time") + ylab("Close Price") + scale_x_date(date_minor_breaks = "7 day") +
      ggtitle("BCH / USD Price")


p2 <- ggplot(price.df, aes(time, priceBTC)) + geom_line(aes(group=1), colour="#000099") + 
      xlab("Time") + ylab("Close Price") + scale_x_date(date_minor_breaks = "7 day") +
      ggtitle("BCH / BTC Price")

multiplot(p1, p2, cols=2)
 
####################################################################
# LDA topics graphs
token_name <- 'BTC'
# Load LDA result directly
files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',
                    pattern = paste0('^',token_name,'_clean_LDA_'))
df.LDA <- read_csv(paste0('~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',files),
                  locale = locale(encoding = 'latin1'))
df.LDA$status_id <- as.character(df.LDA$status_id)
df.LDA$user_id <- as.character(df.LDA$user_id)

count.df <- data.frame(topic = numeric(),
                       count = numeric())
min <- min(df.LDA$topic)
max <- max(df.LDA$topic)

# Count topic
for (i in min:max){
  a <- df.LDA %>% filter(topic == i) %>% count(topic)
  count.df <- count.df %>% rbind(c(i,a$n))
}
names(count.df) <- c('topic','count')

count.df %>% summarise(sum(count))

#################
# Predefined topics graphs

# Load PD result directly
files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',
                    pattern = paste0('^',token_name,'_clean_PD_'))
df.PD <- read_csv(paste0('~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',files),
                  locale = locale(encoding = 'latin1'))
df.PD$status_id <- as.character(df.PD$status_id)
df.PD$user_id <- as.character(df.PD$user_id)

count.df <- data.frame(topic = numeric(),
                       count = numeric())
# Count topic 1 -- 10
for (i in 1:10){
  eval(parse(text = paste0('a <- df.PD %>% filter(topic',i,' == 1) %>% count(topic',i,')')))
  count.df <- count.df %>% rbind(c(i,a$n))
}
names(count.df) <- c('topic','count')

count.df %>% summarise(sum(count))

####################################################################
# Total number of tweets

# Consolidate all available datasets into 1 big dataset
setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/")


# Extract users list from 50 datasets
for (i in 1:nrow(coins_list)){
  a <- as.data.frame(read_csv(dir(pattern=paste0('^',i,'_'))[1],
                              locale = locale(encoding = 'latin1')))
  
  # Extract list of status
  status <- a %>% select(created_at, status_id)
  colnames(status) <- c('created_at','status_id')
  
  if (i == 1){total.status <- status}
  if (i != 1){
    # Merge to final set
    total.status <- bind_rows(total.status,status)
  }
  print(paste0('Finish adding tweets from  ',coins_list$symbol[i]))
}

total.status <- total.status %>% distinct
gc()
total.count <- total.status %>% group_by(created_at) %>% summarise(n = n())

hist(total.count$n)
?hist
min = as.Date('2017-09-30')
max = as.Date('2018-06-20')
ggplot(total.count) +
  geom_line(aes(x=created_at,y=n), color = "#00AFBB", size =1) + 
  scale_x_date(limits = c(min, max), date_breaks = "1 month", date_labels = "%b-%Y") +
  labs(title = "Daily amount of tweets collected", x = "Date", y = "Number of tweets") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1.0, hjust = 1.0))

############################
# Final result 
library(dplyr)

result <- openxlsx::read.xlsx('~/GitHub/NextBigCrypto-Senti/3. Models Development/2.ETH_wBTC_FINAL.xlsx')

names(result) <- c('Model','Interval','Time','Accuracy','F1','Algorithm')

result$F1 <- as.numeric(result$F1)
result$Time <- as.numeric(result$Time)
# get all 3 intervals
all <- result %>% filter(Model == 'LDA')  %>% arrange(Time)

# get best out of 3 intervals
best <- result %>% filter(Model == 'SAT') %>% group_by(Time) %>% slice(which.max(F1))  
best 

final <- result %>% group_by(Model) %>% slice(which.max(F1))  

############################
# Granger Causality Test for PD 

# BTC
token_name <- 'BTC'
# Load PD result directly
files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',
                    pattern = paste0('^',token_name,'_clean_PD_'))
df.PD <- read_csv(paste0('~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',files),
                  locale = locale(encoding = 'latin1'))
df.PD$status_id <- as.character(df.PD$status_id)
df.PD$user_id <- as.character(df.PD$user_id)

