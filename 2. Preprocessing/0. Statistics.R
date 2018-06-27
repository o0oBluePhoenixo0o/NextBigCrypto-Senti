
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

###########
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

##############
# Price (timeseries)
time.slot = 24
token_name = 'ETH'

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
      ggtitle("ETH / USD Price")


p2 <- ggplot(price.df, aes(time, priceBTC)) + geom_line(aes(group=1), colour="#000099") + 
      xlab("Time") + ylab("Close Price") + scale_x_date(date_minor_breaks = "7 day") +
      ggtitle("ETH / BTC Price")

multiplot(p1, p2, cols=2)
 