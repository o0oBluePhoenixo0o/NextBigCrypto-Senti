# clear the environment
rm(list= ls())

# load packages and set options
options(stringsAsFactors = FALSE)

#Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti/")

# install packages if not available
packages <- c("ldatuning", #tuning LDA topic numbers
              "readr", #read data
              "lubridate", #date time conversion
              "dplyr", #date manipulation
              "tm", # text mining package
              "textmineR",
              "tidytext",
              "topicmodels",
              "ggplot2", # plotting package
              "quanteda", #kwic function search phrases
              "xtable", "DT", #viewing data type from quanteda
              "stringi", #string manipulation
              "wordcloud","tidyquant"
)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

#########################################################################
#Input data
df <- as.data.frame(read_csv('1. Crawlers/1b. Report/1_$BTC_FULL.csv',
                             locale = locale(encoding = 'latin1')))
name.df <- 'BTC'

#########################################################################



# IMPORTANT - For converting non-Unicode character from csv file (took 1 month to recognize)
trueunicode.hack <- function(string){
  m <- gregexpr("<U\\+[0-9A-F]{4}>", string)
  if(-1==m[[1]][1])
    return(string)
  
  codes <- unlist(regmatches(string, m))
  replacements <- codes
  N <- length(codes)
  for(i in 1:N){
    replacements[i] <- intToUtf8(strtoi(paste0("0x", substring(codes[i], 4, 7))))
  }
  
  # if the string doesn't start with a unicode, the copy its initial part
  # until first occurrence of unicode
  if(1!=m[[1]][1]){
    y <- substring(string, 1, m[[1]][1]-1)
    y <- paste0(y, replacements[1])
  }else{
    y <- replacements[1]
  }
  
  # if more than 1 unicodes in the string
  if(1<N){
    for(i in 2:N){
      s <- gsub("<U\\+[0-9A-F]{4}>", replacements[i], 
                substring(string, m[[1]][i-1]+8, m[[1]][i]+7))
      Encoding(s) <- "UTF-8"
      y <- paste0(y, s)
    }
  }
  
  # get the trailing contents, if any
  if( nchar(string)>(m[[1]][N]+8) )
    y <- paste0( y, substring(string, m[[1]][N]+8, nchar(string)) )
  y
}

df <- df[,which(colnames(df) %in% c('created_at','text','status_id'))] %>%
  mutate(timestamp = ymd_hms(created_at))

# Convert unicode
df$text <- sapply(df$text,function(x) trueunicode.hack(x))

# remove duplicates base on tweets
df <- df[!duplicated(df$text),]

##################################################
# Preprocessing steps
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "") #delete "byte" ==> delete emoticons unicode
removeURL <- function(x) gsub('"(http.*) |(https.*) |(http.*)$|\n', "", x)

# Function to replace ' and " to spaces before removing punctuation 
# to avoid different words from binding 
AposToSpace = function(x){
  x= gsub("'", ' ', x)
  x= gsub('"', ' ', x)
  x =gsub('break','broke',x) # break may interrupt control flow in few functions
  return(x)
}

###
df$processed <- sapply(df$text, function(x) trueunicode.hack(x))
df$processed <- sapply(df$processed, function(x) conv_fun(x)) # convert to delete emojis
df$processed <- sapply(df$processed, function(x) removeURL(x)) # remove URL

# will remove stopwords later in dtm step
df$processed <- sapply(df$processed, function(x) removeWords(x,stopwords("english"))) 

# remove punctuations except for # $ @
#df$processed <- sapply(df$processed, function(x) removePunctuation(x))
df$processed <- sapply(df$processed, function(x) gsub( "[^#@$a-zA-Z\\s]" , "" , x , perl = TRUE ))

df$processed <- sapply(df$processed, function(x) AposToSpace(x)) 
df$processed <- sapply(df$processed, function(x) stripWhitespace(x))

# remove whitespace before & after
df$processed <- sapply(df$processed, function(x) gsub("^[[:space:]]+", "",x))
df$processed  <- sapply(df$processed, function(x) gsub("[[:space:]]+$", "",x))

#test with removing number 02.03.2018
df$processed <- sapply(df$processed, function(x) removeNumbers(x))

# Remove duplicates (after rmv url)
df <- df[!duplicated(df$processed),]
# Remove blanks
df <- df[!(is.na(df$processed) | df$processed==""), ]
# Remove <f0>
df$processed <- gsub("<f0>", "", df$processed)
###################################################################

#backup
bk <- df
####
# Topic creation
####

# Only make STEM version
# 10 terms / topic

# Regulations
topic1.regulation <- c("regulate", #regul stem
                      "regulation", # no stem avail
                      "SEC", "bank", "collaps", "tax", "rule", "govern",
                      "law", "crackdown")
# technical analysis
topic2.TA <- c("TA", "chart", "RSI", "MACD",
               "support", "line", "signal",
               "elliot wave", "technical analysis", "resistance", "Fibonacci")

# Initial coin offering
topic3.ICO <- c("ICO", "airdrop", "discord", "presale", "telegram",
                "join", "follow", "free", "team", "pre-ICO")

# incidents (negative)
topic4.incidents <- c("hack", "leak", "ban", "scam", "FUD", #fear - uncertain - doubt
                      "FOMO", "lose", "fail", "stolen", "stole", "steal","ponzi")

# trading
topic5.trading <- c("list", "exchange", "trade", "price", "buy", "sell", "HODL",
                    "PnD", "pump", "dump", "ATH", "ATL","hedge","API")

# exchanges
topic6.exchanges <- c("binance","bittrex", "poloniex", "Kucoin", "kraken", "bitstamp",
                      "okex","cryptobridge", "remitano", "hitbtc", "Liqui", " bithumb",
                      "huobi","bitfinex", "upbit","gdax","bitflyer", "gemini", "coinone","coinbase")

# news on mainstream
topic7.mainstream <- c("media", "coindesk", "cryptonews","cnbc", "bloomberg", "cointelegraph",
                       "wallstreet","reuters","themerkle", "news")

# project details
topic8.project <- c("partnership", "list", "team", "update", "github", "meetup",
                    "conference","announce","announcement", "launch", "release")


# Function add topic id to main df
addtopic <- function(maindf, topicdf, topicid){
  
  #kwic detect match
  topic.result <- kwic(maindf$stem, stemDocument(topicdf))
  topic.result <- as.data.frame(topic.result)
  topic.result$docid <- substr(topic.result$docname,5,stri_length(topic.result$docname))
  
  # extract document id from topic results
  topic.list <- as.numeric(topic.result$docid)
  print('Done extraction... now merging...')
  
  # fill correct topic to maindf
  for (i in topic.list){
    maindf[i,which(colnames(df)==paste0("topic",topicid))] <- 1
  }
  return(maindf)
}

# Create stem document column for main dataset
df$stem <- sapply(df$processed, function(x) stemDocument(x))

df$topic1 <- 0
df$topic2 <- 0
df$topic3 <- 0
df$topic4 <- 0
df$topic5 <- 0
df$topic6 <- 0
df$topic7 <- 0
df$topic8 <- 0

# Add topicid to main df
df <- addtopic(df,topic1.regulation,'1')
df <- addtopic(df,topic2.TA,'2')
df <- addtopic(df,topic3.ICO,'3')
df <- addtopic(df,topic4.incidents,'4')
df <- addtopic(df,topic5.trading,'5')
df <- addtopic(df,topic6.exchanges,'6')
df <- addtopic(df,topic7.mainstream,'7')
df <- addtopic(df,topic8.project,'8')

# Simple Sentiment analysis

maindf <- df[,which(colnames(df) %in% c('created_at','status_id','processed',
                                        'topic1','topic2','topic3','topic4',
                                        'topic5','topic6','topic7','topic8'))]

###############################
# Word cloud for each topic
setwd("~/GitHub/NextBigCrypto-Senti/x. Documents/images")
dev.new()
topic1 <- maindf %>%
  select(created_at, processed,topic1) %>%
  filter(topic1==1)

topic1 %>%  unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light(),main="Title"))
savePlot(paste0(name.df,'_topic1_Regulation'),type='png')
dev.off()
#
dev.new()
topic2 <- maindf %>%
  select(created_at, processed,topic2) %>%
  filter(topic2==1)

topic2 %>% unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic2_TA'),type='png')
dev.off()
#
dev.new()
topic3 <- maindf %>%
  select(created_at, processed,topic3) %>%
  filter(topic3==1)

topic3 %>% unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic3_ICO'),type='png')
dev.off()
#
dev.new()
topic4 <- maindf %>%
  select(created_at, processed,topic4) %>%
  filter(topic4==1)

topic4 %>%  unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic4_incidents'),type='png')
dev.off()
#
dev.new()
topic5 <- maindf %>%
  select(created_at, processed,topic5) %>%
  filter(topic5==1)

topic5 %>% unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic5_trading'),type='png')
dev.off()
#
dev.new()
topic6 <- maindf %>%
  select(created_at, processed,topic6) %>%
  filter(topic6==1)

topic6 %>% unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic6_exchanges'),type='png')
dev.off()
#
dev.new()
topic7 <- maindf %>%
  select(created_at, processed,topic7) %>%
  filter(topic7==1) 

topic7 %>%  unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic7_mainstream'),type='png')
dev.off()
#
dev.new()
topic8 <- maindf %>%
  select(created_at, processed,topic8) %>%
  filter(topic8==1)

topic8 %>%  unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic8_project'),type='png')
dev.off()

setwd("~/GitHub/NextBigCrypto-Senti/")


#######################################################################
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


#######################################################

##########################
# Loading price data
market.data <- crypto::getCoins(name.df)

start_date <- min(as.Date(maindf$created_at))
end_date <- max(as.Date(maindf$created_at))

############################################################
plot.senti <- function(df,senti.name){
  
  sentiment_by_time <- df %>% unnest_tokens(word,processed) %>%
    # Define a new column using floor_date()
    mutate(date = floor_date(created_at, unit = "1 day")) %>%
    # Group by date
    group_by(date) %>%
    mutate(total_words = n()) %>%
    ungroup() %>%
    # Implement sentiment analysis using the NRC lexicon
    inner_join(get_sentiments("nrc"))
  
  # sentiments: negative, positive
  # emotions: anger, anticipation, disgust, fear, joy, sadness, surprise, trust
  senti <- sentiment_by_time %>%
    # Filter for positive and negative words
    filter(sentiment %in% c("positive", "negative")) %>%
    # Count by date, sentiment, and total_words
    count(date, sentiment, total_words) %>%
    ungroup() %>%
    mutate(percent = n) %>%
    # Set up the plot with aes()
    ggplot(aes(date, percent, color = sentiment)) +
    geom_line(size = 1.5) +
    geom_smooth(method = "lm", se = FALSE, lty = 2) +
    expand_limits(y = 0) +
    ggtitle(paste0('Sentiment in ',name.df,' ( ',senti.name,')')) +
    labs(x = "Date", y = "Count") +
    theme(legend.position="top")
  
  price <- market.data %>% filter(date >= start_date & date <= end_date) %>%
    ggplot(aes(date,close)) +
    geom_line(size = 1.5) +
    labs(x = "Date", y = "Price") +
    ggtitle(paste0(name.df,' Price'))
  
  png(paste0(name.df,'_',senti.name,'_',Sys.Date(),'.png'),width=800, height=800)
  
  # plot charts
  multiplot(senti,price, cols = 1)
  setwd("~/GitHub/NextBigCrypto-Senti/x. Documents/images")
  
  # save plots
  #savePlot(paste0(name.df,'_',senti.name,'_',Sys.Date()),type='png')
  dev.off()
}

# Plotting topic agaisnt senti
plot.senti(topic1,'Regulation')
plot.senti(topic2,'Technical Analysis')
plot.senti(topic3,'Initial Coins Offering')
plot.senti(topic4,'Incidents')
plot.senti(topic5,'Trading')
plot.senti(topic6,'Exchanges')
plot.senti(topic7,'Mainstream Media')
plot.senti(topic8,'Project Updates')
plot.senti(maindf,'Overall')

#save backup
save.image('Predefined_Topic_BTC_v1.RData')
