# clear the environment
rm(list= ls())

# load packages and set options
options(stringsAsFactors = FALSE)

#Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti/")

#https://github.com/SebastianKirsch123/ensemble_sentiment_classification/blob/master/Ensemble_Sentiment_Classification.pdf

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
#remove.packages(c("tidytext", "ggplot2"))
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

# line 140, change to higher interval
#trace(utils:::unpackPkgZip, edit=TRUE)

#########################################################################
#Input data
df <- as.data.frame(read_csv('1. Crawlers/1b. Report/1_$BTC_FULL.csv',
                             locale = locale(encoding = 'latin1')))
name.df <- 'BTC'

#########################################################################

df <- df[,which(colnames(df) %in% c('screen_name','user_id','created_at','text','status_id'))] %>%
  mutate(timestamp = ymd_hms(created_at))

######################
#
# FUNCTIONS CREATION
#
######################

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

conv_fun <- function(x) iconv(x, "latin1", "ASCII", "") # delete "byte" ==> delete emoticons unicode

# Remove URL now is performed by qdapRegex 25.04.2018
# removeURL <- function(x) gsub('"(http.*) |(https.*) |(http.*)$|\n', "", x)
removeURL <- function(x) rm_url(x, pattern=pastex("@rm_twitter_url", "@rm_url"))

# Function to replace ' and " to spaces before removing punctuation 
# to avoid different words from binding 
AposToSpace = function(x){
  x= gsub("'", ' ', x)
  x= gsub('"', ' ', x)
  return(x)
}

###################################################################

Cleandata <- function(df) {
  
  df$status_id <- as.character(df$status_id)
  
  # Convert unicode
  df$text <- sapply(df$text,function(x) trueunicode.hack(x))
  
  # remove duplicates base on tweets
  df <- df[!duplicated(df$text),]
  
  ###
  df$processed <- df$text
  df$processed <- sapply(df$processed, function(x) conv_fun(x)) # convert to delete emojis
  df$processed <- sapply(df$processed, function(x) removeURL(x)) # remove URL
  df$processed <- sapply(df$processed, function(x) gsub("[\r\n]", " ", x)) #change /r /n break lines into space
  # To lower case
  df$processed <- sapply(df$processed, function(x) tolower(x))
  
  # remove stopwords - create exception lists 25.04
  exceptions   <- c('up','down','all','above','below','under','over',
                    'few','more', 'in')
  # keep negation list
  negations <- grep(pattern = "not|n't", x = stopwords(), value = TRUE)
  exceptions <- c(exceptions,negations)
  
  my_stopwords <- setdiff(stopwords("en"), exceptions)
  
  df$processed <- sapply(df$processed, function(x) removeWords(x,c(my_stopwords))) 
  
  ###########################################
  
  # Get rid of references to other screennames
  df$processed <- str_replace_all(df$processed,"@[a-z,A-Z]*","")  
  
  # remove punctuations except for # $ 
  df$processed <- sapply(df$processed, function(x) gsub( "[^#$a-zA-Z\\s]" , "" , x , perl = TRUE ))
  
  # Apply Apos to space
  df$processed <- sapply(df$processed, function(x) AposToSpace(x)) 
  
  # removing number 02.03.18
  df$processed <- sapply(df$processed, function(x) removeNumbers(x))
  
  # Remove left-overs
  df$processed <- sapply(df$processed, function(x) gsub("ff", " ",x))
  df$processed <- sapply(df$processed, function(x) gsub("# ", " ", x))
  df$processed <- sapply(df$processed, function(x) gsub(" f ", " ", x))
  
  # remove whitespace before & after
  df$processed <- sapply(df$processed, function(x) gsub("^[[:space:]]+", "",x))
  df$processed  <- sapply(df$processed, function(x) gsub("[[:space:]]+$", "",x))
  df$processed <- sapply(df$processed, function(x) stripWhitespace(x))
  
  # Remove blank processed messages
  df <- df[!(is.na(df$processed) | df$processed %in% c(""," ")), ]
  
  # Final remove duplicates (after rmv url)
  df <- df[!duplicated(df$processed),]
  return(df)
}
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
                      "SEC", "bank", "collaps", "tax", "govern",
                      "law", "crackdown","court")

# technical analysis
topic2.TA <- c("TA", "chart", "RSI", "MACD",
               "support", "line", "signal",
               "elliot wave", "technical analysis", "resistance", "Fibonacci")

# Initial coin offering
topic3.ICO <- c("ICO", "airdrop", "discord", "presale", "telegram",
                "join", "follow", "free", "team", "pre-ICO")

# incidents (negative)
topic4.incidents <- c("hack", "leak", "ban", "fake", "scam", "FUD", #fear - uncertain - doubt
                      "FOMO", "lose", "fail", "stolen", "stole", "steal","ponzi",
                      "pyramid scheme", "rob")

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
                    "conference","milestone","announce","announcement", "launch", "release",
                    "whitepaper","yellowpaper")

# technology (01.04.2018)
topic9.technology <- c("lightning network", "plasma", "scaling", "scale", 
                       "POS","POW","DPOS","POE","masternode","privacy","zerocoin",
                       "tumbling","coin mixer","segwit","algorithm")

# mining (01.04.2018)
topic10.mining <- c("POW","mining","block reward","ASIC","GPU Mining","NVIDIA",
                    "AMD","GTX","farm","halving","pool","mining pool","znomp","nomp",
                    "miningcore","suprnova","miningpoolhub")

# Function add topic id to main df
addtopic <- function(maindf, topicdf, topicid){
  
  #kwic detect match
  topic.result <- kwic(maindf$stem, stemDocument(tolower(topicdf)))
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
df$stem <- stemDocument(tolower(x))


df$topic1 <- 0
df$topic2 <- 0
df$topic3 <- 0
df$topic4 <- 0
df$topic5 <- 0
df$topic6 <- 0
df$topic7 <- 0
df$topic8 <- 0
df$topic9 <- 0
df$topic10 <- 0

# Add topicid to main df
df <- addtopic(df,topic1.regulation,'1')
df <- addtopic(df,topic2.TA,'2')
df <- addtopic(df,topic3.ICO,'3')
df <- addtopic(df,topic4.incidents,'4')
df <- addtopic(df,topic5.trading,'5')
df <- addtopic(df,topic6.exchanges,'6')
df <- addtopic(df,topic7.mainstream,'7')
df <- addtopic(df,topic8.project,'8')
df <- addtopic(df,topic9.technology,'9')
df <- addtopic(df,topic10.mining,'10')

# Simple Sentiment analysis

maindf <- df[,which(colnames(df) %in% c('created_at','status_id','processed',
                                        'topic1','topic2','topic3','topic4',
                                        'topic5','topic6','topic7','topic8',
                                        'topic9','topic10'))]
# Change statusid type
maindf$status_id <- as.character(maindf$status_id)

######################################################################################################
# Word cloud for each topic
setwd("~/GitHub/NextBigCrypto-Senti/x. Documents/images")

dev.new()
topic1 <- maindf %>%
  select(created_at, processed,status_id,topic1) %>%
  filter(topic1==1)

topic1 %>%  unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light(),main="Title"))
savePlot(paste0(name.df,'_topic1_Regulation_',Sys.Date()),type='png')
dev.off()

#
dev.new()
topic2 <- maindf %>%
  select(created_at, processed,status_id,topic2) %>%
  filter(topic2==1)

topic2 %>% unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic2_TA_',Sys.Date()),type='png')
dev.off()

#
dev.new()
topic3 <- maindf %>%
  select(created_at, processed,status_id,topic3) %>%
  filter(topic3==1)

topic3 %>% unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic3_ICO_',Sys.Date()),type='png')
dev.off()

#
dev.new()
topic4 <- maindf %>%
  select(created_at, processed,status_id,topic4) %>%
  filter(topic4==1)

topic4 %>%  unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic4_incidents_',Sys.Date()),type='png')
dev.off()

#
dev.new()
topic5 <- maindf %>%
  select(created_at, processed,status_id,topic5) %>%
  filter(topic5==1)

topic5 %>% unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic5_trading_',Sys.Date()),type='png')
dev.off()

#
dev.new()
topic6 <- maindf %>%
  select(created_at, processed,status_id,topic6) %>%
  filter(topic6==1)

topic6 %>% unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic6_exchanges_',Sys.Date()),type='png')
dev.off()

#
dev.new()
topic7 <- maindf %>%
  select(created_at, processed,status_id,topic7) %>%
  filter(topic7==1) 

topic7 %>%  unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic7_mainstream_',Sys.Date()),type='png')
dev.off()

#
dev.new()
topic8 <- maindf %>%
  select(created_at, processed,status_id,topic8) %>%
  filter(topic8==1)

topic8 %>%  unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic8_project_',Sys.Date()),type='png')
dev.off()

#
dev.new()
topic9 <- maindf %>%
  select(created_at, processed,status_id,topic9) %>%
  filter(topic9==1)

topic9 %>%  unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic9_technology_',Sys.Date()),type='png')
dev.off()

#
dev.new()
topic10 <- maindf %>%
  select(created_at, processed,status_id,topic10) %>%
  filter(topic10==1)

topic10 %>%  unnest_tokens(word,processed)%>%
  count(word) %>%
  mutate(word = removeNumbers(word)) %>%
  with(wordcloud(word, n, max.words = 100, colors = palette_light()))
savePlot(paste0(name.df,'_topic10_mining_',Sys.Date()),type='png')
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
plot.senti(topic9,'Technology')
plot.senti(topic10,'Mining')
plot.senti(maindf,'Overall')

#save backup
save.image(paste0('Predefined_Topic_BTC_',Sys.Date(),'.RData'))


# Testing ground 22.03.18
# 
# load('./Models/Predefined_Topic_BTC_v1.RData')
# 
# # Convert to tidy
# topic5_tidy <- topic5 %>%
#   unnest_tokens(word,processed)%>%
#   # Define a new column using floor_date()
#   mutate(date = floor_date(created_at, unit = "1 day")) %>%
#   group_by(date,status_id) %>%
#   mutate(total_words = n()) %>%
#   ungroup()
# 
# # AFINN Lexcicon
# sentiment_messages_afinn <- topic5_tidy %>%
#   inner_join(get_sentiments("afinn"), by = "word") %>%
#   group_by(date,status_id) %>%
#   summarize(sentiment = mean(score),
#             words = n()) %>%
#   ungroup()
# 
# no_pos_afinn <- sentiment_messages_afinn %>%
#   
# 
# # NRC Lexicon
# sentiment_messages_nrc <- topic5_tidy %>%
#   inner_join(get_sentiments("nrc"), by = "word") %>%
#   group_by(date,status_id) %>%
#   ungroup() %>%
#   filter(sentiment %in% c("positive", "negative"))

