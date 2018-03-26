# Test with LDA Tuning (4 methods to determine best number of topics)

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
              "lda","LDAvis","servr"
)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)


#########################################################################
#Input data
df <- as.data.frame(read_csv('1. Crawlers/1b. Report/2_$ETH_FULL.csv',
                             locale = locale(encoding = 'latin1')))
name.df <- 'ETH'

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

df <- df[,which(colnames(df) %in% c('created_at','text'))] %>%
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
test <- df$processed
str(test)

# Alr know result (28.02.2018)
dtm <- CreateDtm(test,
                 doc_names = c(1:length(test)),
                 ngram_window = c(1, 1),
                 lower = FALSE,
                 remove_punctuation = FALSE,
                 remove_numbers = FALSE)

rowTotals <- rowSums(dtm) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words

result <- FindTopicsNumber(
  dtm.new,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
############################################################
# minimization for Arun and Cao Juan
# maximization for Griffiths and Deveaud
FindTopicsNumber_plot(result)

result

# ==> Best for 
# BCH is 6
# ETH is 9
# BTC is 7
# XRP is 12
# LTC is 11
# reuse dtm.new from RData LDATune

k = 9

df_lda <- LDA(dtm.new, k, control = list(seed = 1234))
df_lda
# Word-topic probabilities
df_topics <- tidy(df_lda, matrix = "beta")
df_topics

df_top_terms <- df_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# 10 terms that are most common within each topic
dev.new()
df_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#save.image(file = paste0('LDA_LTC_v3_',Sys.Date(),'.RData'))

# load 080318
load('./Models/LDA_ETH_v3_2018-03-03.RData')
## Build classifier
df_gamma <- tidy(df_lda, matrix = "gamma")
df_gamma

# Assign topic back to document
df_classifications <- df_gamma %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup()

df_classifications$document <- as.numeric(df_classifications$document)
# sort topics
df_classifications <- df_classifications[with(df_classifications, order(document)), ] 

#### Reassign topics back to main df
maindf <- df
maindf$topic <- NA

for (i in df_classifications$document){
  maindf[i,which(colnames(maindf)=="topic")] <- df_classifications$topic[i]
}


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
# Simple Sentiment analysis

maindf <- maindf[,which(colnames(maindf) %in% c('created_at','processed','topic'))]

##########################
# Loading price data
market.data <- crypto::getCoins(name.df)

start_date <- min(as.Date(maindf$created_at))
end_date <- max(as.Date(maindf$created_at))

############################################################
plot.senti <- function(df,topicnum){
  
  if (topicnum != 0){df <- df %>% filter(topic == topicnum)}
  
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
    mutate(percent = n / total_words) %>%
    # Set up the plot with aes()
    ggplot(aes(date, percent, color = sentiment)) +
    geom_line(size = 1.5) +
    geom_smooth(method = "lm", se = FALSE, lty = 2) +
    expand_limits(y = 0) +
    ggtitle(paste0('Sentiment in ',name.df,' (',topicnum,')')) +
    labs(x = "Date", y = "Percent") +
    theme(legend.position="top")
  
  price <- market.data %>% filter(date >= start_date & date <= end_date) %>%
    ggplot(aes(date,close)) +
    geom_line(size = 1.5) +
    labs(x = "Date", y = "Price") +
    ggtitle(paste0(name.df,' Price'))
  
  setwd("~/GitHub/NextBigCrypto-Senti/x. Documents/images")
  png(paste0('LDA_',name.df,'_',topicnum,'_',Sys.Date(),'.png'),width=800, height=800)
  # plot charts
  multiplot(senti,price, cols = 1)
  dev.off()
}

for (i in 0:k) {
  plot.senti(maindf,i)  
}

topic <- c(1:k)
count <- NA
ndf <- data.frame(topic,count)


# Analysis
for (i in 1:k) {
  ndf$topic[i] <- i
  ndf$count[i] <- nrow(maindf %>% filter(topic == i))
}

load('./Models/LDA_ETH_v3_2018-03-03.RData')
ggplot(df_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

dev.new()
ggplot(df_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))
