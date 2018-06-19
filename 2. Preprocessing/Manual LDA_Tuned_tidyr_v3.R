# Test with LDA Tuning (4 methods to determine best number of topics)
# Update 20.05 with new preprocessing pipeline

# clear the environment
rm(list= ls())
gc()
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
              "topicmodels","tidyr",
              "ggplot2", # plotting package
              "lda","LDAvis","servr"
)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

#########################################################################
#Input data
# pick correct token
symbol <- 'ETH'
# Load full dataset
# Read in coin list as Oct 17
coins_list <- read.csv("~/GitHub/NextBigCrypto-Senti/1. Crawlers/Top50_Oct7.csv")
position <- match(symbol, coins_list$symbol) # get position in queue

files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/',
                    pattern = paste0('^',position,'_'))
# Load full dataset
df.full <- read_csv(paste0('~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/',files),
                    locale = locale(encoding = 'latin1')) %>%
  dplyr::select(created_at, status_id,user_id, screen_name, text)

###################################################
# load "cleaned" dataset
files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/0. Datasets',
                    pattern = paste0('^',symbol,'_'))
df.clean <- read_csv(paste0('~/GitHub/NextBigCrypto-Senti/0. Datasets/',files),
                     locale = locale(encoding = 'latin1'))
df.clean$status_id <- as.character(df.clean$status_id)
df.clean$user_id <- as.character(df.clean$user_id)

#########################################################################
text <- df.clean$processed
corp <- VCorpus(VectorSource(text)) # VCorpus compatible with n-gram analysis

# Unigram
frequencies <- DocumentTermMatrix(corp)

# Remove these words that are not used very often. Keep terms that appear in 1% or more of the dataset
sparse <- removeSparseTerms(frequencies, 0.99)

ui <- unique(sparse$i)
sparse.new <- sparse[ui,]

gc() # garbage collector

result <- FindTopicsNumber(
  sparse.new,
  topics = seq(from = 2, to = 20, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = 1L,
  verbose = TRUE
)

gc()
############################################################
# minimization for Arun and Cao Juan
# maximization for Griffiths and Deveaud
FindTopicsNumber_plot(result)

result
# 
# ==> Best for 
# BCH is 6
# ETH is 9 ==> 14 (25.05.18)
# BTC is 7 ==> 18 (25.05.18)
# XRP is 12
# LTC is 11

# save.image('~/GitHub/NextBigCrypto-Senti/Models/LDA_ETH_2505.RData')
load('~/GitHub/NextBigCrypto-Senti/Models/LDA_BTC_2405.RData')
rm(sparse,corp,df,frequencies) # remove abundant objects

k = 14

# sparse dtm func
df_lda <- topicmodels::LDA(sparse.new, 
                           k,
                           method = "Gibbs",
                           control = list(seed = 1234))

# rm(list=setdiff(ls(), c("df_lda"))) #remove everything except BTC.clean
# save.image('~/GitHub/NextBigCrypto-Senti/Models/BTC_LDA.RData')

# normal dtm func
# df_lda <- topicmodels::LDA(dtm.new, k, control = list(seed = 1234))
# df_lda

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

gc()

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
market.data <- crypto::getCoins(symbol)

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
    ggtitle(paste0('Sentiment in ',symbol,' (',topicnum,')')) +
    labs(x = "Date", y = "Percent") +
    theme(legend.position="top")
  
  price <- market.data %>% filter(date >= start_date & date <= end_date) %>%
    ggplot(aes(date,close)) +
    geom_line(size = 1.5) +
    labs(x = "Date", y = "Price") +
    ggtitle(paste0(symbol,' Price'))
  
  setwd("~/GitHub/NextBigCrypto-Senti/x. Documents/images")
  png(paste0('LDA_',symbol,'_',topicnum,'_',Sys.Date(),'.png'),width=800, height=800)
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
