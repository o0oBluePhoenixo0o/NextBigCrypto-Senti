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
# Update 20.05 with new preprocessing pipeline
df <- as.data.frame(read_csv('~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/1_$BTC_FULL.csv',
                                locale = locale(encoding = 'latin1'))) %>% 
  dplyr::select(created_at, status_id, screen_name, user_id, text)

name.df <- 'BTC'

# Load preprocessing file
source('~/GitHub/NextBigCrypto-Senti/2. Preprocessing/1. Preprocessing_TW.R')

df <- Cleandata(df)
df$status_id <- as.character(df$status_id)
df$user_id <- as.character(df$user_id)

# 22.05 clean BTC df
df <- read_csv('~/GitHub/NextBigCrypto-Senti/0. Datasets/BTC_clean_2205.csv') %>%
  filter(created_at < '2018-04-15')
#########################################################################
text <- df$processed
corp <- VCorpus(VectorSource(text)) # VCorpus compatible with n-gram analysis

# Unigram
frequencies <- DocumentTermMatrix(corp)

# Remove these words that are not used very often. Keep terms that appear in 1% or more of the dataset
sparse <- removeSparseTerms(frequencies, 0.99)

# rowTotals <- apply(sparse,1,sum) # Find the sum of words in each Document
# dtm.new   <- dtm[rowTotals> 0, ] # remove all docs without words

ui <- unique(sparse$i)
sparse.new <- sparse[ui,]

gc() # garbage collector

#########################################################################
# #  Create document-term-matrix
# dtm <- CreateDtm(text,
#                  doc_names = c(1:length(text)),
#                  ngram_window = c(1, 1),
#                  lower = FALSE,
#                  remove_punctuation = FALSE,
#                  remove_numbers = FALSE)
# 
# gc() # garbage collector
# 
# rowTotals <- rowSums(dtm)        # Find the sum of words in each Document
# dtm.new   <- dtm[rowTotals> 0, ] # remove all docs without words

# # Faster way to remove 0s in dtm
# ui <- unique(dtm$i)
# dtm.new <- dtm[ui,]
#########################################################################

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
load('~/GitHub/NextBigCrypto-Senti/Models/LDA_BTC_v3_2018-03-03.RData')

# ==> Best for 
# BCH is 6
# ETH is 9
# BTC is 7
# XRP is 12
# LTC is 11

k = 7

# sparse dtm func
df_lda <- topicmodels::LDA(sparse.new, k, control = list(seed = 1234))

# normal dtm func
df_lda <- topicmodels::LDA(dtm.new, k, control = list(seed = 1234))
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
