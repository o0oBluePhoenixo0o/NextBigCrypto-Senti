# Harvest Reddit data from Google BigQuery

# clear the environment
rm(list= ls())

# load packages and set options
options(stringsAsFactors = FALSE)

#Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti/")

# install packages if not available
packages <- c("readr", #read data
              "lubridate", #date time conversion
              "dplyr", #date manipulation
              "tm", # text mining package
              "textmineR",
              "tidytext",
              "ggplot2", # plotting package
              "quanteda", #kwic function search phrases
              "ggthemr", "magrittr", "bigrquery",
              "stringi", #string manipulation
              "wordcloud","tidyquant",
              "caTools","caret", "rpart", "h2o","e1071","RWeka",
              "randomForest"
)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

## follow instructions here https://github.com/r-dbi/bigrquery
## to set this parameter correctly
billing <- '...'

## Google BigQuery has kindly archived this data for us
## For newer data you will have to learn to scrape reddit yourself
months <- c(
  "2017_03","2017_04","2017_05","2017_06","2017_07","2017_08",
  "2017_09","2017_10","2017_11","2017_12","2018_01","2018_02"
)
reddits <- c(
  "Bitcoin", 
  "ethereum", 
  "CryptoCurrency", 
  "0xProject", 
  "omise_go", 
  "Tronix", 
  "Ripple", 
  "Iota", 
  "nanocurrency",
  "vergecurrency", 
  "Monero", 
  "eos", 
  "litecoin", 
  "Stellar", 
  "cardano", 
  "NEO", 
  "btc", 
  "helloicon", 
  "ArkEcosystem",
  "siacoin", 
  "RaiBlocks", 
  "EthereumClassic", 
  "dashpay", 
  "nem", 
  "EtherDelta"
)

all_data <- lapply(reddits, function(r) {
  filename <- paste0("~/Desktop/", r, "_reddit.rds")
  all_btc <- lapply(months, function(m) {
    table <- paste0("fh-bigquery.reddit_comments.", m)
    sql <- paste0(
      "SELECT body, author, score, created_utc FROM `", table, "` WHERE subreddit='", r, "' ORDER BY created_utc ASC"
    )
    tb <- bq_project_query(billing, sql)
    bq_table_download(tb)
  })
  
  btc <- dplyr::bind_rows(all_btc)
  sent <- get_sentences(btc$body) %>% sentiment_by
  btc$sentiment <- sent$ave_sentiment
  btc$word_count <- sent$word_count
  btc$reddit <- if (r == "RaiBlocks") { "nanocurrency" } else { r } #rebranding...
  btc
})

big_df <- dplyr::bind_rows(all_data)
big_df$created_utc %<>% as.POSIXct(., origin = "1970-01-01", tz = "UTC")
big_df$sentiment_type <- sapply(big_df$sentiment, function(x) {
  if (x > 0.6)   return("very positive")
  if (x < -0.6)  return("very negative")
  if (x > 0.3)   return("positive")
  if (x < -0.3)  return("negative")
  return("neutral")
}) %>% factor(., levels = c("very positive", "positive", "neutral", "negative", "very negative"))

reddit_activity <- big_df %>%
  group_by(month = floor_date(created_utc, "month"), reddit = reddit, sentiment_type = sentiment_type) %>%
  summarize(num_words = sum(word_count))


## summary chart
ggthemr_reset()
p <- ggplot(reddit_activity[!reddit_activity$reddit %in% c("Bitcoin", "btc", "CryptoCurrency", "EtherDelta"), ], aes(x = month, y = num_words, fill = sentiment_type)) +
  geom_bar(stat = 'identity') + ggtitle(paste0("Reddit sentiment summary. © Rados.io")) +
  xlab("Month") + scale_y_continuous(name="# Words", labels = scales::comma) +
  guides(fill = guide_legend(title = "Sentiment")) + facet_wrap(~reddit) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(paste0("~/Downloads/", "overall", "_reddit_plot.png"), plot = p, width = 9)

## individual charts
ggthemr("pale")
lapply(reddits, function(r) {
  p <- ggplot(reddit_activity[reddit_activity$reddit==r, ], aes(x = month, y = num_words, fill = sentiment_type)) +
    geom_bar(stat = 'identity') + ggtitle(paste0("Reddit sentiment summary of r/", r, ". © Rados.io")) +
    xlab("Month") + scale_y_continuous(name="# Words", labels = scales::comma) +
    guides(fill = guide_legend(title = "Sentiment"))
  ggsave(paste0("~/Downloads/", r, "_reddit_plot.png"), plot = p, width = 9)
})