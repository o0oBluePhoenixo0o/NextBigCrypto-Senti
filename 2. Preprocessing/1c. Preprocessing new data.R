# Preprocessing code for additional new data (all currencies)
# 1. Clean dataset
# 2. Apply sentiment trained model 
# 3. Apply sentiment pakacge modle
# 4. LDA on cleaned dataset
# 5. Apply Predefined-Topic 

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
              "stringr",
              "tm","openxlsx","qdapRegex","qdap","NLP","openNLP","h2o"
)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

#Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti")

#######################################################
# Load dataset base on token
symbol <- 'ETH'

# Read in coin list as Oct 17
coins_list <- read.csv("~/GitHub/NextBigCrypto-Senti/1. Crawlers/Top50_Oct7.csv")
position <- match(symbol, coins_list$symbol) # get position in queue
files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/0. Datasets/Cleaned/',
                    pattern = paste0('^',symbol,'_'))
# load "cleaned" dataset
df.clean <- read_csv(paste0('~/GitHub/NextBigCrypto-Senti/0. Datasets/Cleaned/',files),
               locale = locale(encoding = 'latin1'))
df.clean$status_id <- as.character(df.clean$status_id)
df.clean$user_id <- as.character(df.clean$user_id)

files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/',
                    pattern = paste0('^',position,'_'))
# Load full dataset
df.full <- read_csv(paste0('~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/',files),
                    locale = locale(encoding = 'latin1')) %>%
  dplyr::select(created_at, status_id,user_id, screen_name, text)

df.full$status_id <- as.character(df.full$status_id)
df.full$user_id <- as.character(df.full$user_id)
gc()

source('~/GitHub/NextBigCrypto-Senti/2. Preprocessing/1. Preprocessing_TW.R')

# check if df.clean already existed
if (exists("df.clean") == FALSE){df.new <- df.full}
if (exists("df.clean") == TRUE){df.new <- anti_join(df.full,df.clean, by = "status_id")}

# # filter base on clean version created_at date
# df.new <- df.new %>%
#   filter(created_at >= max(df.clean$created_at))

# Clean data
df.new <- Cleandata(df.new)
if (exists("df.clean") == FALSE){df.final <- df.new}
if (exists("df.clean") == TRUE){df.final <- rbind(df.clean,df.new)}
gc()

# save new.clean
write_csv(df.final,paste0('~/GitHub/NextBigCrypto-Senti/0. Datasets/',symbol,'_clean_',Sys.Date(),'.csv'))

#########################################
# 2. Senti.Trained for df.final         #
#########################################

rm(list=ls()[! ls() %in% c("df.clean","symbol")])

#df.senti.trained <- read_csv('~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/BTC_clean_senti_trained_0706.csv',
#                              locale = locale(encoding = 'latin1'))

files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',
                    pattern = paste0('^',symbol,'_clean_senti_trained_'))
df.senti.trained <- read_csv(paste0('~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',files),
                         locale = locale(encoding = 'latin1'))

gc()
# Check if already have senti trained df
if (exists("df.senti.trained") == TRUE){df.senti.new <- anti_join(df.clean,df.senti.trained, by = 'status_id')}
if (exists("df.senti.trained") == FALSE){df.senti.new <- df.clean}

df.senti.new$status_id <- as.character(df.senti.new$status_id)

# Apply algo on new batch of data
# Using best model so far (GBM-tuned 080518)
h2o.init()
gbm.model.opt <- h2o.loadModel('./Models/H2O/final_grid_model_1')

# Implement batch-running (in process)
i <- 1
j <- 50000

while (i <= j) {
  # Preprocessing
  corp <- VCorpus(VectorSource(df.clean$processed[i:j])) # VCorpus compatible with n-gram analysis
  # Unigram
  frequencies <- DocumentTermMatrix(corp,
                                    control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  # Remove these words that are not used very often. Keep terms that appear in 0.5% or more of the dataset
  sparse <- removeSparseTerms(frequencies, 0.995)

  # Generate sparse matrix
  ResultSparse <- as.data.frame(as.matrix(sparse))
  colnames(ResultSparse) <- make.names(colnames(ResultSparse))

  # Convert to H2O format
  fullH2O <- as.h2o(ResultSparse)

  # Prediction by batches
  predictionsGBM.opt <- as.data.frame(h2o.predict(gbm.model.opt,fullH2O))

  if (i==1){df.senti <- cbind(df.clean[i:j,], sentiment.trained = predictionsGBM.opt$predict)}
  if (i!=1){df.senti <- rbind(df.senti,cbind(df.clean[i:j,],sentiment.trained = predictionsGBM.opt$predict))}

  print(paste0('Complete from ',i,' to ',j,'/',nrow(df.clean),' observations!'))
  # increase i and j
  i <- i + 50000
  ifelse((j + 50000) <= nrow(df.clean),j <- j + 50000, j <- nrow(df.clean))

  gc()
}
h2o.shutdown()

df.senti.trained.new <- df.senti
# Rename new senti trained to match the previous
df.senti.trained.new <- df.senti.trained.new %>%
  select(created_at, status_id, user_id, screen_name, text,
         processed, botprob, sentiment.trained) %>%
  dplyr::rename(date = created_at)

if (exists("df.senti.trained") == TRUE){df.senti.trained.final <- rbind(df.senti.trained,df.senti.trained.new)}
if (exists("df.senti.trained") == FALSE){df.senti.trained.final <- df.senti.trained.new}

# Save final file
write_csv(df.senti.trained.final, paste0('~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',symbol,'_clean_senti_trained_',Sys.Date(),'.csv'))


#####################################
# 3. Senti.Packages for df.final    #
#####################################

rm(list=ls()[! ls() %in% c('df.clean','symbol')])

# df.senti.pkg <- read_csv('~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/BTC_clean_senti_pkg_0706.csv',
#                               locale = locale(encoding = 'latin1'))

files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',
                    pattern = paste0('^',symbol,'_clean_senti_pkg_'))
df.senti.pkg <- read_csv(paste0('~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',files),
                         locale = locale(encoding = 'latin1'))

df.senti.pkg$status_id <- as.character(df.senti.pkg$status_id)
gc()

# Check if already have senti trained df
if (exists("df.senti.pkg") == TRUE){df.senti.new <- anti_join(df.clean,df.senti.pkg, by = 'status_id')}
if (exists("df.senti.pkg") == FALSE){df.senti.new <- df.clean}

df.senti.new$status_id <- as.character(df.senti.new$status_id)

# keep only relevant columns
df.senti.new <- df.senti.new %>%
  select(created_at, status_id, user_id, screen_name, text) %>%
  dplyr::rename(date = created_at)

source('~/GitHub/NextBigCrypto-Senti/2. Preprocessing/1. Preprocessing_TW.R')

# BingLiu Lexicon - best model
# Pulling in positive and negative wordlists
# BingLiu
pos.words <- scan('~/GitHub/NextBigCrypto-Senti/0. Datasets/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
neg.words <- scan('~/GitHub/NextBigCrypto-Senti/0. Datasets/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
#Adding words to positive and negative databases
pos.words=c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'thx' ,
            'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader','pump',
            'rocket','ath','bullish','bull','undervalued')
neg.words = c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not',
              'FUD','FOMO','bearish','dump','fear','atl','bear','wth','shit','fuck','dumb',
              'weakhands','blood','bloody','scam','con-artist','liar','vaporware','shitfork')

#evaluation function
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  scores <- plyr::laply(sentences, function(sentence, pos.words, neg.words){
    # unicode conversion
    sentence <- trueunicode.hack(sentence)
    sentence <- gsub("[.,]"," ", sentence, perl = TRUE) #remove . and ,
    # remove screen name
    sentences <- str_replace_all(sentence,"@[a-z,A-Z,_]*"," ") 
    # convert abbreviation
    sentence <- convertAbbreviations(sentence)
    sentence <- gsub("[\r\n]", " ", sentence) # fix line breakers
    
    #convert to lower-case and remove punctuations with numbers
    sentence <- gsub( "[^#$a-zA-Z\\s]" , "" , sentence , perl = TRUE ) #remove punc except $
    sentence <- removeNumbers(tolower(sentence))
    removeURL <- function(x) rm_url(x, pattern=pastex("@rm_twitter_url", "@rm_url"))
    sentence <- removeURL(sentence)
    
    # split into words. str_split is in the stringr package
    word.list <- str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    words <- unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, message=sentences)
  return(scores.df)
}

scores <- score.sentiment(df.senti.new$text, pos.words, neg.words, .progress='text')
result <- scores

#Add ID to result set
result$status_id <- as.character(df.senti.new$status_id)
#add new scores as a column
result <- mutate(result, status_id, sentiment.packages = ifelse(result$score > 0, 1, 
                                                                ifelse(result$score < 0, -1, 0)))%>% 
  dplyr::select(status_id, sentiment.packages)

# Merge to get final sentiment dataset
df.senti.pkg.new <- inner_join(df.senti.new, result, by = 'status_id')

if (exists("df.senti.pkg") == FALSE){df.senti.pkg.final <- df.senti.pkg.new}
if (exists("df.senti.pkg") == TRUE){df.senti.pkg.final <- rbind(df.senti.pkg,df.senti.pkg.new)}

# Save final file
write_csv(df.senti.pkg.final, paste0('~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',symbol,'_clean_senti_pkg_',Sys.Date(),'.csv'))

####################################
#       4. LDA models              #
####################################

rm(list=ls()[! ls() %in% c('df.clean','symbol')])

# load LDA base on token
files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/Models/',
                    pattern = paste0('^',symbol,'_LDA'))
load(paste0('~/GitHub/NextBigCrypto-Senti/Models/',files))

# Batch processing
# Implement batch-running (in process)
df.clean$topic <- NA

# Split df into segments for lda[ing]
df.clean.split <- split(df.clean, (seq(nrow(df.clean))-1) %/% 10000)
df.LDA <- df.clean[0,]

for (i in 1:length(df.clean.split)){
  # Preprocessing
  corp <- VCorpus(VectorSource(df.clean.split[[i]]$processed)) # VCorpus compatible with n-gram analysis
  # Unigram
  frequencies <- DocumentTermMatrix(corp)
  # no need to remove sparse
  ui <- unique(frequencies$i)
  # backup for merging later
  sparse.new <- frequencies[ui,]
  test <- df.clean.split[[i]][ui,]

  # Apply LDA on batch
  predict.LDA <- topicmodels::posterior(df_lda,sparse.new)
  top_topic_per_doc <- apply(predict.LDA$topics, 1, which.max)

  #### Reassign topics back to main df
  for (k in 1:length(top_topic_per_doc)){
    test$topic[k] <- top_topic_per_doc[k]
  }
  # Merge for final dataset
  df.LDA <- rbind(df.LDA,test)

  print(paste0('Completed LDA for batch ',i,'/',length(df.clean.split)))
  gc()
}

# save LDA file
write_csv(df.LDA,paste0('~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',symbol,'_clean_LDA_',Sys.Date(),'.csv'))

#########################################
#    5. Apply predefined-topics         #
#########################################

rm(list=ls()[! ls() %in% c('df.clean','symbol')])
topic <- NA
df.PD <- cbind(df.clean,topic)

# only need stemmed version of text
df.PD$stem <- stemDocument(tolower(df.PD$text))
####
# Topic creation
####
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
addtopic <- function(df, topicdf, topicid){
  
  #kwic detect match
  topic.result <- quanteda::kwic(df$processed, stemDocument(tolower(topicdf)))
  topic.result <- as.data.frame(topic.result)
  topic.result$docid <- substr(topic.result$docname,5,stri_length(topic.result$docname))
  
  # extract document id from topic results
  topic.list <- as.numeric(topic.result$docid)
  print('Done extraction... now merging...')
  
  # fill correct topic to df
  for (i in topic.list){
    df[i,which(colnames(df)==paste0("topic",topicid))] <- 1
  }
  return(df)
}

# Create stem document column for main dataset
df.PD$topic1 <- 0
df.PD$topic2 <- 0
df.PD$topic3 <- 0
df.PD$topic4 <- 0
df.PD$topic5 <- 0
df.PD$topic6 <- 0
df.PD$topic7 <- 0
df.PD$topic8 <- 0
df.PD$topic9 <- 0
df.PD$topic10 <- 0

# Add topicid to df.PD
df.PD <- addtopic(df.PD,topic1.regulation,'1')
df.PD <- addtopic(df.PD,topic2.TA,'2')
df.PD <- addtopic(df.PD,topic3.ICO,'3')
df.PD <- addtopic(df.PD,topic4.incidents,'4')
df.PD <- addtopic(df.PD,topic5.trading,'5')
df.PD <- addtopic(df.PD,topic6.exchanges,'6')
df.PD <- addtopic(df.PD,topic7.mainstream,'7')
df.PD <- addtopic(df.PD,topic8.project,'8')
df.PD <- addtopic(df.PD,topic9.technology,'9')
df.PD <- addtopic(df.PD,topic10.mining,'10')

bk <- df.PD

# save PD (temp) file
write_csv(df.PD,paste0('~/GitHub/NextBigCrypto-Senti/0. Datasets/SentiTopic/',symbol,'_clean_PD_',Sys.Date(),'.csv'))

