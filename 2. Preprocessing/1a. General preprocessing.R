#Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti/")

# temporary WiPool
#setwd("E:/NextBigCrypto-Senti")

# Clear environment
rm(list = ls())
# install packages if not available
packages <- c("dplyr","SnowballC","stringr","qdap","memisc",
              "smbinning", # supervised discretization
              "infotheo","data.table","tm")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

#==================================================================
# Convert epoch time
unix2POSIXct <- function(time) structure(time, class = c("POSIXt", "POSIXct"))

####################################################
# Reddit data---------------------------------------
####################################################
clean_reddit <- function(df){
  
  df <- df[ , -which(names(df) %in% c("X","V1"))]
  df <- unique(df)
  
  # Clear all NULL columns 
  df <- df[,colSums(is.na(df))<nrow(df)]
  # Add created_at column (human intepretation date time)
  df$created_at <- unix2POSIXct(df$created_utc)
  df$edited_at <- unix2POSIXct(df$edited)
  
  # Preprocessing ------------------------------------
  
  # Extract subset for EDA
  processed.df <- df[,c('id','created_at',
                                   'title','is_self','selftext',
                                   'link_flair_css_class','link_flair_text','num_comments','score','domain',
                                   'author','author_flair_css_class', #only author classes are relevant
                                   'full_link','url')]
  
  # Extract only non-NA data
  processed.df <- processed.df[complete.cases(processed.df),]
  
  # Clean data 
  # link_flair css -----------------------------
  processed.df$link_flair_css_class[tolower(processed.df$link_flair_css_class) %in% c('general discussion','discussion')] <- 'gdiscussion'
  
  # Delete "mod" flair class data
  processed.df <- processed.df[!(substr(processed.df$link_flair_css_class,0,3) =="mod"),]
  
  # turn all flairs into lower case
  processed.df$link_flair_css_class <- tolower(processed.df$link_flair_css_class)
  processed.df$link_flair_text <- tolower(processed.df$link_flair_text)
  
  # trip whitespace leading/trailing in author flairs
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  processed.df$author_flair_css_class <- trim(processed.df$author_flair_css_class)
  
  return(processed.df)
}

crypto.df <- readr::read_csv("~/GitHub/NextBigCrypto-Senti/1. Crawlers/2b. Reddit Report/Crypto_Reddit.csv")
crypto_market.df <- readr::read_csv("~/GitHub/NextBigCrypto-Senti/1. Crawlers/2b. Reddit Report/CryptoMarkets_Reddit.csv")

processed.crypto <- clean_reddit(crypto.df)
processed_market.crypto <- clean_reddit(crypto_market.df)

#======================================================================

####################################################
# Twitter data---------------------------------------
####################################################

# Test with ETH
ticker <- '$ETH'
path = '1. Crawlers/1b. Report/'

i <- 2
df_path <- paste0(path,i,'_',ticker,'_','FULL','.csv')
df <- fread(df_path)

txt_extract <- df[,5:6]


loadAbbrev <- function(filename) {
  # Concates custom abbreviation dataset with the default one from qdap
  #
  # Args:
  #   filename: Filename of the abbreviation lexicon
  #
  # Returns:
  #   A 2-column(abv,rep) data.frame
  
  myAbbrevs <- read.csv(filename, sep = ",", as.is = TRUE)
  return(rbind(abbreviations,myAbbrevs))
}

# Use the file "5.Dataset/abbrev.csv"
myAbbrevs <- loadAbbrev('0. Datasets/abbrev.csv')

convertAbbreviations <- function(message){
  # Replaces abbreviation with the corresporending long form
  #
  # Args:
  #   text: Text to remove the abbreviations from
  #
  # Returns:
  #   String
  if(is.na(message) || message == ""){
    return(message)
  } else {
    newText <- message
    for (i in 1:nrow(myAbbrevs)){
      newText <- gsub(paste0('\\<', myAbbrevs[[i,1]], '\\>'), paste(myAbbrevs[[i,2]]), newText)
    }
    return (newText)
  }
}

#######################################################
# Preprocessing the dataframe and cleaning the corpus #
#######################################################

# Function for taking in the vector from TW_df data set and do all preprocessing steps below:
# preprocessing steps- Case folding; Remove numbers, URLs, words 
# and punctuation and perform stemming and stripping extra whitespaces

conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")
removeURL <- function(x) gsub('"(http.*) |(http.*)$|\n', "", x)

# vec2clean.corp function takes two arguments: x = vector to be cleaned
vec2clean.corp <- function(x){
  
  # As there are many languages used in the data, we consider stopwords of all the languages
  a = c(stopwords("danish"),stopwords("dutch"),stopwords("english"),
        stopwords("finnish"),stopwords("french"), stopwords('SMART'),
        stopwords("german"),stopwords("hungarian"),stopwords("italian"),
        stopwords("norwegian"),stopwords("portuguese"),stopwords("russian"),
        stopwords("spanish"),stopwords("swedish"))
  
  # Function to replace ' and " to spaces before removing punctuation to avoid different words from binding 
  AposToSpace = function(x){
    x= gsub("'", ' ', x)
    x= gsub('"', ' ', x)
    x =gsub('break','broke',x) # break may interrupt control flow in few functions
    return(x)
  }
  
  x = Corpus(VectorSource(x))
  x = tm_map(x, conv_fun)
  x = tm_map(x, removeURL)
  x = tm_map(x, tolower)
  x = tm_map(x, removeNumbers)
  x = tm_map(x, removeWords, a)
  x = tm_map(x, AposToSpace)
  x = tm_map(x, removePunctuation)
  x = tm_map(x, stemDocument)
  x = tm_map(x, stripWhitespace)
  
  return(x)
}

# Calling the vec2clean.corp with TW_df(x)

corp <- vec2clean.corp(txt_extract$text)

# Extract frequent terms
frequencies <- DocumentTermMatrix(corp)
findFreqTerms(frequencies, lowfreq = 20)

# Remove these words that are not used very often. Keep terms that appear in 0.5% or more of tweets.
sparse <- removeSparseTerms(frequencies, 0.995)

tweetsSparse <- as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))

#######################################################################
# REDDIT PREPROCESSING CLEAN DATA 12.06.2018
#######################################################################


comments.df <- readr::read_csv('~/GitHub/NextBigCrypto-Senti/1. Crawlers/2b. Reddit Report/Crypto_Reddit_comments.csv')
Cleandata.RD <- function(df) {
  # remove duplicates base on contents
  df <- df[!duplicated(df$com_content),]
  
  df$processed <- sapply(df$com_content, function(x) removeURL(x)) # remove URL
  # To lower case
  df$processed <- sapply(df$processed, function(x) tolower(x))
  df$processed <- sapply(df$processed, function(x) gsub("[.,]"," ", x, perl = TRUE)) #remove . and ,
  
  
  # Remove duplicates 
  df <- df[!duplicated(df$processed),]
  
  # converting abbreviations
  df$processed <- sapply(df$processed, function(x) convertAbbreviations(x))
  df$processed <- sapply(df$processed, function(x) conv_fun(x)) # convert to delete emojis
  df$processed <- sapply(df$processed, function(x) gsub("[\r\n]", " ", x)) #change /r /n break lines into space
  
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
  df$processed <- str_replace_all(df$processed,"@[a-z,A-Z,_]*"," ")  
  
  # remove punctuations
  df$processed <- sapply(df$processed, function(x) gsub( "[^a-zA-Z\\s]" , "" , x , perl = TRUE ))
  
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
  
  # Remove duplicates 
  df <- df[!duplicated(df$processed),]
  
  # Lemmatization 26.04.18
  df$processed <- sapply(df$processed, function(x) textstem::lemmatize_strings(x))
  
  return(df)
}
comments.raw <- split(comments.df, (seq(nrow(comments.df))-1) %/% 50000) 
#comments.clean <- Cleandata(comments.df)
# 
# as.data.frame(comments.raw[['0']])
# final.clean <- comments.df[0,]


for (i in 1:length(comments.raw)){
  df <- as.data.frame(comments.raw[[i]])
  clean.df <- Cleandata.RD(df)
  final.clean <- bind_rows(final.clean,clean.df)
  print(paste0('Complete batch ',i,'/',length(comments.raw)))
  write_csv(final.clean,'~/GitHub/NextBigCrypto-Senti/0. Datasets/Reddit.cleanBK.csv')
}
# double check the left-over
not.done <- anti_join(comments.df,final.clean, by = 'post_id')
clean.not.done <- Cleandata.RD(not.done)
final.clean <- bind_rows(final.clean,clean.not.done)


# EDA
# link_flair_all <- dplyr::summarize(dplyr::group_by(processed.reddit,link_flair_text,link_flair_css_class),n())
# link_flair_css <- dplyr::summarize(dplyr::group_by(processed.reddit,link_flair_css_class),n())
# link_flair_text <- dplyr::summarize(dplyr::group_by(processed.reddit,link_flair_text),n())
# 
# num_com <- dplyr::summarize(dplyr::group_by(processed.reddit,num_comments),n())
# num_scores <- dplyr::summarize(dplyr::group_by(processed.reddit,score),n())
# 
# # EDA author flairs
# author_flair_css <- dplyr::summarize(dplyr::group_by(processed.reddit,author_flair_css_class),n())
# 
# # ---------------------------------
# # Binning Scores (for later classification results comparison)
# # For scores
# scores <- processed.reddit[,which(names(processed.reddit) %in% c("id","score"))]
# 
# scores_binning <- discretize(scores$score,
#                              disc = "equalfreq",
#                              10)
# scores_binning <- cbind(scores,scores_binning)
# 
# # For No. of comments
# comments <- processed.reddit[,which(names(processed.reddit) %in% c("id","num_comments"))]
# 
# comments_binning <- discretize(comments$num_comments,
#                                disc = "equalfreq",
#                                10)
# comments_binning <- cbind(comments,comments_binning)