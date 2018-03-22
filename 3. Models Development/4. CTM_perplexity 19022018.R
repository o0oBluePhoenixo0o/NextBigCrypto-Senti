
#Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti/")
# clear the environment
rm(list= ls())
# temporary WiPool
#setwd("E:/NextBigCrypto-Senti")

# install packages if not available
packages <- c("topicmodels","tidytext","dplyr", #data manipulation
              "stringr",#string manipulation
              "lubridate", #datetime
              "tidyr","purrr","broom",
              "data.table","readr",
              "SnowballC", #stemming
              "RColorBrewer",
              "ggplot2","scales" # for plotting stuff
)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

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

# Test with ETH
eth_df <- as.data.frame(read_csv('1. Crawlers/1b. Report/2_$ETH_FULL.csv',
                                 locale = locale(encoding = 'latin1')))

eth_df <- eth_df[,which(colnames(eth_df) %in% c('created_at','text'))] %>%
  mutate(timestamp = ymd_hms(created_at))

# Convert unicode
eth_df$text <- sapply(eth_df$text,trueunicode.hack)

# remove duplicates base on tweets
eth_df <- eth_df[!duplicated(eth_df$text),]

#######################################################
# Preprocessing the dataframe and cleaning the corpus #
#######################################################

# Function for taking in the vector from TW_df data set and do all preprocessing steps below:
# preprocessing steps- Case folding; Remove numbers, URLs, words 
# and punctuation and perform stemming and stripping extra whitespaces

conv_fun <- function(x) iconv(x, "latin1", "ASCII", "byte")
removeURL <- function(x) gsub('"(http.*) |(https.*) |(http.*)$|\n', "", x)

# Function to replace ' and " to spaces before removing punctuation to avoid different words from binding 
AposToSpace = function(x){
  x= gsub("'", ' ', x)
  x= gsub('"', ' ', x)
  x =gsub('break','broke',x) # break may interrupt control flow in few functions
  return(x)
}

######################################################################################

clean <- function (sentence){
  a = c(stopwords("danish"),stopwords("dutch"),stopwords("english"),
        stopwords("finnish"),stopwords("french"), stopwords('SMART'),
        stopwords("german"),stopwords("hungarian"),stopwords("italian"),
        stopwords("norwegian"),stopwords("portuguese"),stopwords("russian"),
        stopwords("spanish"),stopwords("swedish"))

  sentence <- removeURL(sentence)
  sentence <- removeWords(sentence,a) #remove stopwords
  sentence <- AposToSpace(sentence)
  sentence <- stripWhitespace(sentence) # delete whitespace
  sentence <- removePunctuation(sentence) 
  return(sentence)
}

eth_df$text <- sapply(eth_df$text, function(x) clean(x))

###############################################################################
# Creating vocabulary and document-term matrix ###################################################
prepare_train_dtm <- unlist(as.data.frame(eth_df$text))

# Define preprocessing function and tokenization function
# prep_fun <- tolower
tok_fun <- text2vec::word_tokenizer

# Define preprocessing function and tokenization function
it_train <- itoken(as.character(prepare_train_dtm), 
                   # preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   progressbar = TRUE)

# Creating vocabulary and document-term matrix
vocab_train <- create_vocabulary(it_train)
vectorizer_train <- vocab_vectorizer(vocab_train)
train_dtm <- create_dtm(it_train, vectorizer_train)

# Define tfidf model
tfidf <- TfIdf$new()
# Change the document term matrix as tf-idf matrix
train_dtm_tfidf <- fit_transform(train_dtm, tfidf)
# Change tf-idf matrix to fit the model
train_dtm_tfidf@x <- ceiling(train_dtm_tfidf@x)

# Find best value for k ################################################################
# Function for find the best k vaule
bestK <- function(ap_dtm = dtm)
{
  perplexity_CTM = NULL
  for(i in 1:10)
  {
    models <- list(
      ap_CTM       = CTM(ap_dtm, k = 10*i, 
                         control = list(estimate.beta = TRUE,
                                        verbose = 1,
                                        prefix = tempfile(),
                                        save = 0,
                                        keep = 0,
                                        seed = as.integer(Sys.time()), 
                                        nstart = 1L, 
                                        best = TRUE,
                                        var = list(iter.max = 50, tol = 10^-5), 
                                        em = list(iter.max = 100, tol = 10^-3),
                                        initialize = "random",
                                        cg = list(iter.max = 1000, tol = 10^-4)
                         )
      )
    )
    perplexity_CTM <- c(perplexity_CTM, perplexity(models))
  }
  return(perplexity_CTM)
}


# Apply function
k_perplexity <- bestK(train_dtm_tfidf)

# Show all perplexity for each K here
k_perplexity

# Set values for parameters in the topic model #########################################
# Prepare for topic modeling

# Find the k that can achieve the smallest perplexity
k = which.min(k_perplexity)

# The best K
k

# Clustering words ##################################################################
disMatrix <- dist(scale(tdm_clustering))
fit <- hclust(disMatrix,method="ward.D")


# Make a plot
plot(fit)


# Cut tree into 10 clusters
(groups <- cutree(fit,k=16))