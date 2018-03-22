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
              "ggplot2" # plotting package
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

#Input data
df <- as.data.frame(read_csv('1. Crawlers/1b. Report/4_$BCH_FULL.csv',
                             locale = locale(encoding = 'latin1')))

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
df$proccessed <- sapply(df$text, function(x) trueunicode.hack(x))
df$proccessed <- sapply(df$proccessed, function(x) conv_fun(x)) # convert to delete emojis
df$proccessed <- sapply(df$proccessed, function(x) removeURL(x)) # remove URL

# will remove stopwords later in dtm step
# df$proccessed <- sapply(df$proccessed, function(x) removeWords(x,stopwords("english"))) 

df$proccessed <- sapply(df$proccessed, function(x) AposToSpace(x)) 
df$proccessed <- sapply(df$proccessed, function(x) stripWhitespace(x))

# remove whitespace before & after
df$proccessed <- sapply(df$proccessed, function(x) gsub("^[[:space:]]+", "",x))
df$proccessed  <- sapply(df$proccessed, function(x) gsub("[[:space:]]+$", "",x))

# no need to remove punctuations
#df$proccessed <- sapply(df$proccessed, function(x) removePunctuation(x))

# Remove duplicates (after rmv url)
df <- df[!duplicated(df$proccessed),]
# Remove blanks
df <- df[!(is.na(df$proccessed) | df$proccessed==""), ]
# Remove <f0>
df$proccessed <- gsub("<f0>", "", df$proccessed)
###################################################################

#backup
bk <- df
test <- df$proccessed
str(test)

dtm <- CreateDtm(test,
                 doc_names = c(1:length(test)),
                 ngram_window = c(1, 1),
                 lower = FALSE,
                 remove_punctuation = FALSE,
                 remove_numbers = FALSE,
                 stopword_vec = c(tm::stopwords('english'), tm::stopwords('SMART')))

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

# ==> Best for BCH is range from 9-11
# ETH is 8
# BTC is 8
# LTC is 7
# reuse dtm.new from RData LDATune

df$proccessed
dtm.new


df_lda <- LDA(dtm.new, k = 11, control = list(seed = 1234))
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
df_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

save.image(paste0('LDA_BCH_Tuned_',Sys.Date(),'.RData'))

#load(paste0('LDA_LTC_Tuned_',Sys.Date(),'.RData'))
