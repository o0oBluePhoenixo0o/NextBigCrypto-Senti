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
df <- as.data.frame(read_csv('1. Crawlers/1b. Report/1_$BTC_FULL.csv',
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
df$proccessed <- sapply(df$proccessed, function(x) removeWords(x,stopwords("english"))) 

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

# Alr know result (28.02.2018)
# dtm <- CreateDtm(test,
#                  doc_names = c(1:length(test)),
#                  ngram_window = c(1, 1),
#                  lower = FALSE,
#                  remove_punctuation = FALSE,
#                  remove_numbers = FALSE,
#                  stopword_vec = c(tm::stopwords('english'), tm::stopwords('SMART')))
# 
# rowTotals <- rowSums(dtm) #Find the sum of words in each Document
# dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words
# 
# result <- FindTopicsNumber(
#   dtm.new,
#   topics = seq(from = 2, to = 20, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 77),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# ############################################################
# # minimization for Arun and Cao Juan
# # maximization for Griffiths and Deveaud
# FindTopicsNumber_plot(result)
# 
# result

# ==> Best for 
# BCH is 9-11
# ETH is 8
# BTC is 8
# LTC is 7
# reuse dtm.new from RData LDATune

# tokenize on space and output as a list:
doc.list <- strsplit(df$proccessed, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents
W <- length(vocab)  # number of terms in the vocab 
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
N <- sum(doc.length)  # total number of tokens in the data
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus


# MCMC and model tuning parameters:
K <- 8 # topic number
G <- 10000
alpha <- 0.02
eta <- 0.02

# Fit the model:
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
datasetReviews <- list(phi = phi,
                       theta = theta,
                       doc.length = doc.length,
                       vocab = vocab,
                       term.frequency = term.frequency)


# create the JSON object to feed the visualization:
json <- createJSON(phi = datasetReviews$phi, 
                   theta = datasetReviews$theta, 
                   doc.length = datasetReviews$doc.length, 
                   vocab = datasetReviews$vocab, 
                   term.frequency = datasetReviews$term.frequency)
# display visualization
serVis(json, out.dir = 'vi', open.browser = TRUE)

#get topic list
topic_list<- top.topic.words(fit$topics, num.words = 10, by.score = FALSE)
topic_list_LDA<- t(topic_list)

topic_list_LDA<- as.data.frame(topic_list_LDA)
topic_list_LDA<-paste(topic_list_LDA$V1,topic_list_LDA$V2,topic_list_LDA$V3,topic_list_LDA$V4,topic_list_LDA$V5,
                      topic_list_LDA$V6,topic_list_LDA$V7,topic_list_LDA$V8, topic_list_LDA$V9, topic_list_LDA$V30, sep = ', ')
topic_list_LDA<- as.data.frame(topic_list_LDA)
colnames(topic_list_LDA)<- 'Terms'

Topic<- as.data.frame(list(1:dim.data.frame(topic_list_LDA)[1]))
colnames(Topic)<- "TopicID"
topic_list_LDA<- cbind(Topic,topic_list_LDA)

######################################


# export topic list
write.csv(topic_list_LDA, file = paste0('topic_list_LDA_BTC_',Sys.Date(),'.csv'), quote = TRUE, sep= ",",
          row.names=FALSE, qmethod='escape',
          fileEncoding = "UTF-8", na = "NA")

###################################### Assign Topics Back to each Post##################################

# get weighted topic-post assignment matrix
predicDoc<- slda.predict.docsums(documents=documents, 
                                 fit$topics, 
                                 alpha, 
                                 eta, 
                                 num.iterations = 10000, 
                                 average.iterations = 5000, 
                                 trace = 0L)

#get top1 topic for each post
getTopTopic<- function(predictDoc){
  
  t<- list()
  for(i in 1:dim(predictDoc)[2]){
    
    for(j in 1:dim(predictDoc)[1]){
      
      if(predicDoc[j,i]==max(predictDoc[,i])){
        
        t<- rbind(t,j)
      }
    }
  }
  
  return(t)
}

# apply the function and get topic assignment list
topic<- getTopTopic(predicDoc)

topic<- as.data.frame(topic)

# Combine the post dataset with topic
df_Topic<- bind_cols(df, topic)
colnames(df)[4]<- "TopicID"
# link TopicID with topic Content
df_Topic_Final<- merge(x = df_Topic, y = topic_list_LDA, by = "TopicID", all.x = TRUE)


save.image(file = paste0('LDA_BTC_v3_',Sys.Date(),'.RData'))