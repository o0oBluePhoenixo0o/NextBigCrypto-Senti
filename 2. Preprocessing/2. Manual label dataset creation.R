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
              "data.table",
              "stringi", #string manipulation
              "stringr",
              "tm","openxlsx","qdapRegex","qdap"
)

#remove.packages(c("tidytext", "ggplot2"))
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

# ZIP PATH for dev tools
if(devtools::find_rtools()) Sys.setenv(R_ZIPCMD= file.path(devtools:::get_rtools_path(),"zip"))
library(openxlsx)
###################################################

#Input data
# df <- as.data.frame(read_csv('1. Crawlers/1b. Report/1_$BTC_FULL.csv',
#                              locale = locale(encoding = 'latin1')))
# name.df <- 'BTC'

# Input data (labeling dataset) 30.04.18
# Use new set of manual.df 01.05.18
df <- as.data.frame(read.xlsx('Manual_Dataset_0105_labeling.xlsx')) %>%
  select(status_id, text, sentiment) %>%
  filter(is.na(sentiment) == FALSE)

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

  ###
  #df$processed <- df$text
  df$processed <- sapply(df$processed, function(x) conv_fun(x)) # convert to delete emojis
  df$processed <- sapply(df$processed, function(x) removeURL(x)) # remove URL
  df$processed <- sapply(df$processed, function(x) gsub("[\r\n]", " ", x)) #change /r /n break lines into space
  
  # remove stopwords - create exception lists 25.04
  exceptions   <- c('up','down','all','above','below','under','over',
                    'few','more', 'in')
  # keep negation list
  negations <- grep(pattern = "not|n't", x = stopwords(), value = TRUE)
  exceptions <- c(exceptions,negations)
  
  my_stopwords <- setdiff(stopwords("en"), exceptions)
  
  df$processed <- sapply(df$processed, function(x) removeWords(x,c(my_stopwords))) 
  
  ### 29.04 - v1 is better than v2 with tokens list
  # # Read tokens data
  # coins_list <- read_csv("./1. Crawlers/Crypto-Markets_2018-04-22.csv")
  # 
  # tokens <- tolower(as.vector(unique(coins_list$symbol)))
  # # Add extra "$" in front of token
  # tokens_extra <- paste0("$",tokens)
  # tokens <- c(tokens,tokens_extra)
  # 
  # # Combine tokens to stopword dict
  # my_stopwords <- c(my_stopwords,tokens)
  #############
  
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
  
  # Lemmatization 26.04.18
  df$processed <- sapply(df$processed, function(x) textstem::lemmatize_strings(x))
  
  return(df)
}

##### Main Process

# Constructing abbreviation list

# Abbreviation conversion 30.04.18
myAbbrevs <- read_csv('./0. Datasets/abbrev.csv')

# Convert to lower case
myAbbrevs$abv <- tolower(myAbbrevs$abv)
myAbbrevs$rep <- tolower(myAbbrevs$rep)

# Adding abbreviations
myAbbrevs[nrow(myAbbrevs)+1,1] <- ':)'
myAbbrevs[nrow(myAbbrevs),2] <- 'smile'
myAbbrevs[nrow(myAbbrevs)+1,1] <- '=))'
myAbbrevs[nrow(myAbbrevs),2] <- 'laugh'
myAbbrevs[nrow(myAbbrevs)+1,1] <- '=)'
myAbbrevs[nrow(myAbbrevs),2] <- 'laugh'
myAbbrevs[nrow(myAbbrevs)+1,1] <- ':('
myAbbrevs[nrow(myAbbrevs),2] <- 'sad'
myAbbrevs[nrow(myAbbrevs)+1,1] <- ':(('
myAbbrevs[nrow(myAbbrevs),2] <- 'sad'
myAbbrevs[nrow(myAbbrevs)+1,1] <- ':D'
myAbbrevs[nrow(myAbbrevs),2] <- 'smile'

# No need to do this since tokens will be put behind "$" 01.05.2018

# # Load token list --> get exceptions for not being mistaken as acronyms
# coins_list <- read_csv("./1. Crawlers/Crypto-Markets_2018-04-30.csv")
# 
# tokens <- tolower(as.vector(unique(coins_list$symbol)))
# test <- as.data.frame(setdiff(myAbbrevs$abv,tokens))
# names(test) <- 'abv'
# # Keep only non-token abbreviations
# myAbbrevs <- dplyr::left_join(test,myAbbrevs, by = 'abv')
# rm(test)


# Convert dataframe to dictionary list
t_myAbbrevs <- t(myAbbrevs$rep)
names(t_myAbbrevs) <- myAbbrevs$abv

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
    message_split <- strsplit(message,"\\s")
    for (i in 1:lengths(message_split)){
      try(message_split[[1]][i] <- t_myAbbrevs[[message_split[[1]][i]]],
          silent = TRUE)
    }
    # Remerge list into string
    newText <- paste(unlist(message_split), collapse=' ')
    return (newText)
  }
}

# Convert unicode
df$text <- sapply(df$text,function(x) trueunicode.hack(x))

# remove duplicates base on tweets
df <- df[!duplicated(df$text),]

# To lower case
df$processed <- sapply(df$text, function(x) tolower(x))
df$processed <- sapply(df$processed, function(x) gsub("[.,]","", x, perl = TRUE)) #remove . and ,
# converting abbreviations
df$processed <- sapply(df$processed, function(x) convertAbbreviations(x))

# Clean function
clean.df <- Cleandata(df)

write_csv(clean.df,'Manual_Dataset_0105.csv')

#############################################################################################
# # Get new sample to manual data 01.05.2018
# setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/")
# 
# set.seed(1908)
# #Get $BTC
# a <- as.data.frame(read_csv(dir(pattern=paste0('^',2,'_'))[1],
#                             locale = locale(encoding = 'latin1'))) %>% 
#   filter(created_at >= '2018-03-26') %>% 
#   select(status_id, screen_name,text)
# 
# new.sample <- sample_n(a,25)
# # Get the rest from 2 -> 10
# for (i in 2:20){
#   a <- as.data.frame(read_csv(dir(pattern=paste0('^',i,'_'))[1],
#                                locale = locale(encoding = 'latin1'))) %>%
#     filter(created_at >= '2018-03-26') %>%
#     select(status_id, screen_name,text)
#   
#   sample.df <- sample_n(a,50)
#   new.sample <- bind_rows(new.sample,sample.df)
#   print(paste0('Finish adding samples from file number: ',i))
# }
# new.sample <- unique(new.sample)
# new.sample$status_id <- as.character(new.sample$status_id)
# bk <- new.sample
# 
# # Cross-validating with list of Twitter_bots
# bots <- read.xlsx('~/GitHub/NextBigCrypto-Senti/0. Datasets/Twitter_Bot_Users_(Final).xlsx')
# new.sample <- left_join(new.sample,bots,by = 'screen_name')
# 
# new.sample <- new.sample %>% filter(botprob < 0.8) #remove user that are 80% certainly bots
# 
# # Current manual
# current <- as.data.frame(read.xlsx('~/GitHub/NextBigCrypto-Senti/Manual_Dataset_1004_labeling.xlsx')) %>%
#   select(status_id, text, processed, sentiment, trade_senti)
# 
# ###################### Extra 01.05.2018 #
# # Refill screen_name for old manual df
# # Get $BTC
# a <- as.data.frame(read_csv(dir(pattern=paste0('^',1,'_'))[1],
#                             locale = locale(encoding = 'latin1'))) %>% 
#   filter(created_at < '2018-03-26') %>% 
#   select(status_id, screen_name)
# a$status_id <- as.character(a$status_id)
# name.df <- a
# 
# # Generate list of status_id + screen_name
# for (i in 2:10){
#   a <- as.data.frame(read_csv(dir(pattern=paste0('^',i,'_'))[1],
#                               locale = locale(encoding = 'latin1'))) %>% 
#     filter(created_at < '2018-03-26') %>% 
#     select(status_id, screen_name)
#   a$status_id <- as.character(a$status_id)
#   name.df <- bind_rows(name.df,a)
# }
# name.df <- unique(name.df)
# 
# # Add column screen_name to manual.df
# new.current <- left_join(current, name.df, by = 'status_id')
# # Add botprob
# new.current <- left_join(new.current, bots, by = 'screen_name')
# 
# # Complete new manual.df
# final.df <- bind_rows(new.current,new.sample) %>%
#   select(status_id, screen_name, text, processed,sentiment, trade_senti,botprob)
# 
# # Save file
# write.xlsx(final.df,'~/GitHub/NextBigCrypto-Senti/Manual_Dataset_0105_labeling.xlsx')

##############################################################################
# # Main function to scan all FULL files
# set.seed(1908)
# setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/")
# 
# # Get $BTC in first
# a <- as.data.frame(read_csv(dir(pattern=paste0('^',1,'_'))[1],
#                             locale = locale(encoding = 'latin1')))
# clean.df <- Cleandata(a)
# #manual.df <- sample_n(clean.df,100)
# write.csv(clean.df,paste0('clean_df/1_cleandf_',Sys.Date(),'.csv'))
# 
# # Get the rest from 2 -> 10
# for (i in 2:10){
#   a <- as.data.frame(read_csv(dir(pattern=paste0('^',i,'_'))[1],
#                                locale = locale(encoding = 'latin1')))
#   clean.df <- Cleandata(a)
#   write.csv(clean.df,paste0('clean_df/',i,'_cleandf_',Sys.Date(),'.csv'))
#   #sample.df <- sample_n(clean.df,100)
#   #manual.df <- bind_rows(manual.df,sample.df)
#   print(paste0('Finish adding samples from file number: ',i))
# }

# save.image('Manual_0404.RData')
# load('Manual_0404.RData')
# 26.03
# write.xlsx(manual.df,'Manual_Dataset_2603.xlsx')
# 
# manual.done <- read.xlsx('Manual_Dataset_2603.xlsx')
