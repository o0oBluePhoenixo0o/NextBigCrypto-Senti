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
              "stringi" #string manipulation
)

#remove.packages(c("tidytext", "ggplot2"))
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

###################################################

#Input data
df <- as.data.frame(read_csv('1. Crawlers/1b. Report/1_$BTC_FULL.csv',
                             locale = locale(encoding = 'latin1')))
name.df <- 'BTC'

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

###################################################################


Cleandata <- function(df) {
  
  df <- df[,which(colnames(df) %in% c('created_at','text','status_id'))] %>%
    mutate(timestamp = ymd_hms(created_at))
  
  # Convert unicode
  df$text <- sapply(df$text,function(x) trueunicode.hack(x))
  
  # remove duplicates base on tweets
  df <- df[!duplicated(df$text),]
  
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
  # To lower case
  df$processed <- tolower(df$processed)
  
  # Remove duplicates (after rmv url)
  df <- df[!duplicated(df$processed),]
  # Remove blanks
  df <- df[!(is.na(df$processed) | df$processed==""), ]
  # Remove <f0>
  df$processed <- gsub("<f0>", "", df$processed)
  df$status_id <- as.character(df$status_id)
  
  return(df)
}

# Main function to scan all FULL files
set.seed(1908)
setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/")

# Get $BTC in first
a <- as.data.frame(read_csv(dir(pattern=paste0('^',1,'_'))[1],
                            locale = locale(encoding = 'latin1')))
clean.df <- Cleandata(a)
manual.df <- sample_n(clean.df,100)

# Get the rest from 2 -> 50
for (i in 2:50){
  a <- as.data.frame(read_csv(dir(pattern=paste0('^',i,'_'))[1],
                               locale = locale(encoding = 'latin1')))
  clean.df <- Cleandata(a)
  write.csv(clean.df,paste0(i,'_cleandf_',Sys.Date(),'.csv'))
  sample.df <- sample_n(clean.df,100)
  manual.df <- bind_rows(manual.df,sample.df)
  print(paste0('Finish adding samples from file number: ',i))
}

#save.image('Manual_2603.RData')

# 26.03

if(devtools::find_rtools()) Sys.setenv(R_ZIPCMD= file.path(devtools:::get_rtools_path(),"zip"))
ibrary(openxlsx)
write.xlsx(manual.df,'Manual_Dataset_2603.xlsx')

manual.done <- read.xlsx('Manual_Dataset_2603.xlsx')
