# Building Sentiment Anallysis - Trained model 23.04.2018

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
              "xtable", "DT", #viewing data type from quanteda
              "stringi", #string manipulation
              "wordcloud","tidyquant",
              "caTools","caret", "rpart", "h2o","e1071","RWeka",
              "randomForest"
)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

# if use openxlsx
# ZIP PATH for dev tools
# if(devtools::find_rtools()) Sys.setenv(R_ZIPCMD= file.path(devtools:::get_rtools_path(),"zip"))
# library(openxlsx)

###########################
#     Prepare dataset     #
###########################

# Load BTC 02.05.2018
BTC <- as.data.frame(read_csv('./1. Crawlers/1b. Report/1_$BTC_FULL.csv',
                               locale = locale(encoding = 'latin1'))) %>%
  select(created_at, status_id, screen_name, user_id, text)

source('~/GitHub/NextBigCrypto-Senti/2. Preprocessing/1. Preprocessing_TW.R')

# Run dataset through preprocessing pipeline
BTC.clean <- Cleandata(BTC)

# Temporary save
save.image('~/GitHub/NextBigCrypto-Senti/Models/SAT_v1_020518.RData')


# Preprocessing
corp <- VCorpus(VectorSource(BTC.clean$processed)) # VCorpus compatible with n-gram analysis

# Unigram
frequencies <- DocumentTermMatrix(corp)

# TF-IDF weighting 09.04.2018 (lower acc)
#frequencies <- DocumentTermMatrix(corp,
#                                  control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

# Remove these words that are not used very often. Keep terms that appear in 0.5% or more of the dataset
sparse <- removeSparseTerms(frequencies, 0.995)

ResultSparse <- as.data.frame(as.matrix(sparse))
colnames(ResultSparse) <- make.names(colnames(ResultSparse))

#######################
#     Load models     #
#######################
# Using best model so far (NB - 02.05.18)
load('./Models/Senti_Manual_Uni_New_2018-05-02.RData')

predictionsNB <- predict(NBayes,  newdata = ResultSparse)

New.BTC <- cbind(BTC.clean, sentiment_NB = predictionsNB)

###############################
#     Load price dataset      #
###############################
token_name <- 'BTC'
start_date <- as.Date('2017-09-30')

##########################
# load price dataset

price.df <- read_csv("./1. Crawlers/Crypto-Markets_2018-04-30.csv") %>%
  filter(symbol == token_name & date >= start_date)

price.df <- price.df[c('date','close')]

# calculate differences between close prices of each transaction dates
price.df$pricediff <- 0

for (i in 2:nrow(price.df)){
  price.df$pricediff[i] <- price.df$close[i] - price.df$close[i-1]
}

############
# Exploratory
dat <- as.data.frame(price.df$pricediff)
dat <- cbind(price.df$date,dat, price.df$close)
colnames(dat) <- c('date', 'pricediff','close')

# Density curve
ggplot(dat , aes(x=pricediff)) +
  geom_density(fill='lightblue') +
  geom_rug() +
  labs(x='Differences between close prices')

###########
# BINNING #
###########

hist(dat$pricediff)

dat$diff <- NA
for (i in 2:nrow(dat)){
  dat$diff[i] <- round(((dat$close[i]-dat$close[i-1])/dat$close[i])*100,2)
}

# Split into 4 different bins: strong up/ up / down / strong down
# dat$bin <- as.numeric(cut2(dat$pricediff, g=4))

# < 5% is normal >=5% is "strong"

hist(dat$diff, xlab = 'Percent differences between close prices',
     main = 'Histogram of percentage differences between $BTC close prices')

# Binning process
for (i in 2:nrow(dat)){
  dat$bin[i] <- ifelse(dat$diff[i] < -5,'strong down',
                       ifelse(dat$diff[i] < 0 & dat$diff[i] >= -5,'down',
                              ifelse(dat$diff[i] > 0 & dat$diff[i] <= 5,'up','strong up')))
}


# check distribution
dat %>% group_by(bin) %>% tally
dat <- dat[complete.cases(dat),]

###############################
# Preparation

price.df$diff <- NA
price.df$bin <- NA

# Assigning bin to main dataframe
for (i in 2:nrow(price.df)){
  price.df$diff[i] <- round(((price.df$close[i]-price.df$close[i-1])/price.df$close[i])*100,2)
}

# Binning process
for (i in 2:nrow(price.df)){
  price.df$bin[i] <- ifelse(price.df$diff[i] < -5,'strong down',
                            ifelse(price.df$diff[i] < 0 & price.df$diff[i] >= -5,'down',
                                   ifelse(price.df$diff[i] > 0 & price.df$diff[i] <= 5,'up','strong up')))
}
price.df <- price.df[complete.cases(price.df),]

price.df$t_1 <- NA # price movement on day t-1
price.df$t_2 <- NA # price movement on day t-2
price.df$t_3 <- NA # price movement on day t-3
price.df$t_4 <- NA # price movement on day t-4
price.df$t_5 <- NA # price movement on day t-5
price.df$t_6 <- NA # price movement on day t-6
price.df$t_7 <- NA # price movement on day t-7
price.df$t_8 <- NA # price movement on day t-8
price.df$t_9 <- NA # price movement on day t-9
price.df$t_10 <- NA # price movement on day t-10
price.df$t_11 <- NA # price movement on day t-11
price.df$t_12 <- NA # price movement on day t-12
price.df$t_13 <- NA # price movement on day t-13
price.df$t_14 <- NA # price movement on day t-14


for (i in 1:nrow(price.df)){
  if (i==1){next}
  price.df$t_1[i] <- price.df$bin[i-1]
  if (i==2){next}
  price.df$t_2[i] <- price.df$bin[i-2]
  if (i==3){next}
  price.df$t_3[i] <- price.df$bin[i-3]
  if (i==4){next}
  price.df$t_4[i] <- price.df$bin[i-4]
  if (i==5){next}
  price.df$t_5[i] <- price.df$bin[i-5]
  if (i==6){next}
  price.df$t_6[i] <- price.df$bin[i-6]
  if (i==7){next}
  price.df$t_7[i] <- price.df$bin[i-7]
  if (i==8){next}
  price.df$t_8[i] <- price.df$bin[i-8]
  if (i==9){next}
  price.df$t_9[i] <- price.df$bin[i-9]
  if (i==10){next}
  price.df$t_10[i] <- price.df$bin[i-10]
  if (i==11){next}
  price.df$t_11[i] <- price.df$bin[i-11]
  if (i==12){next}
  price.df$t_12[i] <- price.df$bin[i-12]
  if (i==13){next}
  price.df$t_13[i] <- price.df$bin[i-13]
  if (i==14){next}
  price.df$t_14[i] <- price.df$bin[i-14]
}

# Convert to categorical variables
price.df$t_1 <- as.factor(price.df$t_1)
price.df$t_2 <- as.factor(price.df$t_2)
price.df$t_3 <- as.factor(price.df$t_3)
price.df$t_4 <- as.factor(price.df$t_4)
price.df$t_5 <- as.factor(price.df$t_5)
price.df$t_6 <- as.factor(price.df$t_6)
price.df$t_7<- as.factor(price.df$t_7)
price.df$t_8<- as.factor(price.df$t_8)
price.df$t_9<- as.factor(price.df$t_9)
price.df$t_10<- as.factor(price.df$t_10)
price.df$t_11<- as.factor(price.df$t_11)
price.df$t_12<- as.factor(price.df$t_12)
price.df$t_13<- as.factor(price.df$t_13)
price.df$t_14<- as.factor(price.df$t_14)

# Build a training and testing set.
set.seed(1908)

main.df <- price.df[c('date','bin','t_1','t_2','t_3','t_4','t_5',
                      't_6','t_7','t_8','t_9','t_10',
                      't_11','t_12','t_13','t_14')]

# Split according to timeline
train <- main.df %>% filter(date <= '2018-02-28')
train <- train[,-1]

test <- main.df %>% filter(date > '2018-02-28')
test <- test[,-1]

