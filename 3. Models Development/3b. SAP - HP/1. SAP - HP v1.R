# Building Sentiment Anallysis - Packages model 08.05.2018

# clear the environment
rm(list= ls())
# rm(list=setdiff(ls(), c("BTC.senti","BTC.senti.done"))) #remove everything except BTC.clean
gc()
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


###########################
#     Prepare dataset     #
###########################

# Load BTC 08.05.2018
BTC <- as.data.frame(read_csv('~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/1_$BTC_FULL.csv',
                              locale = locale(encoding = 'latin1'))) %>% 
  dplyr::select(created_at, status_id, screen_name, user_id, text)


#################################
# 28.05
# Load cleaned dataset (already botprob)
BTC <- read_csv('~/GitHub/NextBigCrypto-Senti/0. Datasets/BTC_clean_2805.csv',locale = locale(encoding = 'latin1'))
BTC$status_id <- as.character(BTC$status_id)
BTC$user_id <- as.character(BTC$user_id)

source('~/GitHub/NextBigCrypto-Senti/2. Preprocessing/1. Preprocessing_TW.R')
####################################
#     Load SA models (packages)    #
####################################

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

scores <- score.sentiment(BTC$text, pos.words, neg.words, .progress='text')
result <- scores

#Add ID to result set
result$status_id <- as.character(BTC$status_id)
#add new scores as a column
result <- mutate(result, status_id, sentiment.packages = ifelse(result$score > 0, 1, 
                                                                ifelse(result$score < 0, -1, 0)))%>% 
  dplyr::select(status_id, sentiment.packages)

# Merge to get final sentiment dataset
BTC.senti.final <- inner_join(BTC, result, by = 'status_id')

BTC.senti.final <- BTC.senti.final %>%
  mutate(date = as.Date(created_at))%>%
  dplyr::select(date, status_id, user_id, screen_name, text, 
         #botprob, 
         sentiment.packages)

# Packages models
BTC.senti.packages <- BTC.senti.final %>% 
  dplyr::select(date,sentiment.packages) %>%
  dplyr::group_by(date, sentiment.packages) %>%
  dplyr::summarize(count =n())

BTC.senti.packages <- BTC.senti.packages %>% 
  dplyr::group_by(date) %>% 
  mutate(countT = sum(count)) %>%
  dplyr::group_by(sentiment.packages) %>%
  mutate(per = round(100* count/countT,2))

# Convert to each sentiment = column
BTC.senti.packages <- dcast(BTC.senti.packages, date + countT ~ sentiment.packages , 
                            value.var = 'per')
colnames(BTC.senti.packages) <- c('date','count','neg','neu','pos')


#######################################################################################
###############################
#     Load price dataset      #
###############################
##########################
# load price dataset

start_date <- min(BTC.senti.final$date)
end_date <- max(BTC.senti.final$date)
token_name <- 'BTC'

price.df <- read_csv("./1. Crawlers/Crypto-Markets_2018-05-07.csv") %>%
  filter(symbol == token_name & date >= start_date & date <= end_date) %>%
  dplyr::select(date,close)

# price.df <- readxl::read_xlsx('~/GitHub/NextBigCrypto-Senti/1. Crawlers/Historical_Data_HR.xlsx') %>%
#   filter(symbol == token_name) %>%
#   dplyr::select(-date.time)

# calculate differences between close prices of each transaction dates
price.df$pricediff <- 0

for (i in 2:nrow(price.df)){
  price.df$pricediff[i] <- price.df$close[i] - price.df$close[i-1]
}

###########
# BINNING #
###########

price.df$diff <- NA
price.df$bin <- NA

# Assigning bin to main dataframe
for (i in 2:nrow(price.df)){
  price.df$diff[i] <- round(((price.df$close[i]-price.df$close[i-1])/price.df$close[i])*100,2)
}

# This version only split 2 classes
for (i in 2:nrow(price.df)){
  price.df$bin[i] <- ifelse(price.df$diff[i] < 0,'down','up')
}

#
# FEATURES ENGINEER
#
price.df <- price.df[complete.cases(price.df),]
# Generate columns through loop
for (i in 1:14){
  eval(parse(text = paste0('price.df$t_', i,' <- NA')))
}

for (i in 1:nrow(price.df)){
  for (j in 1:x){
    eval(parse(text = paste0('price.df$t_', j,' <- as.factor(lag(price.df$bin,',j,'))')))
  }
}
# Convert to categorical variables
price.df$bin <- as.factor(price.df$bin)
price.df <- unique(price.df)
#############################################################
# Sentiment Analysis dataset

name <- c('count','pos','neg','neu')

for (k in 1:length(name)){
  # Create 14x4 sentiment features
  # Generate columns through loop
  for (i in 1:x){
    eval(parse(text = paste0('BTC.senti.packages$,',name[k],'_', i,' <- NA')))
  }
  
  for (i in 1:nrow(price.df)){
    for (j in 1:x){
      eval(parse(text = paste0('BTC.senti.packages$,',name[k],'_', j,' <- lag(BTC.senti.packages$,',name[k],'_',j,')')))
    }
  }
}

# Build a training and testing set.
main.df <- inner_join(price.df, BTC.senti.packages, by = 'date')
main.df <- unique(main.df)
# Select only relevant features
main.df <- main.df %>%
  dplyr::select(-date,-close,-diff,-pricediff)
# Remove NA 
main.df <- main.df[complete.cases(main.df),]

# Split random
set.seed(1908)
split <- sample.split(main.df$bin, SplitRatio=0.8) #bin is target variable
train <- subset(main.df, split==TRUE)
test <- subset(main.df, split==FALSE)

##########################################################################################################
# Function to calculate accuracy/prediction/recall

metrics <- function(cm) {
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  
  #Accuracy
  accuracy = sum(diag) / n
  
  #Per-class Precision, Recall, and F-1
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  
  #One-For-All
  OneVsAll = lapply(1 : nc,
                    function(i){
                      v = c(cm[i,i],
                            rowsums[i] - cm[i,i],
                            colsums[i] - cm[i,i],
                            n-rowsums[i] - colsums[i] + cm[i,i]);
                      return(matrix(v, nrow = 2, byrow = T))})
  
  s = matrix(0, nrow = 2, ncol = 2)
  for(i in 1 : nc){s = s + OneVsAll[[i]]}
  
  #Average Accuracy
  avgAccuracy = sum(diag(s)) / sum(s)
  
  #Macro Averaging
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  
  #Micro Averageing
  micro_prf = (diag(s) / apply(s,1, sum))[1]
  
  #####################################
  #Matthew Correlation Coefficient
  mcc_numerator<- 0
  temp <- array()
  count <- 1
  
  for (k in 1:nrow(cm)){
    for (l in 1:nrow(cm)){
      for (m in 1:nrow(cm)){
        temp[count] <- (cm[k,k]*cm[m,l])-(cm[l,k]*cm[k,m])
        count <- count+1}}}
  sum(temp)
  mcc_numerator <- sum(temp)
  
  mcc_denominator_1 <- 0 
  count <- 1
  mcc_den_1_part1 <- 0
  mcc_den_1_part2 <- 0
  
  for (k in 1:nrow(cm)){
    mcc_den_1_part1 <- 0
    for (l in 1:nrow(cm)){
      mcc_den_1_part1 <- mcc_den_1_part1 + cm[l,k]}
    
    mcc_den_1_part2 <- 0;
    
    for (f in 1:nrow(cm)){
      if (f != k){
        for (g in 1:nrow(cm)){
          mcc_den_1_part2 <- mcc_den_1_part2+cm[g,f]
        }}}
    mcc_denominator_1=(mcc_denominator_1+(mcc_den_1_part1*mcc_den_1_part2));
  }
  
  
  mcc_denominator_2 <- 0 
  count <- 1
  mcc_den_2_part1 <- 0
  mcc_den_2_part2 <- 0
  
  for (k in 1:nrow(cm)){
    mcc_den_2_part1 <- 0
    for (l in 1:nrow(cm)){
      mcc_den_2_part1 <- mcc_den_2_part1 + cm[k,l]}
    
    mcc_den_2_part2 <- 0;
    
    for (f in 1:nrow(cm)){
      if (f != k){
        for (g in 1:nrow(cm)){
          mcc_den_2_part2 <- mcc_den_2_part2+cm[f,g]
        }}}
    mcc_denominator_2=(mcc_denominator_2+(mcc_den_2_part1*mcc_den_2_part2));
  }
  
  mcc = (mcc_numerator)/((mcc_denominator_1^0.5)*(mcc_denominator_2^0.5))
  
  final <- as.data.frame(cbind(accuracy,precision,recall,avgAccuracy,
                               macroPrecision,macroRecall,macroF1,
                               micro_prf,mcc))
  return(final)
}
##############################
# k-fold validation (10)
train_control <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)

#################################
# Base-line model 
logitrain <- train
logitest <- test

logitrain$bin <- ifelse(logitrain$bin == 'up',1,0)
logitest$bin <- ifelse(logitest$bin == 'up',1,0)

LogiModel <- glm(bin ~., 
                 data = logitrain, 
                 family = binomial(link = "logit"))

summary(LogiModel)

# Prediction
prediction.Logi <- predict(LogiModel, 
                           newdata= logitest[,2:ncol(logitest)], 
                           type = "response")
prediction.Logi

# Convert to up/down
prediction.Logi <- ifelse(prediction.Logi < 0.5,'down','up')


cmLogi <- table(test$bin, prediction.Logi)
metrics(cmLogi) # 53%

confusionMatrix(as.factor(prediction.Logi),test$bin)

########################################
# Naive Bayes
set.seed(1234)
NBayes <- train(bin ~., 
                data = train, 
                laplace = 1, 
                method = "nb",
                trControl = train_control)

predictionsNB <- predict(NBayes, 
                         newdata = test[,2:ncol(test)])

cmNB <- table(test$bin, predictionsNB)

confusionMatrix(predictionsNB,test$bin)

metrics(cmNB)

########################################
# Random Forest
set.seed(1234)
RF <- train(bin ~.,
            data = train,
            method = "rf",
            trControl = train_control)

predictionsRF <- predict(RF, 
                         newdata = test[,2:ncol(test)])

cmRF <- table(test$bin, predictionsRF)

confusionMatrix(predictionsRF,test$bin)

metrics(cmRF) 

########################################
# Support Vector Machine
set.seed(1234)
SVM <- train(bin ~.,
             data = train,
             method = "svmLinear",
             trControl = train_control)

predictionsSVM <- predict(SVM, 
                          newdata = test[,2:ncol(test)])

cmSVM <- table(test$bin, predictionsSVM)

confusionMatrix(predictionsSVM,test$bin)

metrics(cmSVM)

########################################
# C5.0 tree
set.seed(1234)
C5.0 <- train(bin ~.,
              data = train,
              method = "C5.0",
              trControl = train_control)

predictionsC50 <- predict(C5.0, 
                          newdata = test[,2:ncol(test)])

cmC50 <- table(test$bin, predictionsC50)

confusionMatrix(predictionsC50,test$bin)

metrics(cmC50) # acc 57%

# Temporary save
save.image('~/GitHub/NextBigCrypto-Senti/Models/SAP_v1_100518.RData')
# load('~/GitHub/NextBigCrypto-Senti/Models/SAP_v1_100518.RData')