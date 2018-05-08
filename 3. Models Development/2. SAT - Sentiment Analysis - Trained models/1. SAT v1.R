# Building Sentiment Anallysis - Trained model 23.04.2018

# clear the environment
rm(list= ls())
# rm(list=setdiff(ls(), c("BTC.senti","BTC.senti.done"))) #remove everything except BTC.clean

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
  select(created_at, status_id, screen_name, user_id, text)

source('~/GitHub/NextBigCrypto-Senti/2. Preprocessing/1. Preprocessing_TW.R')

# Run dataset through preprocessing pipeline
BTC.clean <- Cleandata(BTC)
BTC.clean$status_id <- as.character(BTC.clean$status_id)
BTC.clean$user_id <- as.character(BTC.clean$user_id)

###################################
#     Load SA models (trained)    #
###################################

# Using best model so far (GBM-tuned 080518)
h2o.init()
gbm.model.opt <- h2o.loadModel('./Models/H2O/final_grid_model_1')

# Implement batch-running (in process)
i <- 1
j <- 50000

while (i <= j) {
  # Preprocessing
  corp <- VCorpus(VectorSource(BTC.clean$processed[i:j])) # VCorpus compatible with n-gram analysis
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
  
  if (i==1){BTC.senti <- cbind(BTC.clean[i:j,], sentiment.trained = predictionsGBM.opt$predict)}
  if (i!=1){BTC.senti <- rbind(BTC.senti,cbind(BTC.clean[i:j,],sentiment.trained = predictionsGBM.opt$predict))}
  
  print(paste0('Complete from ',i,' to ',j,' observations!'))
  # increase i and j
  i <- i + 50000
  ifelse((j + 50000) <= nrow(BTC.clean),j <- j + 50000, j <- nrow(BTC.clean))
  
  gc()
  }

BTC.senti <- BTC.senti %>%
  mutate(date = as.Date(created_at))%>%
  select(date, status_id, user_id, screen_name, text, processed, 
         botprob, sentiment.packages, sentiment.trained)

# Summarize base on sentiment each day
# Trained models
BTC.senti.trained <-BTC.senti.final %>% 
  select(date,sentiment.trained) %>%
  group_by(date, sentiment.trained) %>%
  summarize(count =n())

# Get total + percentage of each class / day
BTC.senti.trained <- BTC.senti.trained %>% 
                      group_by(date) %>% 
                      mutate(countT = sum(count)) %>%
                      group_by(sentiment.trained) %>%
                      mutate(per = round(100* count/countT,2))

# Convert to each sentiment = column
BTC.senti.trained <- dcast(BTC.senti.trained, date + countT ~ sentiment.trained , 
                           value.var = 'per')
colnames(BTC.senti.trained) <- c('date','count.total','negative','neutral','positive')


#######################################################################################
###############################
#     Load price dataset      #
###############################
##########################
# load price dataset

start_date <- min(BTC.senti.final$created)
end_date <- max(BTC.senti.final$created)
token_name <- 'BTC'

price.df <- read_csv("./1. Crawlers/Crypto-Markets_2018-05-07.csv") %>%
  filter(symbol == token_name & date >= start_date & date <= end_date) %>%
  select(date,close)

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

price.df$bin <- as.factor(price.df$bin)

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
main.df <- inner_join(price.df, BTC.senti.trained, by = 'date')

# Split random
set.seed(1908)
split <- sample.split(main.df$bin, SplitRatio=0.8) #bin is target variable
train <- subset(main.df[,5:ncol(main.df)], split==TRUE)
test <- subset(main.df[,5:ncol(main.df)], split==FALSE)

# delete 1st row of test data
# test <- test[-1,]

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

LogiModel <- glm(bin ~., data = logitrain, family = binomial(link = "logit"))

summary(LogiModel)

# Prediction
prediction.Logi <- predict(LogiModel, newdata= logitest, type = "response")
prediction.Logi

# Convert to up/down
prediction.Logi <- ifelse(prediction.Logi < 0.5,'down','up')


cmLogi <- table(test$bin, prediction.Logi)
metrics(cmLogi) # acc 72%

confusionMatrix(as.factor(prediction.Logi),test$bin)

########################################
# Naive Bayes

NBayes <- train(bin ~., 
                data = train, 
                laplace = 1, 
                model = "nb",
                na.action = na.omit,
                trControl = train_control)

# remove NA observations
test <- test[-c(1:2),]

NBayes

predictionsNB <- predict(NBayes, newdata = test[,2:ncol(test)])
cmNB <- table(test$bin, predictionsNB)

confusionMatrix(predictionsNB,test$bin)

metrics(cmNB) # acc 37%



# Temporary save
save.image('~/GitHub/NextBigCrypto-Senti/Models/SAT_v1_090518.RData')
# load('~/GitHub/NextBigCrypto-Senti/Models/SAT_v1_080518.RData')