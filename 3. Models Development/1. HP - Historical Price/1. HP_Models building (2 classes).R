# Prepare dataset for building historical price model (HP)

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
              "ggplot2", # plotting package
              "quanteda", #kwic function search phrases
              "xtable", "DT", #viewing data type from quanteda
              "stringi", #string manipulation
              "Hmisc", #binning
              "tidyquant",
              "memisc", # case-when function
              "caTools","caret", "rpart", "h2o","e1071","RWeka","randomForest") # machine learning packages

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

# if use openxlsx
# ZIP PATH for dev tools
if(devtools::find_rtools()) Sys.setenv(R_ZIPCMD= file.path(devtools:::get_rtools_path(),"zip"))
library(openxlsx)

############################
token_name <- 'BTC'

#start_date <- as.Date(min(tweet.df$created_at))

start_date <- as.Date('2017-09-30')

##########################
# load price dataset

price.df <- read_csv("./1. Crawlers/Crypto-Markets_2018-04-22.csv") %>%
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

# Histogram overlaid with kernel density curve
# ggplot(dat, aes(x=pricediff)) + 
#   geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
#                  binwidth=.5,
#                  colour="black", fill="white") +
#   geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
# 
# ggplot(dat, aes(x=pricediff)) +
#   geom_histogram(binwidth=.5, colour="black", fill="white") +
#   geom_vline(aes(xintercept=mean(diff.abs, na.rm=T)),   # Ignore NA values for mean
#              color="red", linetype="dashed", size=1)

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
# for (i in 2:nrow(price.df)){
#   price.df$bin[i] <- ifelse(price.df$diff[i] < -5,'strong down',
#                             ifelse(price.df$diff[i] < 0 & price.df$diff[i] >= -5,'down',
#                                    ifelse(price.df$diff[i] > 0 & price.df$diff[i] <= 5,'up','strong up')))
# }

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
set.seed(1908)

main.df <- price.df[c('date','bin','t_1','t_2','t_3','t_4','t_5',
                      't_6','t_7','t_8','t_9','t_10',
                      't_11','t_12','t_13','t_14')]

# Split random

# split <- sample.split(main.df$bin, SplitRatio=0.8) #bin is target variable
# train <- subset(main.df, split==TRUE)
# test <- subset(main.df, split==FALSE)
# delete 1st row of test data
# test <- test[-1,]

# Split according to timeline
train <- main.df %>% filter(date <= '2018-02-28')
train <- train[,-1]

test <- main.df %>% filter(date > '2018-02-28')
test <- test[,-1]

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
prediction.Logi <- predict(LogiModel, newdata= logitest[,2:ncol(logitest)], type = "response")
prediction.Logi

# Convert to up/down
prediction.Logi <- ifelse(prediction.Logi < 0.5,'down','up')


cmLogi <- table(test$bin, prediction.Logi)
metrics(cmLogi)

ConfusionMatrix(prediction.Logi,test$bin)

########################################
# Naive Bayes

NBayes <- train(bin ~., 
                data = train, 
                laplace = 1, 
                model = "nb",
                na.action = na.exclude,
                trControl = train_control)

predictionsNB <- predict(NBayes, newdata = test[,2:ncol(test)])
cmNB <- table(test$bin, predictionsNB)

ConfusionMatrix(predictionsNB,test$bin)

metrics(cmNB) # acc 37%

##############
# Recursive feature elimination (via caret package)
set.seed(1908)

ctrl <- rfeControl(functions = nbFuncs, #naive bayes
                   method = "cv",
                   number = 10,
                   verbose = FALSE)

# convert dependent variables to factor vector
y <- train[,1]
y <- as.vector(unlist(y))
y <- as.factor(y)

# apply rfe
nbProfile <- rfe(x = train[,2:ncol(train)], # features
                 y,
                 sizes = 1:8,   # retain from 1-8 features
                 rfeControl =  ctrl)

# summary of rfe
nbProfile
# get predictors
predictors(nbProfile)

nbProfile$fit
head(nbProfile$resample)

# Ploting rfe progress
trellis.par.set(caretTheme())
plot(nbProfile, type = c("g", "o"), main = 'RFE for Naive Bayes')

########################################
# Route 1
## Apply new predictors to NBayes
newNB_rfe <- predict(nbProfile, test[,2:ncol(test)])
newNB_rfe

cmNB_rfe1 <- table(test$bin,newNB_rfe$pred)
metrics(cmNB_rfe1) # acc up to 46%

ConfusionMatrix(newNB_rfe$pred,test$bin)

# Route 2
## Apply new predictors to modeling
nbProfile$optVariables
f <- as.formula(paste("bin", paste(nbProfile$optVariables, collapse=" + "), sep=" ~ "))

set.seed(1908)
NBayes_rfe <- train(f,
                    data = train,
                    laplace = 1, model = "nb",
                    na.action = na.exclude,
                    trControl = train_control)

predictionsNB_rfe <- predict(NBayes_rfe, newdata=test[,2:ncol(test)])

cmNB_rfe2 <- table(test$bin, predictionsNB_rfe)

metrics(cmNB_rfe2) # acc 46%

##########################################
# Support Vector Machine

SVM <-  train(bin ~ ., 
              data=train, 
              model = "svm", 
              na.action = na.exclude,
              trControl=train_control)

predictSVM <- predict(SVM, newdata=test[,2:ncol(test)])

cmSVM <-   table(test$bin, predictSVM)

metrics(cmSVM) # acc 37%

# Support Vector Machine - RFE
set.seed(1908)
svmFuncs <- caretFuncs
ctrl <- rfeControl(functions = svmFuncs, #SVM
                   method = "cv",
                   number = 10,
                   verbose = FALSE)

# convert dependent variables to factor vector
y <- train[,1]
y <- as.vector(unlist(y))
y <- as.factor(y)

# apply rfe
svmProfile <- rfe(x = train[,2:ncol(train)], # features
                 y,
                 sizes = 1:8,   # retain from 1-8 features
                 rfeControl =  ctrl)

# summary of rfe
svmProfile
# get predictors
predictors(svmProfile)

svmProfile$fit
head(svmProfile$resample)

# Ploting rfe progress
trellis.par.set(caretTheme())
plot(nbProfile, type = c("g", "o"), main = 'RFE for Naive Bayes')
