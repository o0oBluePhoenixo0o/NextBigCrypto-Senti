# Prepare dataset for building historical price model (HP)

# Feature Selection Methods
# http://r-statistics.co/Variable-Selection-and-Importance-With-R.html

# clear the environment
rm(list= ls())
gc()
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

############################
token_name <- 'ETH'

#start_date <- as.Date(min(tweet.df$created_at))

start_date <- as.Date('2017-09-30')

##########################
# load price dataset

price.df <- read_csv("./1. Crawlers/Crypto-Markets_2018-05-07.csv") %>%
  filter(symbol == token_name & date >= start_date)

price.df <- price.df[c('date','close')]

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

# Remove all NAs
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

# Build a training and testing set.
main.df <- price.df[c('bin','t_1','t_2','t_3','t_4','t_5',
                      't_6','t_7','t_8','t_9','t_10',
                      't_11','t_12','t_13','t_14')]

# Remove NA 
main.df <- main.df[complete.cases(main.df),]
main.df <- unique(main.df)
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

####################################################################################
# Features Importance Analysis

# Decide if a variable is important or not using Boruta
boruta_output <- Boruta::Boruta(bin ~ ., data = train, doTrace=2)  # perform Boruta search
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance


# Stepwise regression
base.mod <- glm(bin ~ 1 , data= train, family = binomial)  # base intercept only model
all.mod <- glm(bin ~ . , data= train, family = binomial) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(shortlistedVars)

# Variance importance factor (>2 is multicollinearity)
car::vif(all.mod)

##############################

# MODELS DEVELOPMENT

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
metrics(cmLogi) 

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

metrics(cmC50) 

####################################################################################################################
# RFE RF
set.seed(1908)

ctrl <- rfeControl(functions = rfFuncs, # random forest
                   method = "cv",
                   number = 10,
                   verbose = FALSE)

# convert dependent variables to factor vector
y <- train[,2]
y <- as.vector(unlist(y))
y <- as.factor(y)

# apply rfe
rfProfile <- rfe(x = train[,2:ncol(train)], # features
                 y,
                 sizes = 1:10,   # retain from 1-8 features
                 rfeControl =  ctrl)

# summary of rfe
rfProfile
# get predictors
predictors(rfProfile)

rfProfile$fit
head(rfProfile$resample)

# Ploting rfe progress
trellis.par.set(caretTheme())
plot(rfProfile, type = c("g", "o"), main = 'RFE for Random Forest')

##########################################################
# Recursive feature elimination NB (via caret package)
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
                 sizes = 1:10,   # retain from 1-8 features
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
metrics(cmNB_rfe1) # acc up to 57%

confusionMatrix(newNB_rfe$pred,test$bin)

# Route 2
## Apply new predictors to modeling
nbProfile$optVariables
f <- as.formula(paste("bin", paste(nbProfile$optVariables, collapse=" + "), sep=" ~ "))

set.seed(1908)
NBayes_rfe <- train(f,
                    data = train,
                    laplace = 1, method = "nb",
                    na.action = na.exclude,
                    trControl = train_control)

predictionsNB_rfe <- predict(NBayes_rfe, newdata=test[,2:ncol(test)])

cmNB_rfe2 <- table(test$bin, predictionsNB_rfe)

metrics(cmNB_rfe2) # acc 58%
confusionMatrix(predictionsNB_rfe,test$bin)
##########################################

