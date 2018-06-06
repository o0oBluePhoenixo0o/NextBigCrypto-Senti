# Building Sentiment Anallysis - Trained model 23.04.2018

# clear the environment
rm(list= ls())
gc()
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
              "textmineR","openxlsx",
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

#######################################################
# Load new 27.05 BTC.senti

BTC.senti <- read_csv('~/GitHub/NextBigCrypto-Senti/0. Datasets/BTC_clean_senti_2705.csv')
BTC.senti$status_id <- as.character(BTC.senti$status_id)
BTC.senti$user_id <- as.character(BTC.senti$user_id)

# # Load BTC 08.05.2018
# BTC <- as.data.frame(read_csv('~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/1_$BTC_FULL.csv',
#                               locale = locale(encoding = 'latin1'))) %>% 
#   dplyr::select(created_at, status_id, screen_name, user_id, text)
# 
# source('~/GitHub/NextBigCrypto-Senti/2. Preprocessing/1. Preprocessing_TW.R')
# 
# # Run dataset through preprocessing pipeline
# BTC.clean <- Cleandata(BTC)
# BTC.clean$status_id <- as.character(BTC.clean$status_id)
# BTC.clean$user_id <- as.character(BTC.clean$user_id)
# 
# 
# BTC.clean <- read_csv('~/GitHub/NextBigCrypto-Senti/0. Datasets/BTC_clean_2205.csv')
# 
# ###################################
# #     Load SA models (trained)    #
# ###################################
# 
# # Using best model so far (GBM-tuned 080518)
# h2o.init()
# gbm.model.opt <- h2o.loadModel('./Models/H2O/final_grid_model_1')
# 
# # Implement batch-running (in process)
# i <- 1
# j <- 50000
# 
# while (i <= j) {
#   # Preprocessing
#   corp <- VCorpus(VectorSource(BTC.clean$processed[i:j])) # VCorpus compatible with n-gram analysis
#   # Unigram
#   frequencies <- DocumentTermMatrix(corp,
#                                     control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
#   # Remove these words that are not used very often. Keep terms that appear in 0.5% or more of the dataset
#   sparse <- removeSparseTerms(frequencies, 0.995)
#   
#   # Generate sparse matrix
#   ResultSparse <- as.data.frame(as.matrix(sparse))
#   colnames(ResultSparse) <- make.names(colnames(ResultSparse))
# 
#   # Convert to H2O format
#   fullH2O <- as.h2o(ResultSparse)
#   
#   # Prediction by batches
#   predictionsGBM.opt <- as.data.frame(h2o.predict(gbm.model.opt,fullH2O))
#   
#   if (i==1){BTC.senti <- cbind(BTC.clean[i:j,], sentiment.trained = predictionsGBM.opt$predict)}
#   if (i!=1){BTC.senti <- rbind(BTC.senti,cbind(BTC.clean[i:j,],sentiment.trained = predictionsGBM.opt$predict))}
#   
#   print(paste0('Complete from ',i,' to ',j,' observations!'))
#   # increase i and j
#   i <- i + 50000
#   ifelse((j + 50000) <= nrow(BTC.clean),j <- j + 50000, j <- nrow(BTC.clean))
#   
#   gc()
#   }
# h2o.shutdown()
# gc()
# 
# BTC.senti <- BTC.senti %>%
#   mutate(date = as_datetime(created_at))%>%
#   select(date, status_id, user_id, screen_name, text, processed, 
#          botprob, sentiment.trained)

### 28.05 Pre-trained

BTC.senti <- read_csv('~/GitHub/NextBigCrypto-Senti/0. Datasets/BTC_clean_senti_trained_2805.csv')

BTC.senti$status_id <- as.character(BTC.senti$status_id)
BTC.senti$user_id <- as.character(BTC.senti$user_id)

# Summarize base on sentiment each day
# Trained models
# 20.05 trigger 6/12/24hr
time.slot <- 6 # insert trigger

BTC.senti.trained <- BTC.senti %>% 
  dplyr::select(date,sentiment.trained) %>%
  group_by(time = floor_date(date, paste0(time.slot,' hour')),
           sentiment.trained) %>%
  summarize(count = n())

# Get total + percentage of each class / day
BTC.senti.trained <- BTC.senti.trained %>% 
                      group_by(time) %>% 
                      mutate(countT = sum(count)) %>%
                      group_by(sentiment.trained) %>%
                      mutate(per = round(100* count/countT,2))

# Convert to each sentiment = column
BTC.senti.trained <- reshape2::dcast(BTC.senti.trained, time + countT ~ sentiment.trained,
                                     value.var = 'per')
colnames(BTC.senti.trained) <- c('time','count','neg','neu','pos')

#######################################################################################
###############################
#     Load price dataset      #
###############################
##########################
# load price dataset
token_name <- 'BTC'

price.df <- readxl::read_xlsx('~/GitHub/NextBigCrypto-Senti/1. Crawlers/Historical_Data_HR.xlsx') %>%
  filter(symbol == token_name) %>%
  dplyr::select(-date.time)

# convert to UTC (20.05.18) -- IMPORTANT!!
price.df$time <- as_datetime(anytime::anytime(price.df$time))

# filter out 24-hr mark
price.df$mark <- NA

if (time.slot == 6){target <- c(0,6,12,18)}
if (time.slot == 12){target <- c(0,12)}
if (time.slot == 24){target <- c(0)}

for (i in 1:nrow(price.df)){
  if (lubridate::hour(price.df$time[i]) %in% target){price.df$mark[i] <- 1}  
}

price.df <- price.df %>% 
  filter(mark == 1) %>%
  dplyr::select(time,close,priceBTC)

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

########################################
#
# FEATURES ENGINEER
#
########################################

# Generate columns through loop
x <- 14/(time.slot/24) # number of time shifts want to make

for (i in 1:x){
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
    eval(parse(text = paste0('BTC.senti.trained$',name[k],'_', i,' <- NA')))
  }
  
  for (i in 1:nrow(BTC.senti.trained)){
    for (j in 1:x){
      eval(parse(text = paste0('BTC.senti.trained$',name[k],'_', j,' <- lag(BTC.senti.trained$',name[k],',',j,')')))
    }
  }
}

########################################################################################
# Build a training and testing set ==> split into 14 days with 4 new features/day
main.df <- inner_join(price.df, BTC.senti.trained, by = 'time')
main.df <- unique(main.df)

# Select only relevant features
main.df <- main.df %>%
  dplyr::select(-time,-close,-diff,-pricediff,
                -count,-neg,-neu,-pos,-priceBTC)

# # Test 11.05 - keep only t-1 ==> t-3
# main.df <- main.df %>%
#   dplyr::select(-t_4,-t_5,-t_6,-t_7,-t_8,-t_9,-t_10,-t_11,-t_12,-t_13,-t_14,
#                 -neg_4,-neg_5,-neg_6,-neg_7,-neg_8,-neg_9,-neg_10,-neg_11,-neg_12,-neg_13,-neg_14,
#                 -neu_4,-neu_5,-neu_6,-neu_7,-neu_8,-neu_9,-neu_10,-neu_11,-neu_12,-neu_13,-neu_14,
#                 -pos_4,-pos_5,-pos_6,-pos_7,-pos_8,-pos_9,-pos_10,-pos_11,-pos_12,-pos_13,-pos_14,
#                 -count_4,-count_5,-count_6,-count_7,-count_8,-count_9,-count_10,-count_11,-count_12,-count_13,-count_14)

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

###############################
#                             #
#       MAIN MODEL            #
#                             #
###############################

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
VIF <- as.data.frame(car::vif(all.mod))
colnames(VIF) <- c('VIF')

##############################

# MODELS DEVELOPMENT

##############################
# k-fold validation (10)
train_control <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)

#################################
# Base-line model 
LogiModel <- train(bin ~.,
                   data = train,
                   method = "glm",
                   trControl = train_control)
LogiModel

# Prediction
prediction.Logi <- predict(LogiModel, 
                           newdata= test[,2:ncol(test)], 
                           type = "raw")
prediction.Logi

confusionMatrix(as.factor(prediction.Logi),test$bin)

cmLogi <- table(test$bin, prediction.Logi)
metrics(cmLogi)

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

metrics(cmNB) # 50% acc

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

metrics(cmRF) # 65% acc

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

metrics(cmSVM) # 59% acc

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

# Temporary save
save.image('~/GitHub/NextBigCrypto-Senti/Models/SAT_6h_200518.RData')

####################################################################################################################
# RFE RF
set.seed(1908)

ctrl.rf <- rfeControl(functions = rfFuncs, # random forest
                   method = "cv",
                   number = 10,
                   verbose = FALSE)

# convert dependent variables to factor vector
y <- train[,1]
y <- as.vector(unlist(y))
y <- as.factor(y)

# apply rfe
rfProfile <- rfe(x = train[,2:ncol(train)], # features
                 y,
                 sizes = 1:10,   # retain from 1-8 features
                 rfeControl =  ctrl.rf)

# summary of rfe
rfProfile
# get predictors
predictors(rfProfile)

rfProfile$fit
head(rfProfile$resample)

# Ploting rfe progress
trellis.par.set(caretTheme())
plot(rfProfile, type = c("g", "o"), main = 'RFE for Random Forest')
########################################
# Route 1
## Apply new predictors to Random Forest
newRF_rfe <- predict(rfProfile, test[,2:ncol(test)])
newRF_rfe

cmRF_rfe1 <- table(test$bin,newRF_rfe$pred)
metrics(cmRF_rfe1) 

confusionMatrix(newRF_rfe$pred,test$bin)
##########################################################
# Recursive feature elimination NB (via caret package)
set.seed(1908)

ctrl.nb <- rfeControl(functions = nbFuncs, #naive bayes
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
                 rfeControl =  ctrl.nb)

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
metrics(cmNB_rfe1) 

confusionMatrix(newNB_rfe$pred,test$bin)

# Route 2
## Apply new predictors to modeling
nbProfile$optVariables
f <- as.formula(paste("bin", paste(nbProfile$optVariables, collapse=" + "), sep=" ~ "))

set.seed(1908)
NBayes_rfe <- train(f,
                    data = train,
                    laplace = 1, 
                    method = "nb",
                    trControl = train_control)

predictionsNB_rfe <- predict(NBayes_rfe, newdata=test[,2:ncol(test)])

cmNB_rfe2 <- table(test$bin, predictionsNB_rfe)

metrics(cmNB_rfe2)
confusionMatrix(predictionsNB_rfe,test$bin)
##########################################