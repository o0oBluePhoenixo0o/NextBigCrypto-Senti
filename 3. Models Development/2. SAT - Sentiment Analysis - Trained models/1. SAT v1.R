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
                              locale = locale(encoding = 'latin1')))
BTC <- BTC %>% select(created_at, status_id, screen_name, user_id, text)

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
colnames(BTC.senti.trained) <- c('date','count','neg','neu','pos')


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
# Remove NA
price.df <- price.df[complete.cases(price.df),]

#
# FEATURES ENGINEER
#

# Create 14 price movement features
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
price.df <- unique(price.df)

#############################################################
# Sentiment Analysis dataset

# Create 14x4 sentiment features
BTC.senti.trained$count_1 <- NA # total messages on day t-1
BTC.senti.trained$count_2 <- NA # total messages on day t-2
BTC.senti.trained$count_3 <- NA # total messages on day t-3
BTC.senti.trained$count_4 <- NA # total messages on day t-4
BTC.senti.trained$count_5 <- NA # total messages on day t-5
BTC.senti.trained$count_6 <- NA # total messages on day t-6
BTC.senti.trained$count_7 <- NA # total messages on day t-7
BTC.senti.trained$count_8 <- NA # total messages on day t-8
BTC.senti.trained$count_9 <- NA # total messages on day t-9
BTC.senti.trained$count_10 <- NA # total messages on day t-10
BTC.senti.trained$count_11 <- NA # total messages on day t-11
BTC.senti.trained$count_12 <- NA # total messages on day t-12
BTC.senti.trained$count_13 <- NA # total messages on day t-13
BTC.senti.trained$count_14 <- NA # total messages on day t-14

for (i in 1:nrow(BTC.senti.trained)){
  if (i==1){next}
  BTC.senti.trained$count_1[i] <- BTC.senti.trained$count[i-1]
  if (i==2){next}
  BTC.senti.trained$count_2[i] <- BTC.senti.trained$count[i-2]
  if (i==3){next}
  BTC.senti.trained$count_3[i] <- BTC.senti.trained$count[i-3]
  if (i==4){next}
  BTC.senti.trained$count_4[i] <- BTC.senti.trained$count[i-4]
  if (i==5){next}
  BTC.senti.trained$count_5[i] <- BTC.senti.trained$count[i-5]
  if (i==6){next}
  BTC.senti.trained$count_6[i] <- BTC.senti.trained$count[i-6]
  if (i==7){next}
  BTC.senti.trained$count_7[i] <- BTC.senti.trained$count[i-7]
  if (i==8){next}
  BTC.senti.trained$count_8[i] <- BTC.senti.trained$count[i-8]
  if (i==9){next}
  BTC.senti.trained$count_9[i] <- BTC.senti.trained$count[i-9]
  if (i==10){next}
  BTC.senti.trained$count_10[i] <- BTC.senti.trained$count[i-10]
  if (i==11){next}
  BTC.senti.trained$count_11[i] <- BTC.senti.trained$count[i-11]
  if (i==12){next}
  BTC.senti.trained$count_12[i] <- BTC.senti.trained$count[i-12]
  if (i==13){next}
  BTC.senti.trained$count_13[i] <- BTC.senti.trained$count[i-13]
  if (i==14){next}
  BTC.senti.trained$count_14[i] <- BTC.senti.trained$count[i-14]
}

# Create 14x4 sentiment features POSITIVE
BTC.senti.trained$pos_1 <- NA # total messages on day t-1
BTC.senti.trained$pos_2 <- NA # total messages on day t-2
BTC.senti.trained$pos_3 <- NA # total messages on day t-3
BTC.senti.trained$pos_4 <- NA # total messages on day t-4
BTC.senti.trained$pos_5 <- NA # total messages on day t-5
BTC.senti.trained$pos_6 <- NA # total messages on day t-6
BTC.senti.trained$pos_7 <- NA # total messages on day t-7
BTC.senti.trained$pos_8 <- NA # total messages on day t-8
BTC.senti.trained$pos_9 <- NA # total messages on day t-9
BTC.senti.trained$pos_10 <- NA # total messages on day t-10
BTC.senti.trained$pos_11 <- NA # total messages on day t-11
BTC.senti.trained$pos_12 <- NA # total messages on day t-12
BTC.senti.trained$pos_13 <- NA # total messages on day t-13
BTC.senti.trained$pos_14 <- NA # total messages on day t-14

for (i in 1:nrow(BTC.senti.trained)){
  if (i==1){next}
  BTC.senti.trained$pos_1[i] <- BTC.senti.trained$pos[i-1]
  if (i==2){next}
  BTC.senti.trained$pos_2[i] <- BTC.senti.trained$pos[i-2]
  if (i==3){next}
  BTC.senti.trained$pos_3[i] <- BTC.senti.trained$pos[i-3]
  if (i==4){next}
  BTC.senti.trained$pos_4[i] <- BTC.senti.trained$pos[i-4]
  if (i==5){next}
  BTC.senti.trained$pos_5[i] <- BTC.senti.trained$pos[i-5]
  if (i==6){next}
  BTC.senti.trained$pos_6[i] <- BTC.senti.trained$pos[i-6]
  if (i==7){next}
  BTC.senti.trained$pos_7[i] <- BTC.senti.trained$pos[i-7]
  if (i==8){next}
  BTC.senti.trained$pos_8[i] <- BTC.senti.trained$pos[i-8]
  if (i==9){next}
  BTC.senti.trained$pos_9[i] <- BTC.senti.trained$pos[i-9]
  if (i==10){next}
  BTC.senti.trained$pos_10[i] <- BTC.senti.trained$pos[i-10]
  if (i==11){next}
  BTC.senti.trained$pos_11[i] <- BTC.senti.trained$pos[i-11]
  if (i==12){next}
  BTC.senti.trained$pos_12[i] <- BTC.senti.trained$pos[i-12]
  if (i==13){next}
  BTC.senti.trained$pos_13[i] <- BTC.senti.trained$pos[i-13]
  if (i==14){next}
  BTC.senti.trained$pos_14[i] <- BTC.senti.trained$pos[i-14]
}

# Create 14x4 sentiment features NEUTRAL
BTC.senti.trained$neu_1 <- NA # total messages on day t-1
BTC.senti.trained$neu_2 <- NA # total messages on day t-2
BTC.senti.trained$neu_3 <- NA # total messages on day t-3
BTC.senti.trained$neu_4 <- NA # total messages on day t-4
BTC.senti.trained$neu_5 <- NA # total messages on day t-5
BTC.senti.trained$neu_6 <- NA # total messages on day t-6
BTC.senti.trained$neu_7 <- NA # total messages on day t-7
BTC.senti.trained$neu_8 <- NA # total messages on day t-8
BTC.senti.trained$neu_9 <- NA # total messages on day t-9
BTC.senti.trained$neu_10 <- NA # total messages on day t-10
BTC.senti.trained$neu_11 <- NA # total messages on day t-11
BTC.senti.trained$neu_12 <- NA # total messages on day t-12
BTC.senti.trained$neu_13 <- NA # total messages on day t-13
BTC.senti.trained$neu_14 <- NA # total messages on day t-14

for (i in 1:nrow(BTC.senti.trained)){
  if (i==1){next}
  BTC.senti.trained$neu_1[i] <- BTC.senti.trained$neu[i-1]
  if (i==2){next}
  BTC.senti.trained$neu_2[i] <- BTC.senti.trained$neu[i-2]
  if (i==3){next}
  BTC.senti.trained$neu_3[i] <- BTC.senti.trained$neu[i-3]
  if (i==4){next}
  BTC.senti.trained$neu_4[i] <- BTC.senti.trained$neu[i-4]
  if (i==5){next}
  BTC.senti.trained$neu_5[i] <- BTC.senti.trained$neu[i-5]
  if (i==6){next}
  BTC.senti.trained$neu_6[i] <- BTC.senti.trained$neu[i-6]
  if (i==7){next}
  BTC.senti.trained$neu_7[i] <- BTC.senti.trained$neu[i-7]
  if (i==8){next}
  BTC.senti.trained$neu_8[i] <- BTC.senti.trained$neu[i-8]
  if (i==9){next}
  BTC.senti.trained$neu_9[i] <- BTC.senti.trained$neu[i-9]
  if (i==10){next}
  BTC.senti.trained$neu_10[i] <- BTC.senti.trained$neu[i-10]
  if (i==11){next}
  BTC.senti.trained$neu_11[i] <- BTC.senti.trained$neu[i-11]
  if (i==12){next}
  BTC.senti.trained$neu_12[i] <- BTC.senti.trained$neu[i-12]
  if (i==13){next}
  BTC.senti.trained$neu_13[i] <- BTC.senti.trained$neu[i-13]
  if (i==14){next}
  BTC.senti.trained$neu_14[i] <- BTC.senti.trained$neu[i-14]
}

# Create 14x4 sentiment features NEGATIVE
BTC.senti.trained$neg_1 <- NA # total messages on day t-1
BTC.senti.trained$neg_2 <- NA # total messages on day t-2
BTC.senti.trained$neg_3 <- NA # total messages on day t-3
BTC.senti.trained$neg_4 <- NA # total messages on day t-4
BTC.senti.trained$neg_5 <- NA # total messages on day t-5
BTC.senti.trained$neg_6 <- NA # total messages on day t-6
BTC.senti.trained$neg_7 <- NA # total messages on day t-7
BTC.senti.trained$neg_8 <- NA # total messages on day t-8
BTC.senti.trained$neg_9 <- NA # total messages on day t-9
BTC.senti.trained$neg_10 <- NA # total messages on day t-10
BTC.senti.trained$neg_11 <- NA # total messages on day t-11
BTC.senti.trained$neg_12 <- NA # total messages on day t-12
BTC.senti.trained$neg_13 <- NA # total messages on day t-13
BTC.senti.trained$neg_14 <- NA # total messages on day t-14

for (i in 1:nrow(BTC.senti.trained)){
  if (i==1){next}
  BTC.senti.trained$neg_1[i] <- BTC.senti.trained$neg[i-1]
  if (i==2){next}
  BTC.senti.trained$neg_2[i] <- BTC.senti.trained$neg[i-2]
  if (i==3){next}
  BTC.senti.trained$neg_3[i] <- BTC.senti.trained$neg[i-3]
  if (i==4){next}
  BTC.senti.trained$neg_4[i] <- BTC.senti.trained$neg[i-4]
  if (i==5){next}
  BTC.senti.trained$neg_5[i] <- BTC.senti.trained$neg[i-5]
  if (i==6){next}
  BTC.senti.trained$neg_6[i] <- BTC.senti.trained$neg[i-6]
  if (i==7){next}
  BTC.senti.trained$neg_7[i] <- BTC.senti.trained$neg[i-7]
  if (i==8){next}
  BTC.senti.trained$neg_8[i] <- BTC.senti.trained$neg[i-8]
  if (i==9){next}
  BTC.senti.trained$neg_9[i] <- BTC.senti.trained$neg[i-9]
  if (i==10){next}
  BTC.senti.trained$neg_10[i] <- BTC.senti.trained$neg[i-10]
  if (i==11){next}
  BTC.senti.trained$neg_11[i] <- BTC.senti.trained$neg[i-11]
  if (i==12){next}
  BTC.senti.trained$neg_12[i] <- BTC.senti.trained$neg[i-12]
  if (i==13){next}
  BTC.senti.trained$neg_13[i] <- BTC.senti.trained$neg[i-13]
  if (i==14){next}
  BTC.senti.trained$neg_14[i] <- BTC.senti.trained$neg[i-14]
}
########################################################################################

# Build a training and testing set ==> split into 14 days with 4 new features/day
main.df <- inner_join(price.df, BTC.senti.trained, by = 'date')
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
metrics(cmLogi) # 59%

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

# Temporary save
save.image('~/GitHub/NextBigCrypto-Senti/Models/SAT_v1_110518.RData')
# load('~/GitHub/NextBigCrypto-Senti/Models/SAT_v1_100518.RData')
