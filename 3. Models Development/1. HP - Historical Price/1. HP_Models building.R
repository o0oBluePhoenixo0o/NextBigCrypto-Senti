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

price.df <- read_csv("./1. Crawlers/Crypto-Markets_2018-04-16.csv") %>%
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
dat <- cbind(price.df$date,dat)
colnames(dat) <- c('date', 'pricediff')

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

###############################

# Split into 4 different bins: strong up/ up / down / strong down
dat$bin <- as.numeric(cut2(dat$pricediff, g=4))

# check distribution
dat %>% group_by(bin) %>% tally

###############################
# Preparation
price.df$cut <- as.numeric(cut2(price.df$pricediff, g=4))

# Assigning bin
price.df$bin <- ifelse(price.df$cut == 1,'strong down',
                       ifelse(price.df$cut == 2,'down',
                              ifelse(price.df$cut ==3,'up','strong up')))

price.df$t_1 <- NA # price movement on day t-1
price.df$t_2 <- NA # price movement on day t-2
price.df$t_3 <- NA # price movement on day t-3
price.df$t_4 <- NA # price movement on day t-4
price.df$t_5 <- NA # price movement on day t-5
price.df$t_6 <- NA # price movement on day t-6
price.df$t_7 <- NA # price movement on day t-7


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
}

# Convert to categorical variables
price.df$t_1 <- as.factor(price.df$t_1)
price.df$t_2 <- as.factor(price.df$t_2)
price.df$t_3 <- as.factor(price.df$t_3)
price.df$t_4 <- as.factor(price.df$t_4)
price.df$t_5 <- as.factor(price.df$t_5)
price.df$t_6 <- as.factor(price.df$t_6)
price.df$t_7<- as.factor(price.df$t_7)

# Build a training and testing set.
set.seed(1908)

main.df <- price.df[c('bin','t_1','t_2','t_3','t_4','t_5','t_6','t_7')]

split <- sample.split(main.df$bin, SplitRatio=0.8) #bin is target variable
train <- subset(main.df, split==TRUE)
test <- subset(main.df, split==FALSE)
# delete 1st row of test data
test <- test[-1,]

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
#train_control <- trainControl(method="cv", number=10)
train_control <- trainControl(## 10-fold CV
                              method = "repeatedcv",
                              number = 10,
                              ## repeated ten times
                              repeats = 10)
#################################
# Base-line model 
library(MASS)

# Since we have 4 classes which is close together ==> ordinal logistic regression model (test)
# Turn the apply variable into a factor
train$bin <- factor(train$bin)

# Run the ordinal logistic regression model
model <- polr(bin ~ ., data=train)
summary(model)
coefs <- coef(model)
# Display coefficients
coefs
# Raise e to the coefficients
exp(coefs)

# Load the library
library(brant)
# Run the Brant test on the model
brant(model) #Omnibus prob < 0.05 ==> ordinal logistic regression fails

################################################
# Multinomial logistic regression

# Set the reference group for bin to be 1
train$bin <- relevel(train$bin, ref=1)

# Load the package
library(nnet)
# Run the model
mlr.model <- train(bin ~ ., 
                   data=train, 
                   method = 'multinom',
                   na.action = na.exclude,
                   trControl = train_control)

# Summary the model
summary(mlr.model)
# Get coefficients
coefs <- coef(mlr.model)

# # Raise e to the coefficients
# exp(coefs)
# # Do the full transformation, all in one line
# (exp(coefs)-1)*100

# Prediction
predict.mlr <- predict(mlr.model, newdata = test, type = "raw")

# Confusion Matrix
cm.mlr <- table(test$bin,predict.mlr)

metrics(cm.mlr) # acc 31%

########################################
# Naive Bayes

NBayes <- train(bin ~., data = train, 
                laplace = 1, model = "nb",
                na.action = na.exclude,
                trControl = train_control)

predictionsNB <- predict(NBayes, newdata=test)

cmNB <- table(test$bin, predictionsNB)

metrics(cmNB) # acc 28%

# Recursive feature elimination (via caret package)
set.seed(1908)

ctrl <- rfeControl(functions = nbFuncs, #naive bayes
                   method = "repeatedcv",
                   repeats = 10,
                   verbose = FALSE)
?rfe

lmProfile <- rfe(bin ~ ., # formula 
                 train, 
                 sizes = 1:5, #retain from 1-4 features
                 rfeControl =  ctrl)

lmProfile
predictors(lmProfile)

lmProfile$fit
head(lmProfile$resample)

trellis.par.set(caretTheme())
plot(lmProfile, type = c("g", "o")) # improve 5$ accuracy
