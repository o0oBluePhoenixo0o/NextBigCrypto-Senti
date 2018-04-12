# Test run sentiment analysis models on Manual dataset
# 08.04.2018

# clear the environment
rm(list= ls())

# load packages and set options
options(stringsAsFactors = FALSE)

#Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti/")

#https://github.com/SebastianKirsch123/ensemble_sentiment_classification/blob/master/Ensemble_Sentiment_Classification.pdf

# install packages if not available
packages <- c("readr", #read data
              "lubridate", #date time conversion
              "dplyr", #date manipulation
              "tm", # text mining package
              "textmineR",
              "tidytext",
              "topicmodels",
              "ggplot2", # plotting package
              "quanteda", #kwic function search phrases
              "xtable", "DT", #viewing data type from quanteda
              "stringi", #string manipulation
              "wordcloud","tidyquant",
              "caTools","caret", "rpart", "h2o","e1071","RWeka",
              "randomForest"
)
#remove.packages(c("tidytext", "ggplot2"))
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

# if use openxlsx
# ZIP PATH for dev tools
if(devtools::find_rtools()) Sys.setenv(R_ZIPCMD= file.path(devtools:::get_rtools_path(),"zip"))
library(openxlsx)

########################################################

# Read the manual dataset (current ~2000 available messages)
manual.df <- read.xlsx('Manual_Dataset_0804_labeling.xlsx') %>%
  select(status_id,text,processed,sentiment,trade_senti) %>%
  filter(sentiment %in% c(-1,0,1))

# Remove @ values & whitespace (extra step for new preprocessing pipeline)
manual.df$processed <- str_replace_all(manual.df$processed,"@[a-z,A-Z]*","")  
manual.df$processed <- sapply(manual.df$processed, function(x) stripWhitespace(x))

# Remove blank processed messages
manual.df <- manual.df[!(is.na(manual.df$processed) | manual.df$processed %in% c(""," ")), ]

########################################################
# Preprocessing

corp <- Corpus(VectorSource(manual.df$processed))
manual.df$sentiment <- as.factor(manual.df$sentiment)

# Extract frequent terms
frequencies <- DocumentTermMatrix(corp)

# TF-IDF weighting 09.04.2018
#frequencies <- DocumentTermMatrix(corp,
#                                  control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

# Remove these words that are not used very often. Keep terms that appear in 0.5% or more of the dataset
sparse <- removeSparseTerms(frequencies, 0.995)

ResultSparse <- as.data.frame(as.matrix(sparse))
colnames(ResultSparse) <- make.names(colnames(ResultSparse))

frequencies

sparse


# SVM (2-stage)
# Add senti_sub (subjectivity detection)
manual.df$senti_sub <- as.factor(ifelse(manual.df$sentiment == '0','objective','subjective'))

# Add senti_pol (polarity detection)
manual.df$senti_pol <- as.factor(ifelse(manual.df$sentiment == '1','positive',
                                 ifelse(manual.df$sentiment == '-1','negative',NA)))

# Add objectivity senti column to the matrix
ResultSparse <- cbind(senti_sub = manual.df$senti_sub,ResultSparse)

# Add later (polarity)
#ResultSparse <- cbind(senti_pol = manual.df$senti_pol,ResultSparse)

# Build a training and testing set.
set.seed(1908)

split <- sample.split(ResultSparse$senti_sub, SplitRatio=0.8)
trainSparse <- subset(ResultSparse, split==TRUE)
testSparse <- subset(ResultSparse, split==FALSE)

# k-fold validation (5)
train_control <- trainControl(method="cv", number=5)

###############################################
# SVM (stage 1 - subjectivity detection)

SVM_sub_kfold <- svm(formula = senti_sub~., 
                     data = trainSparse, 
                     cross = 10, #5-fold validation
                     type = 'C-classification', 
                     kernel = 'linear')

predictSVM_sub_kfold <- predict(SVM_sub_kfold, newdata=testSparse)

cmSVM_sub_kfold <-   table(testSparse$senti_sub, predictSVM_sub_kfold)

metrics(cmSVM_sub_kfold) #65% / avg Acc 76%


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
#########################################################
  