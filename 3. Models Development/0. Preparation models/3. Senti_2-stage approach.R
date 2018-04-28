# Test run sentiment analysis models on Manual dataset
# 08.04.2018
# 26.04.2018 - SVM Linear + lemmaz
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
# manual.df %>% group_by(sentiment) %>%tally # overall observation

# Positive	1056
# Neutral	1256
# Negative	356

manual.df <- read_csv('Manual_Dataset_2504.csv') %>%
  dplyr::select(status_id,text,processed,sentiment,trade_senti) %>%
  filter(sentiment %in% c(-1,0,1))

# Lemmatization 26.04.18
manual.df$processed <- sapply(manual.df$processed, function(x) textstem::lemmatize_strings(x))

manual.df$status_id <- as.character(manual.df$status_id)

########################################################
# Preprocessing (1-gram)

corp <- Corpus(VectorSource(manual.df$processed))
manual.df$sentiment <- as.factor(manual.df$sentiment)

# Extract frequent terms (unigram)
frequencies <- DocumentTermMatrix(corp)

# # TF-IDF weighting 16.04.2018 (2-stage)
# frequencies <- DocumentTermMatrix(corp,
#                                   control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

# Remove these words that are not used very often. Keep terms that appear in 0.5% or more of the dataset
sparse <- removeSparseTerms(frequencies, 0.995)

ResultSparse <- as.data.frame(as.matrix(sparse))
colnames(ResultSparse) <- make.names(colnames(ResultSparse))

# SVM (2-stage)
# Add senti_sub (subjectivity detection)
manual.df$senti_sub <- as.factor(ifelse(manual.df$sentiment == '0','objective','subjective'))

# Add senti_pol (polarity detection)
manual.df$senti_pol <- as.factor(ifelse(manual.df$sentiment == '1','positive',
                                 ifelse(manual.df$sentiment == '-1','negative',NA)))

# Add objectivity senti column to the matrix
ResultSparse <- cbind(senti_sub = manual.df$senti_sub,ResultSparse)

# Build a training and testing set.
set.seed(1908)

split <- sample.split(ResultSparse$senti_sub, SplitRatio=0.8)
trainSparse <- subset(ResultSparse, split==TRUE)
testSparse <- subset(ResultSparse, split==FALSE)

#####################################################
# Polarity dataset separation

ResultSparse.pol <- ResultSparse[,2:ncol(ResultSparse)]
ResultSparse.pol <- cbind(senti_pol = manual.df$senti_pol,ResultSparse.pol)

ResultSparse.pol <- ResultSparse.pol %>% 
  filter(is.na(senti_pol) == FALSE)

# Build a training and testing set.
set.seed(1908)

split <- sample.split(ResultSparse.pol$senti_sub, SplitRatio=0.8)
trainSparse.pol <- subset(ResultSparse.pol, split==TRUE)
testSparse.pol <- subset(ResultSparse.pol, split==FALSE)

# Train control
# k-fold validation (10)
train_control <- trainControl(method="cv", number = 10)

####################################################
# Function to calculate accuracy/prediction/recall #  
####################################################

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

#########################################
#                                       #
#   SUBJECTIVITY DETECTION (STAGE 1)    #
#                                       #
#########################################


#########################################
# SVM - Linear Kernel

SVM_lk <- train(senti_sub ~.,
                data = trainSparse,
                model = "svmLinear",
                trControl = train_control)

predict.SVM_lk <- predict(SVM_lk, newdata = testSparse[,2:ncol(testSparse)])

cm.SVM_lk <- table(testSparse$senti_sub, predict.SVM_lk)

metrics(cm.SVM_lk) # acc 74%

#####################################
#                                   #
#   POLARITY DETECTION (STAGE 2)    #
#                                   #
#####################################




#############################################################
# Naive Bayes

NBayes_sub <- train(senti_sub ~., 
                data = trainSparse, 
                laplace = 1, 
                model = "nb", 
                trControl = train_control)

predictionsNB <- predict(NBayes_sub, as.data.frame(testSparse[,2:ncol(testSparse)]))

cmNB_sub <- table(testSparse$senti_sub, predictionsNB)

metrics(cmNB_sub) #67% - term freq / 71% - tf-idf

###############################################

# H2O GBM
h2o.init()

# Build a training and testing set for H2O environment

trainH2O <- as.h2o(trainSparse)
testH2O <- as.h2o(testSparse)

# Train GBM model
gbm.model <- h2o.gbm(  training_frame = trainH2O,
                       validation_frame = testH2O,
                       x=2:ncol(trainSparse),            
                       y=1,         
                       ntrees = 500, 
                       max_depth = 50, 
                       learn_rate = 0.3, 
                       nfolds = 10,
                       seed = 1234)
#model_path <- h2o.saveModel(object=gbm.model, path=getwd(), force=TRUE)
#print(model_path)
predictionsGBM <- as.data.frame(h2o.predict(gbm.model,testH2O))

cmGBM <-   table(testSparse$senti_sub, predictionsGBM$predict)

confusionMatrix(testSparse$senti_sub, predictionsGBM$predict)

metrics(cmGBM) #66% - term freq

###################################################################
# H2O DRF

h2o_drf <- h2o.randomForest(    
  training_frame = trainH2O,
  validation_frame = testH2O,
  x=2:ncol(trainSparse),            
  y=1,                          
  ntrees = 500,                 # Increase max trees to 500 
  max_depth = 30,               # Increase depth, from 20
  nbins_cats = 5000,
  nfolds = 10, 
  seed = 1234)                  #

#model_path <- h2o.saveModel(object=h2o_drf, path=getwd(), force=TRUE)
#print(model_path)
predictionsDRF <- as.data.frame(h2o.predict(h2o_drf,testH2O))
cmDRF <- table(testSparse$senti_sub, predictionsDRF$predict)

confusionMatrix(testSparse$senti_sub, predictionsDRF$predict)

metrics(cmDRF) #70% - term freq / 71% - tf-idf




  