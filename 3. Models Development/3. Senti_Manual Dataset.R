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

# Positive	944
# Neutral	1055
# Negative	333

# Read the manual dataset (current ~2000 available messages)
manual.df <- read.xlsx('Manual_Dataset_1004_labeling.xlsx') %>%
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

# TF-IDF weighting 09.04.2018 (lower acc)
#frequencies <- DocumentTermMatrix(corp,
#                                  control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

# Remove these words that are not used very often. Keep terms that appear in 0.5% or more of the dataset
sparse <- removeSparseTerms(frequencies, 0.995)

ResultSparse <- as.data.frame(as.matrix(sparse))
colnames(ResultSparse) <- make.names(colnames(ResultSparse))

# Add sentiment column to the matrix
ResultSparse <- cbind(sentiment = manual.df$sentiment,ResultSparse)

# Build a training and testing set.
set.seed(1908)

split <- sample.split(ResultSparse$sentiment, SplitRatio=0.8)
trainSparse <- subset(ResultSparse, split==TRUE)
testSparse <- subset(ResultSparse, split==FALSE)

# Test set for prebuilt packages
test <- as.data.frame(subset(manual.df$processed,split==FALSE))
colnames(test) <- 'processed'

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
# k-fold validation (5)
train_control <- trainControl(method="cv", number=5)

##############################################################
# Build a CART classification regression tree model on the training set

tweetCART_kfold <- train(sentiment~., data = trainSparse, model = "rpart", trControl = train_control)

predictCART_kfold <- predict(tweetCART_kfold, newdata=testSparse)
cmCART_kfold <- table(testSparse$sentiment, predictCART_kfold)
metrics(cmCART_kfold) # 65% / avg 76%

#####################################################
# Random Forest
tweetRF <- train(sentiment ~ ., data=trainSparse, model = "rf", trControl = train_control)

predictRF <- predict(tweetRF, newdata=testSparse)

cmRF <-   table(testSparse$sentiment, predictRF)

metrics(cmRF) #65% / avg Acc 77%

###############################################
# SVM

SVM_kfold <-  train(sentiment ~ ., data=trainSparse, model = "svm", trControl=train_control)

predictSVM_kfold <- predict(SVM_kfold, newdata=testSparse)

cmSVM_kfold <-   table(testSparse$sentiment, predictSVM_kfold)

metrics(cmSVM_kfold) #65% / avg Acc 76%

#############################################################
# Naive Bayes

NBayes <- train(sentiment ~., data = trainSparse, laplace = 3, model = "nb", trControl = train_control)

predictionsNB <- predict(NBayes, as.data.frame(testSparse))

cmNB <- table(testSparse$sentiment, predictionsNB)

metrics(cmNB) #65% / avg Acc 76%

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
model_path <- h2o.saveModel(object=gbm.model, path=getwd(), force=TRUE)
print(model_path)
predictionsGBM <- as.data.frame(h2o.predict(gbm.model,testH2O))

cmGBM <-   table(testSparse$sentiment, predictionsGBM$predict)

metrics(cmGBM) #62% / avg acc 74%

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

model_path <- h2o.saveModel(object=h2o_drf, path=getwd(), force=TRUE)
print(model_path)
predictionsDRF <- as.data.frame(h2o.predict(h2o_drf,testH2O))
cmDRF <- table(testSparse$sentiment, predictionsDRF$predict)

metrics(cmDRF) #69% / avg acc 79%

#####################################
#BingLiu Lexicon

#Pulling in positive and negative wordlists
#BingLiu
pos.words <- scan('./0. Datasets/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
neg.words <- scan('./0. Datasets/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
#Adding words to positive and negative databases
pos.words=c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 
            'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader','pump',
            'rocket','ath','bullish','bull')
neg.words = c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not',
              'FUD','FOMO','bearish','dump','fear','atl')

#evaluation function
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  scores <- plyr::laply(sentences, function(sentence, pos.words, neg.words){
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    #convert to lower-case and remove punctuations with numbers
    sentence <- removePunctuation(removeNumbers(tolower(sentence)))
    removeURL <- function(x) gsub('"(http.*) |(http.*)$|\n', "", x)
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

scores <- score.sentiment(manual.df$text, pos.words, neg.words, .progress='text')
result <- scores

#Add ID to result set
result$status_id <- manual.df$status_id
#add new scores as a column
result <- mutate(result, status_id, sentiment = ifelse(result$score > 0, 'Positive', 
                                                ifelse(result$score < 0, 'Negative', 'Neutral')))

cmBL <- table(manual.df$sentiment,result$sentiment)
metrics(cmBL) # 53% / avg acc 69%

######################################
# Syuzhet package
# NRC lexicon
test.syuzhet <- syuzhet::get_nrc_sentiment(as.character(manual.df$processed))
test.syuzhet <- as.data.frame(test.syuzhet[,9:10])
test.syuzhet$sent <- ifelse((test.syuzhet$positive - test.syuzhet$negative) == 0, 
                            0, 
                            ifelse(test.syuzhet$positive - test.syuzhet$negative > 0 , 
                                   1, -1))# translate sentiments back to the original training data
test.syuzhet$sent <- as.factor(test.syuzhet$sent)

#confusionMatrix(manual.df$sentiment, test.syuzhet$sent)
cm_nrc <- table(manual.df$sentiment, test.syuzhet$sent)
metrics(cm_nrc)

######################################
# SentimentR

test.sentimentR <- sentimentr::sentiment_by(as.character(sentimentr::get_sentences(manual.df$processed)))
test.sentimentR$sent <- ifelse(test.sentimentR$ave_sentiment == 0, 0, 
                               ifelse(test.sentimentR$ave_sentiment > 0 , 1, -1)) # translate sentiments back to the original training data

test.sentimentR$sent <- as.factor(test.sentimentR$sent)

confusionMatrix(manual.df$sentiment, test.sentimentR$sent)
cm_sentimentr <- table(manual.df$sentiment, test.sentimentR$sent)
metrics(cm_sentimentr)

################################################################################
#######################################################################################################
# MAJORITY VOTING #

finaldf <- cbind(testSparse$sentiment,
                 as.data.frame(predictRF), #Random Forest
                 as.data.frame(predictCART_kfold), #Classification and Regression Trees
                 as.data.frame(predictSVM_kfold), #Support Vector Machine
                 predictionsDRF$predict, # Distributed Random Forest
                 predictionsGBM$predict, #Gradient Boosting Machine
                 as.data.frame(predictionsNB)) #Naive Bayes
                 # result$sentiment, #BingLiu
                 # test.syuzhet$sent, #Syuzhet package
                 # test.sentimentR$sent) #sentimentR package

#colnames(finaldf) <- c("Sentiment","RF","CART","SVM","DRF","GBM","NB","BingLiu","Syuzhet_NRC","SentimentR")
colnames(finaldf) <- c("Sentiment","RF","CART","SVM","DRF","GBM","NB")

#The majority vote
find_major <- function(df,x){
  a <- df[x,]
  neg <- 0
  pos <- 0
  neu <- 0
  if (names(df[1]) == 'Sentiment'){j <- 2} else {j<-1}
  for (i in j:ncol(a)){
    if (a[i] == 'Negative'| a[i] == '-1'){neg <- neg + 1}
    else if (a[i] == 'Positive'| a[i] == '1'){pos <- pos + 1}
    else if (a[i] == 'Neutral'| a[i] == '0'){neu <- neu + 1}
  }
  result <- c("Positive", "Negative", "Neutral")[which.max(c(pos,neg,neu))]
}

for (i in 1:nrow(finaldf)){
  finaldf$Major[i] <- find_major(finaldf,i)
}

cmMAJOR <- table(finaldf$Sentiment, finaldf$Major)

metrics(cmMAJOR) # 66% / avg acc 77%

save.image('./Models/Senti_Manual_Dataset.RData')
