# Test run sentiment analysis models on Manual dataset
# 08.04.2018
# 19.04.2018 - Bigram test
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

# if use openxlsx
# ZIP PATH for dev tools
if(devtools::find_rtools()) Sys.setenv(R_ZIPCMD= file.path(devtools:::get_rtools_path(),"zip"))
library(openxlsx)

########################################################

# Positive	944
# Neutral	1055
# Negative	333

# Read the manual dataset (current ~2500 available messages)
manual.df <- read.xlsx('Manual_Dataset_1004_labeling.xlsx') %>%
  dplyr::select(status_id,text,processed,sentiment,trade_senti) %>%
  filter(sentiment %in% c(-1,0,1))

# Remove @ values & whitespace (extra step for new preprocessing pipeline)
manual.df$processed <- str_replace_all(manual.df$processed,"@[a-z,A-Z]*","")  
manual.df$processed <- sapply(manual.df$processed, function(x) stripWhitespace(x))

# remove left-overs
manual.df$processed <- gsub(" f ", "", manual.df$processed)
manual.df$processed <- gsub("ff", "", manual.df$processed)
manual.df$processed <- gsub("# ", "", manual.df$processed)

# Remove blank processed messages
manual.df <- manual.df[!(is.na(manual.df$processed) | manual.df$processed %in% c(""," ")), ]

########################################################
# Preprocessing

#corp <- Corpus(VectorSource(manual.df$processed))
corp <- VCorpus(VectorSource(manual.df$processed))
# Factorize sentiment target
manual.df$sentiment <- as.factor(manual.df$sentiment)

#############################
# Bi-gram 19.04.2018
BigramTokenizer <- function(x){ unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}

# Tri-gram 19.04.2018
TrigramTokenizer <- function(x){ unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)}
#############################

# Unigram
frequencies <- DocumentTermMatrix(corp)

# Change to Bigram or Trigram
#frequencies <- DocumentTermMatrix(corp, control = list(tokenize = TrigramTokenizer))

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
# k-fold validation (10)

train_control <- trainControl(## 10-fold CV
                              method = "cv",
                              number = 10)

#########################################################
# Multinomial logistic regression (baseline model)

# Set the reference group for bin to be 1
trainSparse$sentiment <- relevel(trainSparse$sentiment, ref=1)

# Load the package
library(nnet)
# Run the model
mlr.model <- multinom(sentiment ~ ., data=trainSparse,
                      MaxNWts = 10000,
                      trControl = train_control)

predict.mlr <- predict(mlr.model, newdata = testSparse[,2:ncol(testSparse)], type = "class")
length(predict.mlr)

cm.mlr <- table(testSparse$sentiment, predict.mlr)
metrics(cm.mlr) # Uni acc 58%

##############################################################
# Build a CART classification regression tree model on the training set

tweetCART_kfold <- train(sentiment~., data = trainSparse, 
                         model = "rpart", trControl = train_control)

predictCART_kfold <- predict(tweetCART_kfold, newdata = testSparse[,2:ncol(testSparse)])
cmCART_kfold <- table(testSparse$sentiment, predictCART_kfold)
metrics(cmCART_kfold) # Uni acc 62%

#####################################################
# Random Forest
tweetRF <- train(sentiment ~ ., data=trainSparse, model = "rf", trControl = train_control)

predictRF <- predict(tweetRF, newdata = testSparse[,2:ncol(testSparse)])

cmRF <-   table(testSparse$sentiment, predictRF)

metrics(cmRF) # Uni - acc 62%

###############################################
# SVM

SVM_kfold <-  train(sentiment ~ ., data=trainSparse, model = "svm", trControl=train_control)

predictSVM_kfold <- predict(SVM_kfold, newdata = testSparse[,2:ncol(testSparse)])

cmSVM_kfold <-   table(testSparse$sentiment, predictSVM_kfold)

metrics(cmSVM_kfold) # Uni acc 63%

#############################################################
# Naive Bayes

NBayes <- train(sentiment ~., data = trainSparse, laplace = 3, model = "nb", trControl = train_control)

predictionsNB <- predict(NBayes,  newdata = testSparse[,2:ncol(testSparse)])

cmNB <- table(testSparse$sentiment, predictionsNB)

metrics(cmNB) # Uni - acc 63%

###############################################

###############################################

# H2O GBM
h2o.init()

# Build a training and testing set for H2O environment

trainH2O <- as.h2o(trainSparse)
testH2O <- as.h2o(testSparse[,2:ncol(testSparse)])

# Train GBM model
gbm.model <- h2o.gbm(  training_frame = trainH2O,
                       #validation_frame = testH2O,
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

metrics(cmGBM) # Uni - acc 60%

###################################################################
# H2O DRF

h2o_drf <- h2o.randomForest(    
  training_frame = trainH2O,
  #validation_frame = testH2O,
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

metrics(cmDRF) # Uni - acc 63%

####################
#                  #
#  PACKAGES MODEL  #
#                  #
####################
#BingLiu Lexicon

#Pulling in positive and negative wordlists
#BingLiu
pos.words <- scan('./0. Datasets/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
neg.words <- scan('./0. Datasets/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
#Adding words to positive and negative databases
pos.words=c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'thx' ,
            'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader','pump',
            'rocket','ath','bullish','bull')
neg.words = c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not',
              'FUD','FOMO','bearish','dump','fear','atl','bear','wth','shit','fuck','dumb',
              'weakhands','blood','bloody','scam','con-artist','liar','vaporware','shitfork')

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
result <- mutate(result, status_id, sentiment = ifelse(result$score > 0, 1, 
                                                ifelse(result$score < 0, -1, 0)))

cmBL <- table(manual.df$sentiment,result$sentiment)
metrics(cmBL) # acc 57%

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
metrics(cm_nrc) # acc 47%

######################################
# Syuzhet
test.syuzhet2 <- syuzhet::get_sentiment(as.character(manual.df$processed))

test.syuzhet2 <- as.data.frame(test.syuzhet2)

test.syuzhet2$sent <- ifelse(test.syuzhet2$test.syuzhet2 < 0,-1,
                             ifelse(test.syuzhet2$test.syuzhet2 == 0 ,0,1))

test.syuzhet2$sent <- as.factor(test.syuzhet2$sent)

#confusionMatrix(manual.df$sentiment, test.syuzhet2$sent)
cm_syuzhet <- table(manual.df$sentiment, test.syuzhet2$sent)
metrics(cm_syuzhet) # acc 53%

######################################
# Syuzhet
test.syuzhet3 <- syuzhet::get_sentiment(as.character(manual.df$processed), method = "afinn")

test.syuzhet3 <- as.data.frame(test.syuzhet3)

test.syuzhet3$sent <- ifelse(test.syuzhet3$test.syuzhet3 < 0,-1,
                             ifelse(test.syuzhet3$test.syuzhet3 == 0 ,0,1))

test.syuzhet3$sent <- as.factor(test.syuzhet3$sent)

#confusionMatrix(manual.df$sentiment, test.syuzhet3$sent)
cm_afinn <- table(manual.df$sentiment, test.syuzhet3$sent)
metrics(cm_afinn) # acc 55%

######################################
# SentimentR

test.sentimentR <- sentimentr::sentiment_by(as.character(sentimentr::get_sentences(manual.df$processed)))
test.sentimentR$sent <- ifelse(test.sentimentR$ave_sentiment == 0, 0, 
                               ifelse(test.sentimentR$ave_sentiment > 0 , 1, -1)) # translate sentiments back to the original training data

test.sentimentR$sent <- as.factor(test.sentimentR$sent)

confusionMatrix(manual.df$sentiment, test.sentimentR$sent)
cm_sentimentr <- table(manual.df$sentiment, test.sentimentR$sent)
metrics(cm_sentimentr) # acc 51%

#########################################################
# MAJORITY VOTING (packages)

major.packages <- as.data.frame(as.factor(manual.df$sentiment))
major.packages <- cbind(major.packages,       # target variable
                        as.factor(result$sentiment),     # Bing Liu lexicon
                        as.factor(test.syuzhet$sent),    # NRC lexicon
                        as.factor(test.syuzhet2$sent),   # Syuzhet
                        as.factor(test.syuzhet3$sent),   # AFINN lexicon
                        as.factor(test.sentimentR$sent)) # Sentiment R

colnames(major.packages) <- c("Sentiment","BingLiu","NRC","Syuzhet","AFINN","SentimentR")

# The majority vote
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

# Calculating majority votes
for (i in 1:nrow(major.packages)){
  major.packages$Major[i] <- find_major(major.packages,i)
}

cmMAJOR.packages <- table(major.packages$Sentiment, major.packages$Major)

metrics(cmMAJOR.packages) # acc 52%

#########################################################
# MAJORITY VOTING (trained models)

finaldf <- cbind(testSparse$sentiment,
                 as.data.frame(predictRF),         # Random Forest
                 as.data.frame(predictCART_kfold), # Classification and Regression Trees
                 as.data.frame(predictSVM_kfold),  # Support Vector Machine
                 predictionsDRF$predict,           # Distributed Random Forest
                 predictionsGBM$predict,           # Gradient Boosting Machine
                 as.data.frame(predictionsNB))     # Naive Bayes

#colnames(finaldf) <- c("Sentiment","RF","CART","SVM","DRF","GBM","NB","BingLiu","Syuzhet_NRC","SentimentR")
colnames(finaldf) <- c("Sentiment","RF","CART","SVM","DRF","GBM","NB")

# Calculating majority votes
for (i in 1:nrow(finaldf)){
  finaldf$Major[i] <- find_major(finaldf,i)
}

cmMAJOR <- table(finaldf$Sentiment, finaldf$Major)

metrics(cmMAJOR) # Uni - acc 62%

#save.image('./Models/Senti_Manual_UniGram_2018-04-24.RData')
