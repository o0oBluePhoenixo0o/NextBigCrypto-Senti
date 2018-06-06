# 27.05.2018
# SAT only (no price)
# Loop for generating results day t-1 ==> t-14 (comparing 6h/12h/24h on Accuracy & F1-score)

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
              "stringi", #string manipulation
              "tidyquant", "openxlsx","anytime",
              "tm", #text mining package
              "caTools","caret", "rpart", "h2o","e1071","RWeka","randomForest") # machine learning packages

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

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
#######################################################
# Load new 27.05 BTC.senti

BTC.senti <- read_csv('~/GitHub/NextBigCrypto-Senti/0. Datasets/BTC_clean_senti_2705.csv')
BTC.senti$status_id <- as.character(BTC.senti$status_id)
BTC.senti$user_id <- as.character(BTC.senti$user_id)

############################
# BTC.clean <- read_csv('~/GitHub/NextBigCrypto-Senti/0. Datasets/BTC_clean_2205.csv')
# 
# BTC.clean$status_id <- as.character(BTC.clean$status_id)
# BTC.clean$user_id <- as.character(BTC.clean$user_id)
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
# }
# h2o.shutdown()
# 
# BTC.senti <- BTC.senti %>%
#   mutate(date = as_datetime(created_at))%>%
#   select(date, status_id, user_id, screen_name, text, processed, 
#          botprob, sentiment.trained)


##########################
# load price dataset     #
##########################
token_name <- 'BTC'

price.df <- readxl::read_xlsx('~/GitHub/NextBigCrypto-Senti/1. Crawlers/Historical_Data_HR.xlsx') %>%
  filter(symbol == token_name) %>%
  dplyr::select(-date.time)

###################
bk <- price.df

final.result <- data.frame('Type_hr' = character(),
                           'Time' = character(),
                           'Accuracy' = numeric(),
                           'F1_score' = numeric(), 
                           'Algo' = character())
time.set <- c(6,12,24)


###############
#
# MAIN LOOP 
#
###############

for (y in 1:length(time.set)){
  
  time.slot <- time.set[y] # insert trigger
  price.df <- bk           # get backup for price.df
  
  # Summarize base on sentiment each day
  # Filter Senti Trained base on time.slot
  BTC.senti.trained <- BTC.senti %>% 
    dplyr::select(date,sentiment.trained) %>%
    group_by(time = floor_date(date, paste0(time.slot,' hour')),
             sentiment.trained) %>%
    summarize(count = n()) %>% 
    group_by(time) %>% 
    mutate(countT = sum(count)) %>%
    group_by(sentiment.trained) %>%
    mutate(per = round(100* count/countT,2))
  
  # Convert to each sentiment = column
  BTC.senti.trained <- reshape2::dcast(BTC.senti.trained, time + countT ~ sentiment.trained,
                                       value.var = 'per')
  colnames(BTC.senti.trained) <- c('time','count','neg','neu','pos')
  
  #########################################
  # Price.df
  
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
  
  for (z in 1:14){
    
    x <- z/(time.slot/24)
    
    ## SAT Model only no need for price t-1 => t-14
    # for (i in 1:x){
    #   eval(parse(text = paste0('price.df$t_', i,' <- NA')))
    # }
    # 
    # for (i in 1:nrow(price.df)){
    #   for (j in 1:x){
    #     eval(parse(text = paste0('price.df$t_', j,' <- as.factor(lag(price.df$bin,',j,'))')))
    #   }
    # }
    
    # Convert to categorical variables
    price.df$bin <- as.factor(price.df$bin)
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
    
    # Build a training and testing set
    main.df <- inner_join(price.df, BTC.senti.trained, by = 'time')
    main.df <- unique(main.df)
    # Build a training and testing set.
    main.df <- main.df %>%
      dplyr::select(-time,-close,-diff,-pricediff,
                    -count,-neg,-neu,-pos,-priceBTC)
    
    # Remove NA 
    main.df <- main.df[complete.cases(main.df),]
    
    # Split random
    set.seed(1908)
    split <- sample.split(main.df$bin, SplitRatio=0.8) #bin is target variable
    train <- subset(main.df, split==TRUE)
    test <- subset(main.df, split==FALSE)
    
    gc()
    ##############################
    # k-fold validation (10)
    train_control <- trainControl(## 10-fold CV
      method = "cv",
      number = 10)
    
    #################################
    # Base-line model (GLM)
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
    
    ###############################################
    # Accuracy
    acc.mLogi <- ifelse(is.na(max(metrics(cmLogi)$avgAccuracy)),0,max(metrics(cmLogi)$avgAccuracy))
    acc.mSVM <- ifelse(is.na(max(metrics(cmSVM)$avgAccuracy)),0,max(metrics(cmSVM)$avgAccuracy))
    acc.mC50 <- ifelse(is.na(max(metrics(cmC50)$avgAccuracy)),0,max(metrics(cmC50)$avgAccuracy))
    acc.mNB <- ifelse(is.na(max(metrics(cmNB)$avgAccuracy)),0,max(metrics(cmNB)$avgAccuracy))
    acc.mRF <- ifelse(is.na(max(metrics(cmRF)$avgAccuracy)),0,max(metrics(cmRF)$avgAccuracy))
    
    acc <- max(acc.mLogi,acc.mSVM,acc.mC50,acc.mNB,acc.mRF)
    
    # F1 score
    f1.mLogi <- ifelse(is.na(max(metrics(cmLogi)$macroF1)),0,max(metrics(cmLogi)$macroF1))
    f1.mSVM <- ifelse(is.na(max(metrics(cmSVM)$macroF1)),0,max(metrics(cmSVM)$macroF1))
    f1.mC50 <- ifelse(is.na(max(metrics(cmC50)$macroF1)),0,max(metrics(cmC50)$macroF1))
    f1.mNB <- ifelse(is.na(max(metrics(cmNB)$macroF1)),0,max(metrics(cmNB)$macroF1))
    f1.mRF <- ifelse(is.na(max(metrics(cmRF)$macroF1)),0,max(metrics(cmRF)$macroF1))
    
    f1 <- max(f1.mLogi,f1.mSVM,f1.mC50,f1.mNB,f1.mRF)
    
    model <- which.max(c(f1.mLogi,
                         f1.mSVM,
                         f1.mC50,
                         f1.mNB,
                         f1.mRF))
    
    if (model == 1){model <- 'Logi'}
    if (model == 2){model <- 'SVM'}
    if (model == 3){model <- 'C50'}
    if (model == 4){model <- 'NB'}
    if (model == 5){model <- 'RF'}
    
    # Add results to final dataset
    result <- as.data.frame(cbind(paste0(time.set[y],'hr'),z,acc,f1,model))
    colnames(result) <- colnames(final.result)
    
    final.result <- rbind(final.result,result)
    
    print(paste0('Complete model type ',time.set[y],'-hr on time t-',z,'. Best model is ',model,' with ',acc,' accuracy and ',f1,' F1-score.'))
    gc() # garbage collection
  }
}

# Save final result
write.xlsx(final.result,'~/GitHub/NextBigCrypto-Senti/3. Models Development/SAT_result.xlsx')

# Save model
save.image('~/GitHub/NextBigCrypto-Senti/Models/SAT_LOOP_2705.RData')
#load('~/GitHub/NextBigCrypto-Senti/Models/SAT_LOOP_2705.RData')
