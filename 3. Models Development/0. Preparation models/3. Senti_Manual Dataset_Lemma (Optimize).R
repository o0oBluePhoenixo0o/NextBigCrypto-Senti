# # Lemmatization
# load('./Models/Senti_Manual_TriGram_Lemma_2018-04-26.RData')
# load('./Models/Senti_Manual_BiGram_Lemma_2018-04-26.RData')
# load('./Models/Senti_Manual_UniGram_Lemma_2018-04-26.RData')
# 
# # clear the environment
# rm(list= ls())
# 
# # Non-lemmatizaion
# load('./Models/Senti_Manual_TriGram_2018-04-26.RData')
# load('./Models/Senti_Manual_BiGram_2018-04-26.RData')
# load('./Models/Senti_Manual_UniGram_2018-04-26.RData')
# 
# # Trained models
# metrics(cm.mlr)
# metrics(cmCART_kfold) 
# metrics(cmRF)
# metrics(cmSVM_kfold)
# metrics(cmNB)
# metrics(cm.C50)
# metrics(cmGBM)
# metrics(cmDRF)
# metrics(cmMAJOR)
# # Packages
# metrics(cmBL) 
# metrics(cm_nrc) 
# metrics(cm_syuzhet)
# metrics(cm_afinn)
# metrics(cm_sentimentr)
# metrics(cmMAJOR.packages)

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
              "ggplot2", # plotting package
              "quanteda", #kwic function search phrases
              "stringi", #string manipulation
              "wordcloud","tidyquant",
              "caTools","caret", "rpart", "h2o","e1071","RWeka",
              "randomForest"
)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

# Pick Unigram - Lemma as case for DRF + GBM optimization
# load('~/GitHub/NextBigCrypto-Senti/Models/Senti_Manual_Uni_TFIDF_working.RData')


########################################################
# Read the manual dataset (current ~2700 available messages)
# manual.df <- read.xlsx('Manual_Dataset_1004_labeling.xlsx') %>%
#   dplyr::select(status_id,text,processed,sentiment,trade_senti) %>%
#   filter(sentiment %in% c(-1,0,1))

manual.df <- read_csv('Manual_Dataset_0805.csv') %>%
  dplyr::select(status_id,text,processed,sentiment) %>%
  filter(sentiment %in% c(-1,0,1))

manual.df$status_id <- as.character(manual.df$status_id)

manual.df %>% group_by(sentiment) %>%tally # overall observation

# 01.05.2018
# Positive	1195
# Neutral	  1501
# Negative	492

########################################################
# Preprocessing

#corp <- Corpus(VectorSource(manual.df$processed))
corp <- VCorpus(VectorSource(manual.df$processed)) # VCorpus compatible with n-gram analysis
# Factorize sentiment target
manual.df$sentiment <- as.factor(manual.df$sentiment)

#############################
# Bi-gram 19.04.2018
BigramTokenizer <- function(x){ unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}

# Tri-gram 19.04.2018
TrigramTokenizer <- function(x){ unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)}
#############################

# Unigram
#frequencies <- DocumentTermMatrix(corp)

# Change to Bigram or Trigram
#frequencies <- DocumentTermMatrix(corp, control = list(tokenize = TrigramTokenizer))

# TF-IDF weighting 09.04.2018 (lower acc)
frequencies <- DocumentTermMatrix(corp,
                                  control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

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

h2o.init()

fullH2O <- as.h2o(ResultSparse) # full set
trainH2O <- as.h2o(trainSparse) # train set
testH2O <- as.h2o(testSparse) # test set

#######################################################
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

##########################################################################
# Train GBM and DRF model
gbm.model <- h2o.gbm(  training_frame = trainH2O,
                       #validation_frame = testH2O,
                       x=2:ncol(trainH2O),
                       y=1,
                       ntrees = 1000,
                       max_depth = 30,
                       learn_rate = 0.3,
                       nfolds = 10,
                       seed = 1234)

drf.model <- h2o.randomForest(training_frame = trainH2O,
                              #validation_frame = testH2O,
                              x=2:ncol(trainH2O),
                              y=1,
                              ntrees = 1000,                 # Increase max trees to 500
                              max_depth = 30,               # Increase depth, from 20
                              nbins_cats = 5000,
                              nfolds = 10,
                              seed = 1234)                  #
h2o.saveModel(gbm.model,'./Models/H2O/', force = TRUE)
h2o.saveModel(drf.model,'./Models/H2O/', force = TRUE)

predictionsDRF <- as.data.frame(h2o.predict(drf.model,testH2O))
predictionsGBM <- as.data.frame(h2o.predict(gbm.model,testH2O))

cmDRF <- table(testSparse$sentiment, predictionsDRF$predict)
cmGBM <- table(testSparse$sentiment, predictionsGBM$predict)

metrics(cmDRF)
metrics(cmGBM)

## Get the logloss (lower = better) on the validation set
h2o.logloss(h2o.performance(drf.model, newdata = testH2O))
h2o.logloss(h2o.performance(gbm.model, newdata = testH2O)) #1.35

h2o.performance(drf.model,newdata = testH2O)
h2o.performance(gbm.model,newdata = testH2O)

##############################################################################
# Optimization GBM

## Depth 10 is usually plenty of depth for most datasets, but you never know
hyper_params = list( max_depth = seq(1,29,2) )
#hyper_params = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,

  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),

  ## which algorithm to run
  algorithm="gbm",

  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",

  ## standard model parameters
  x = 2:ncol(trainSparse),
  y = 1,
  training_frame = trainH2O,
  validation_frame = testH2O,
  ## more trees is better if the learning rate is small enough
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,
  ## learning rate annealing: learning_rate shrinks by 1% after every tree
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,
  ## sample 80% of rows per tree
  sample_rate = 0.8,
  ## sample 80% of columns per split
  col_sample_rate = 0.8,
  ## fix a random number generator seed for reproducibility
  seed = 1234,
  ## early stopping once the validation logloss doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "logloss",
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10
)

## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
grid

## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("depth_grid", sort_by="logloss", decreasing = FALSE)
sortedGrid

## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]
minDepth = min(as.numeric(topDepths)) # 15
maxDepth = max(as.numeric(topDepths)) # 27

minDepth
maxDepth
################# 
# Add max + min depth to new hyper parameter

hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),                                      
  # max_depth = seq(15,27,1),                                      
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),                       
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),                    
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),               
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),   
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(trainH2O))-1,1),    
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                        
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                    
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),   
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)

search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3600,      
  ## build no more than 100 models
  max_models = 100,          
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "logloss",
  stopping_tolerance = 1e-3
)

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  ## which algorithm to run
  algorithm = "gbm",
  ## identifier for the grid, to later retrieve it
  grid_id = "final_grid", 
  ## standard model parameters
  x = 2:ncol(trainSparse), 
  y = 1, 
  training_frame = trainH2O, 
  validation_frame = testH2O,
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 10000,                  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,            
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,    
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 3600,       
  ## early stopping once the validation logloss doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "logloss", 
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,                                                
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234                                                             
)

## Sort the grid models by logloss
sortedGrid <- h2o.getGrid("final_grid", sort_by = "logloss", decreasing = FALSE)    
sortedGrid
  
# Apply new optimized model on testH2O
gbm.model.opt <- h2o.getModel(sortedGrid@model_ids[[1]])
print(h2o.logloss(h2o.performance(gbm.model.opt, newdata = testH2O)))

gbm.model.opt@parameters

# Get predictions
predictionsGBM.opt <- as.data.frame(h2o.predict(gbm.model.opt,testH2O))
cmGBM.opt <- table(testSparse$sentiment, predictionsGBM.opt$predict)
metrics(cmGBM.opt)

# Keep best model
h2o.saveModel(gbm.model.opt, path = './Models/H2O/', force = TRUE)

# Save R environment
save.image('~/GitHub/NextBigCrypto-Senti/Models/Senti_zGBM_DRF_Optimized_080518.RData')

# load('~/GitHub/NextBigCrypto-Senti/Models/Senti_GBM_DRF_Optimized_080518.RData')
 