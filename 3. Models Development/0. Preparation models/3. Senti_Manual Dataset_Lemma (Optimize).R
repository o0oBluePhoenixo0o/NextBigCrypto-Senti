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
load('./Models/Senti_Manual_UniGram_Lemma_2018-04-26.RData')

# Initialize h2o cluster
h2o.init()

# load h2o model
gbm.model <- h2o.loadModel('./Models/GBM_model_R_1524717959078_3')

predictionsGBM <- as.data.frame(h2o.predict(gbm.model,testH2O))

cmGBM <-   table(testSparse$sentiment, predictionsGBM$predict)
metrics(cmGBM)
h2o.auc(h2o.performance(gbm.model, newdata = testH2O)) 
