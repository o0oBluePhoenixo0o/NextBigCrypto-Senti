# Building Sentiment Anallysis - Trained model 23.04.2018

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

#### Load up models from manual sentiment analysis #####

#load ('./Models/Senti_Manual_UniGram_2018-04-19.RData')

# Using best model so far (Distributed Random Forest - DRF)

# initialize h2o cluster
h2o.init()

testH2O <- as.h2o(testSparse[,2:ncol(testSparse)])

# load DRF model
h2o_drf <- h2o.loadModel('DRF_model_R_1524133809810_4')

predictionsDRF <- as.data.frame(h2o.predict(h2o_drf,testH2O))
cmDRF <- table(testSparse$sentiment, predictionsDRF$predict)

metrics(cmDRF) 

confusionMatrix(predictionsDRF$predict, testSparse$sentiment)
