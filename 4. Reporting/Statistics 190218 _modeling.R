########################################################
# Load data from topic modeling
setwd("~/GitHub/NextBigCrypto-Senti/")

# clear the environment
rm(list= ls())

load('LDATUning_ETH.Rdata')


eth_topic <- read.csv('topic_list_LDA_ETH_120218.csv')
