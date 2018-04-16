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
# Load tweet message for date references (edit input data here)

tweet.df <- as.data.frame(read_csv('1. Crawlers/1b. Report/1_$BTC_FULL.csv',
                             locale = locale(encoding = 'latin1')))

token_name <- 'BTC'

start_date <- as.Date(min(tweet.df$created_at))

##########################
# load price dataset

price.df <- read_csv("./1. Crawlers/Crypto-Markets_2018-04-16.csv") %>%
  filter(symbol == token_name & date >= start_date) %>%
  select(date, open, close)

# calculate differences between close prices of each transaction dates
price.df$pricediff <- 0

for (i in 2:nrow(price.df)){
  price.df$pricediff[i] <- price.df$close[i] - price.df$close[i-1]
}

# Calculate absolute value
# price.df$diff.abs <- abs(price.df$pricediff)

############
# Exploratory
dat <- as.data.frame(price.df$pricediff)
dat <- cbind(price.df$date,dat)
colnames(dat) <- c('date', 'pricediff')

summary(dat$diff.abs)

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

for (i in 1:nrow(price.df)){
  if (i==1){next}
  price.df$t_1[i] <- price.df$bin[i-1]
  if (i==2){next}
  price.df$t_2[i] <- price.df$bin[i-2]
  if (i==3){next}
  price.df$t_3[i] <- price.df$bin[i-3]
}

#################################
# Granger causality test
install.packages("lmtest")
library(lmtest)

## Which came first: the chicken or the egg?
# data(ChickEgg)
# grangertest(egg ~ chicken, order = 3, data = ChickEgg)
# grangertest(chicken ~ egg, order = 3, data = ChickEgg)


price.test <- price.df

price.test$t <- NA
price.test$t_1 <- NA
price.test$t_2 <- NA
price.test$t_3 <- NA
price.test$t_4 <- NA
price.test$t_5 <- NA
price.test$t_6 <- NA
price.test$t_7 <- NA
# Assign close prices to t-1/-2/-3/-4/-5/-6/-7 for Granger test
for (i in 1:nrow(price.test)){
  price.test$t[i] <- price.test$close[i]
  if (i==1){next}
  price.test$t_1[i] <- price.test$close[i-1]
  if (i==2){next}
  price.test$t_2[i] <- price.test$close[i-2]
  if (i==3){next}
  price.test$t_3[i] <- price.test$close[i-3]
  if (i==4){next}
  price.test$t_4[i] <- price.test$close[i-4]
  if (i==5){next}
  price.test$t_5[i] <- price.test$close[i-5]
  if (i==6){next}
  price.test$t_6[i] <- price.test$close[i-6]
  if (i==7){next}
  price.test$t_7[i] <- price.test$close[i-7]
}

# price.test <- price.test[,which(colnames(price.test) %in% c('t','t_1','t_2','t_3',
#                                                           't_4','t_5','t_6','t_7'))]
price.test <- price.test[c('t','t_1','t_2','t_3',
                           't_4','t_5','t_6','t_7')]
# Granger test on price movement
grangertest(t ~ t_1, 
            data = price.test)
warnings()
gc()
