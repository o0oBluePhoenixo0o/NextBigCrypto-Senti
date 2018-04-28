# Filter out Twitter bots (06.04.2018)

# Check Twitter bots (06.04.18)
# 
# if (!requireNamespace("devtools", quietly = TRUE)) {
#   install.packages("devtools")
# }
# devtools::install_github("mkearney/botrnot")

options("scipen"=100, "digits"=4)

# Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers")

# Clear environment
rm(list = ls())

# install packages if not available
packages <- c('rtweet','twitteR', #Twitter API crawlers
              'data.table','dplyr','scales','ggplot2',
              'httr','stringr','rvest','curl','lubridate','coinmarketcapr',
              'gtools','readr','botrnot')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

# if use openxlsx
# ZIP PATH for dev tools
if(devtools::find_rtools()) Sys.setenv(R_ZIPCMD= file.path(devtools:::get_rtools_path(),"zip"))
library(openxlsx)

####################################################################
# Read top 50 data (Oct 7 2017)
coins_list <- read.csv("Top50_Oct7.csv")

# Consolidate all available datasets into 1 big dataset
setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers/1b. Report/")

# Create final user list
final.user_list <- data.frame(screen_name = character())

# Extract users list from 50 datasets
for (i in 1:nrow(coins_list)){
  a <- as.data.frame(read_csv(dir(pattern=paste0('^',i,'_'))[1],
                              locale = locale(encoding = 'latin1')))
  
  # Extract list of screen names
  user_list <- unique(as.data.frame(as.character(a$screen_name)))
  colnames(user_list) <- 'screen_name'
  
  # Merge to final set
  final.user_list <- bind_rows(final.user_list,user_list)
  print(paste0('Finish adding user list from token ',coins_list$symbol[i]))
}

final.user_list <- unique(final.user_list)
final.user_list$botprob <- NA

# Set working directory back to Models folder
setwd("~/GitHub/NextBigCrypto-Senti/Models")

# Determine whether a Twitter ID is a bot or not (probability - gradient boosting machine)
j <- 0 #counter

for (i in 113055:nrow(final.user_list)){
  if (is.na(final.user_list$botprob[i]) == FALSE) { 
    print(paste0('Already analyzed user ',final.user_list$screen_name[i],' at position: ',i))
    next}
  
  if (j %% 180 ==0 & j != 0){
    print('Pause 15 mins due to REST API limit. Reseting queue')
    Sys.sleep(900) #sleep 15 min due to REST API
    # Save backup
    write.xlsx(final.user_list,paste0('Twitter_Bot_Users_(Working).xlsx'))
    j <- 0 #reset counter
  } 
  
  tryCatch({
    final.user_list$botprob[i] <- botrnot(final.user_list$screen_name[i])$prob_bot
  }, error=function(e){cat("User position ",i," has ERROR :",conditionMessage(e), "\n")})
  
  print(paste0('Scanning user ',i,' at position ',j,' in queue'))
  j <- j + 1 #increase counter
  
}

# Save final user list
write.xlsx(final.user_list,paste0('Twitter_Bot_Users_(Final).xlsx'))
save.image(paste0('Twitter_Bot_Final_',Sys.Date(),'.RData'))

# setwd("~/GitHub/NextBigCrypto-Senti")
# final.user_list <- read.xlsx('./Models/Twitter_Bot_Users_(Working).xlsx')
# final.user_list$botprob <- as.numeric(final.user_list$botprob)
