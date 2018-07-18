# clear the environment
rm(list= ls())
gc()

#################
#               #
# CONTROL PANEL #
#               #
#################
# Edit variables here for the model you want to generate
# crypto token to be built models on
token_name <- NA
# compare price with USD$ or BTC (Bitcoin) (0 = USD / 1 = BTC)
compare.w.BTC <- NA

# Legend
# - Historical Price (HP)
# - Sentiment Analysis (trained) (SAT)
# - Sentiment Analysis (packages) (SAP)
# - Pre-defined topics (PD) 

# Change the flag here (1 = true / 0 = false)
# Example, model 4 would be SAT = 1 and HP = 1 while the rest are 0
model.list <- data.frame(HP = numeric(), 
                         SAT = numeric(), 
                         SAP = numeric(), 
                         LDA = numeric(), 
                         PD = numeric())

# 1.  HP
model.list[1,] <- c(1,0,0,0,0) 
# 2.  SAT            
model.list[2,] <- c(0,1,0,0,0) 
# 3.  SAP            
model.list[3,] <- c(0,0,1,0,0)
# 4.  SAT - HP
model.list[4,] <- c(1,1,0,0,0)
# 5.  SAP - HP
model.list[5,] <- c(1,0,1,0,0)
# 6.  LDA            
model.list[6,] <- c(0,0,0,1,0)
# 7.  LDA - HP
model.list[7,] <- c(1,0,0,1,0)
# 8.  LDA - SAT
model.list[8,] <- c(0,1,0,1,0)      
# 9.  LDA - SAT - HP
model.list[9,] <- c(1,1,0,1,0)
# 10. LDA - SAP
model.list[10,] <- c(0,0,1,1,0)
# 11. LDA - SAP - HP 
model.list[11,] <- c(1,0,1,1,0)
# 12. PD             
model.list[12,] <- c(0,0,0,0,1)
# 13. PD - HP
model.list[13,] <- c(1,0,0,0,1)
# 14. PD - SAT
model.list[14,] <- c(0,1,0,0,1)
# 15. PD - SAT - HP
model.list[15,] <- c(1,1,0,0,1)
# 16. PD - SAP
model.list[16,] <- c(0,0,1,0,1)
# 17. PD - SAP - HP
model.list[17,] <- c(1,0,1,0,1)

ID <- seq.int(nrow(model.list))
model.list <- cbind(ID,model.list)

# Function to get model name (for later use - combine all results together)
get.model.name <- function(model.list,model.no,compare.w.BTC,position){
  title.final <- ''
  if (model.list$HP == 1){title.final <- paste0(title.final,'_HP')}
  if (model.list$SAT == 1){title.final <- paste0(title.final,'_SAT')}
  if (model.list$SAP == 1){title.final <- paste0(title.final,'_SAP')}
  if (model.list$LDA == 1){title.final <- paste0(title.final,'_LDA')}
  if (model.list$PD == 1){title.final <- paste0(title.final,'_PD')}
  
  # compare price in BTC / USD
  if (compare.w.BTC == 1){compare.w.BTC <- '_wBTC'}
  if (compare.w.BTC == 0){compare.w.BTC <- '_wUSD'}

  result_filename <- paste0(position,'.',token_name,'_',model.no, compare.w.BTC, title.final)
  return(result_filename)
}
# Read in coin list as Oct 17
coins_list <- read.csv("~/GitHub/NextBigCrypto-Senti/1. Crawlers/Top50_Oct7.csv")

####################
# Single
###################
# Add PD for ETH
token_name <- 'BCH'
compare.w.BTC <- 0 #USD
position <- match(token_name, coins_list$symbol) # get position in queue
set <- c(2:3)
for (count in 1:length(set)){
  # get model's name
  model.no <- set[count]
  result_filename <- get.model.name(model.list[set[count],],model.no,compare.w.BTC,position)

  # Call funcs
  source('~/GitHub/NextBigCrypto-Senti/3. Models Development/0. Final_Models_Func.R')
  # clear environment
  rm(list=ls()[! ls() %in% c('token_name','compare.w.BTC','position','count','model.list','get.model.name',
                             'df.clean','df.senti','df.LDA','df.PD',
                             'set')])
  gc() # garbage collector

}
####################
# Multiple
###################
token_name <- 'BCH'
compare.w.BTC <- 1 # compare with BTC
position <- match(token_name, coins_list$symbol) # get position in queue
#16 done
for (count in 11:nrow(model.list)){
  if (count == 16){next}
  # get model's name
  model.no <- count
  result_filename <- get.model.name(model.list[count,],model.no,compare.w.BTC,position)
  
  # Call funcs
  source('~/GitHub/NextBigCrypto-Senti/3. Models Development/0. Final_Models_Func.R')
  # clear environment
  rm(list=ls()[! ls() %in% c('token_name','compare.w.BTC','position','count','model.list','get.model.name',
                             'df.clean','df.senti','df.LDA','df.PD')])
  gc() # garbage collector
}

#######################################################################################
# Get final results (combination of all)
#######################################################################################

token_name <- 'BCH'
compare.w.BTC <- 0

# Read in coin list as Oct 17
coins_list <- read.csv("~/GitHub/NextBigCrypto-Senti/1. Crawlers/Top50_Oct7.csv")
position <- match(token_name, coins_list$symbol) # get position in queue

# compare price in BTC / USD
if (compare.w.BTC == 1){compare.w.BTC <- '_wBTC'}
if (compare.w.BTC == 0){compare.w.BTC <- '_wUSD'}

files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/3. Models Development/Results/',
                    pattern = paste0('^',position,'.',token_name,'_','\\d*',compare.w.BTC,'_'))


# files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/3. Models Development/Results/',
#                     pattern = paste0('^',position,'.',token_name,'_','*[:digit:]',
#                                      '_',compare.w.BTC,'_'))
# substr(files[1],14,nchar(files[1])-5)
# test <- strsplit(files[2],'_')[[1]][2] # extract the position
for (i in 1:length(files)){
  test <- strsplit(files[i],'_')[[1]][2] # extract the position
  if (nchar(test) == 1){name <- substr(files[i],14,nchar(files[i])-5)}
  if (nchar(test) == 2){name <- substr(files[i],15,nchar(files[i])-5)}
  
  results <- readxl::read_xlsx(paste0('~/GitHub/NextBigCrypto-Senti/3. Models Development/Results/',files[i]))
  # Final results file
  if (i == 1){results.final <- cbind(name,results)}
  if (i != 1){
    results <- cbind(name, results)
    results.final <- rbind(results.final, results)}
}
# Save final result
openxlsx::write.xlsx(results.final,paste0('~/GitHub/NextBigCrypto-Senti/3. Models Development/',
                               position,'.',token_name, compare.w.BTC,'_FINAL.xlsx'))
files