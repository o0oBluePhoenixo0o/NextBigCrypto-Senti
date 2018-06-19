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
token_name <- 'ETH'
# compare price with USD$ or BTC (Bitcoin) (0 = USD / 1 = BTC)
compare.w.BTC <- 1

# Legend
# - Historical Price (HP)
# - Sentiment Analysis (trained) (SAT)
# - Sentiment Analysis (packages) (SAP)
# - Pre-defined topics (PD) 

# Change the flag here (1 = true / 0 = false)
# 1.  HP             
# 2.  SAT            
# 3.  SAP            
# 4.  SAT - HP       
# 5.  SAP - HP       
# 6.  LDA            
# 7.  LDA - HP       
# 8.  LDA - SAT      
# 9.  LDA - SAT - HP 
# 10. LDA - SAP      
# 11. LDA - SAP - HP 
# 12. PD             
# 13. PD - HP        
# 14. PD - SAT       
# 15. PD - SAT - HP  
# 16. PD - SAP       
# 17. PD - SAP - HP  

# Example, model 4 would be SAT = 1 and HP = 1 while the rest are 0
HP <- 0
SAT <- 0
SAP <- 0
LDA <- 0
PD <- 0


# Function to get model name (for later use - combine all results together)
get.model.name <- function(HP,SAT,SAP,LDA,PD,compare.w.BTC){
  title.final <- ''
  if (HP == 1){title.final <- paste0(title.final,'_HP')}
  if (SAT == 1){title.final <- paste0(title.final,'_SAT')}
  if (SAP == 1){title.final <- paste0(title.final,'_SAP')}
  if (LDA == 1){title.final <- paste0(title.final,'_LDA')}
  if (PD == 1){title.final <- paste0(title.final,'_PD')}
  
  # compare price in BTC / USD
  if (compare.w.BTC == 1){compare.w.BTC <- '_wBTC'}
  if (compare.w.BTC == 0){compare.w.BTC <- '_wUSD'}
  
  # Read in coin list as Oct 17
  coins_list <- read.csv("~/GitHub/NextBigCrypto-Senti/1. Crawlers/Top50_Oct7.csv")
  position <- match(token_name, coins_list$symbol) # get position in queue
  
  result_filename <- paste0(position,'.',token_name, compare.w.BTC, title.final)
  return(result_filename)
}
####################

# ETH
# HP - SAT - LDA
# conditional check --> restart all flags
if (HP == 1 | SAT == 1 | SAP == 1 | LDA == 1 | PD == 1){HP <- 0; SAT <- 0; SAP <- 0; LDA <- 0; PD <- 0}

HP <- 1
SAT <- 1
LDA <- 1
result_filename <- get.model.name(HP,SAT,SAP,LDA,PD,compare.w.BTC)

source('~/GitHub/NextBigCrypto-Senti/3. Models Development/0. Final_Models_Func.R')

# clear environment
rm(list=ls()[! ls() %in% c('token_name','compare.w.BTC',
                           'HP','SAT','SAP','LDA','PD','get.model.name')])

# HP - SAP - LDA
# conditional check --> restart all flags
if (HP == 1 | SAT == 1 | SAP == 1 | LDA == 1 | PD == 1){HP <- 0; SAT <- 0; SAP <- 0; LDA <- 0; PD <- 0}

HP <- 1
SAP <- 1
LDA <- 1
result_filename <- get.model.name(HP,SAT,SAP,LDA,PD,compare.w.BTC)

source('~/GitHub/NextBigCrypto-Senti/3. Models Development/0. Final_Models_Func.R')

# clear environment
rm(list=ls()[! ls() %in% c('token_name','compare.w.BTC',
                           'HP','SAT','SAP','LDA','PD','get.model.name')])
#######################################################################################
# Get final results (combination of all)
#######################################################################################

# Read in coin list as Oct 17
coins_list <- read.csv("~/GitHub/NextBigCrypto-Senti/1. Crawlers/Top50_Oct7.csv")
position <- match(token_name, coins_list$symbol) # get position in queue

# compare price in BTC / USD
if (compare.w.BTC == 1){compare.w.BTC <- '_wBTC'}
if (compare.w.BTC == 0){compare.w.BTC <- '_wUSD'}
files <- list.files(path = '~/GitHub/NextBigCrypto-Senti/3. Models Development/Results/',
                    pattern = paste0('^',position,'.',token_name,compare.w.BTC,'_'))

for (i in 1:length(files)){
  name <- substr(files[i],12,nchar(files[i])-5)
  
  results <- readxl::read_xlsx(paste0('~/GitHub/NextBigCrypto-Senti/3. Models Development/Results/',files[i]))
  # Final results file
  if (i == 1){results.final <- cbind(name,results)}
  if (i != 1){
    results <- cbind(name, results)
    results.final <- rbind(results.final, results)}
}
# Save final result
write.xlsx(results.final,paste0('~/GitHub/NextBigCrypto-Senti/3. Models Development/Results/',
                               position,'.',token_name, compare.w.BTC,'_FINAL.xlsx'))
