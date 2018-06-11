# 12.06.2018
# Calculating inter-rater agreement in manual label dataset

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
              "irr" #inter-rater reliability
              )
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

#####################

# Assemble raters's result
clean.manual <- function(df){
  df <- df[-1,] # remove 1st col
  colnames(df) <- df[1,]
  df <- df[-1,] # remove 1st col (again)
  return(df)
}
manual.true <- readxl::read_xlsx('~/GitHub/NextBigCrypto-Senti/0. Datasets/Manual/Manual_Label_Annotation.xlsx') %>%
  dplyr::select(sentiment) %>%
  rename(senti.true = sentiment)

manual1 <- readxl::read_xlsx('~/GitHub/NextBigCrypto-Senti/0. Datasets/Manual/Tung_Manual.xlsx',
                             col_names = FALSE)
manual2 <- readxl::read_xlsx('~/GitHub/NextBigCrypto-Senti/0. Datasets/Manual/Tien_Manual.xlsx',
                             col_names = FALSE) 
manual3 <- readxl::read_xlsx('~/GitHub/NextBigCrypto-Senti/0. Datasets/Manual/Nha_Manual.xlsx',
                             col_names = FALSE) 
manual4 <- readxl::read_xlsx('~/GitHub/NextBigCrypto-Senti/0. Datasets/Manual/Huy_Manual.xlsx',
                             col_names = FALSE) 
manual.df <- cbind(
  manual.true,
  clean.manual(manual1) %>% select(sentiment) %>% rename(senti1 = sentiment) %>% mutate(senti1 = as.numeric(senti1)),
  clean.manual(manual2) %>% select(sentiment) %>% rename(senti2 = sentiment) %>% mutate(senti2 = as.numeric(senti2)),
  clean.manual(manual3) %>% select(sentiment) %>% rename(senti3 = sentiment) %>% mutate(senti3 = as.numeric(senti3)),
  clean.manual(manual4) %>% select(sentiment) %>% rename(senti4 = sentiment) %>% mutate(senti4 = as.numeric(senti4))
)


kappa2(manual.df[,c(3,4)], "unweighted") # almost good 0.84

# Since we have > 2 raters ==> use Fleiss's Kappa 
kappam.fleiss(manual.df[,c(1,3,4)]) # 0.462 --> keep this one

kappam.fleiss(manual.df) # 0.292
