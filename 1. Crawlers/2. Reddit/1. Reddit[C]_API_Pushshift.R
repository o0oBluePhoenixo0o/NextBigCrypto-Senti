# Try APIs call on https://elasticsearch.pushshift.io

#Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti/1. Crawlers")

# Clear environment
rm(list = ls())
gc()
# install packages if not available
packages <- c("jsonlite","httr", # support API call
              "dplyr","readr")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

#---------------------------------------------------------------------

# Develop function to loop crawl comments for reddit submissions from pushshift.io
# base on last epoch date available in the dataset up to current date

crawl_comments <- function(post_id){
  url <- "https://api.pushshift.io"
  path <- "/reddit/submission/comment_ids/" 
  query <- post_id
  
  # extract list of comments id belong to a post
  API.call <- paste0(url,path,query)
  raw_data <- fromJSON(API.call)
  
  if (length(raw_data$data) != 0){
    # extract comment contents 
    list_comments <- NA
    comments_crawl_url <- "https://api.pushshift.io/reddit/comment/search?ids="
    
    # generate list of comments
    # depends on length of comment list ==> can only retrieve 100/time
    # Case 1 - comments list < 100
    if (length(raw_data$data) < 100){
      for (i in 1:length(raw_data$data)){
        if (i==1){
          list_comments <- raw_data$data[i]
          next}
        list_comments <- paste0(list_comments,',',raw_data$data[i])
      }
      
      API.call2 <- paste0(comments_crawl_url,list_comments)
      comments_data <- fromJSON(API.call2)
      
      # some submissions don't have all columns
      main_col <- c('author','created_utc','body','id','parent_id','score')
      
      comments_data <- comments_data$data %>%
        dplyr::select(one_of(main_col))
      # rename columns
      namekey <- c(author = 'com_author', 
                   body = 'com_content',
                   id = 'com_id', 
                   score = 'com_score',
                   parent_id = 'parent_id',
                   created_utc = 'created_utc')
      
      names(comments_data) <- namekey[names(comments_data)]
    }
    
    # Case 2 - comments list > 100
    i <- 1
    j <- 100
    if (length(raw_data$data) >= 100){
      while (i <= j){
        for (k in i:j){
          if (k==i){
            list_comments <- raw_data$data[k]
            next}
          list_comments <- paste0(list_comments,',',raw_data$data[k])
        }
        
        API.call2 <- paste0(comments_crawl_url,list_comments)
        comments_data <- fromJSON(API.call2)
        
        if(i==1){
          # some submissions don't have all columns
          main_col <- c('author','created_utc','body','id','parent_id','score')
          
          comments.final <- comments_data$data %>%
            dplyr::select(one_of(main_col))
          # rename columns
          namekey <- c(author = 'com_author', 
                       body = 'com_content',
                       id = 'com_id', 
                       score = 'com_score',
                       parent_id = 'parent_id',
                       created_utc = 'created_utc')
          
          names(comments.final) <- namekey[names(comments.final)]
          
        }
        if(i!=1){
          # some submissions don't have all columns
          main_col <- c('author','created_utc','body','id','parent_id','score')
          
          new.comments <- comments_data$data %>%
            dplyr::select(one_of(main_col))
          # rename columns
          namekey <- c(author = 'com_author', 
                       body = 'com_content',
                       id = 'com_id', 
                       score = 'com_score',
                       parent_id = 'parent_id',
                       created_utc = 'created_utc')
          
          names(new.comments) <- namekey[names(new.comments)]
          comments.final <- bind_rows(comments.final,new.comments)}
        
        # Initiate next round
        i <- i + 100
        ifelse((j + 100) <= length(raw_data$data),j <- j + 100, j <- length(raw_data$data))
      }
    }
    
    # to make naming consistent
    if (length(raw_data$data) >= 100){comments_data <- comments.final}
    
    # Add post_id for primary key 
    comments_data <- cbind(post_id, comments_data)
    gc()
  }
  
  if(length(raw_data$data) == 0){comments_data <- 0}
  return(comments_data)
}

# -------------------------------------------------------------
# Crawl Comments base on submission ids - 28.05.2018
reddit.crypto <- readr::read_csv("~/GitHub/NextBigCrypto-Senti/1. Crawlers/2b. Reddit Report/Crypto_Reddit.csv") %>%
  dplyr::select(author, 
                score, 
                created_utc, # created time
                id, #submission id
                num_comments, # number of comments
                post_hint, # type of post (video/image/text)
                title, # title of submission
                selftext, # content of submission (text if not video/image)
                link_flair_text, link_flair_css_class, # act as topic classification by reddit
                subreddit, subreddit_id # /r/CryptoCurrency
                )

# Remove submissions with 0 score + 0 comments (low karma)
reddit.crypto <- reddit.crypto  %>%
  filter(num_comments !=0 & score !=0) %>%
  dplyr::rename(post_author = author,
                post_id = id,
                post_score = score)

###########################

comments.df <- read_csv('~/GitHub/NextBigCrypto-Senti/1. Crawlers/2b. Reddit Report/Crypto_Reddit_comments.csv')

comments.old <- comments.df  %>%
  dplyr::select(post_id) %>%
  distinct()
# Filter to get new submission from crawling
new.reddit.crypto <- anti_join(reddit.crypto, comments.old, by = 'post_id') %>%
  filter(num_comments !=0 & post_score !=0)

# Start crawling
tryCatch({
  for (i in 1:nrow(new.reddit.crypto)){
    
    post_id <- new.reddit.crypto$post_id[i]
    
    # Check if submission has been added
    flag <- comments.df %>% filter(post_id == new.reddit.crypto$post_id[i]) %>% tally 
    flag <- flag$n
    if (flag !=0){
      print(paste0('Already crawled comments for post ',post_id,' at position ',i,'/',nrow(new.reddit.crypto)))
      next} # already exist ==> skip
    
    comments_data <- crawl_comments(post_id)
    if (comments_data != 0){
      print(paste0('Complete crawling comments for post ',post_id,' with ',nrow(comments_data),' comments at position ',i,'/',nrow(new.reddit.crypto)))
      # Create comments dataframe
      if (i==1){
        comments.df <- comments_data
        next
      }
      comments.df <- bind_rows(comments.df, comments_data)
      gc()
    }
    if (comments_data == 0){
      print(paste0('Complete crawling comments for post ',post_id,' with 0 comment at position ',i,'/',nrow(new.reddit.crypto)))
    }
    if (i %% 2000 == 0){
      write_csv(comments.df, '~/GitHub/NextBigCrypto-Senti/1. Crawlers/2b. Reddit Report/Crypto_Reddit_comments[BK].csv')
      }
  }
  
}, error=function(e){
  })


# Backup comments data only
# write_csv(comments.df,'~/GitHub/NextBigCrypto-Senti/1. Crawlers/2b. Reddit Report/Crypto_Reddit_comments.csv')

# Merge to get final data
final.df <- right_join(reddit.crypto, comments.df, by = 'post_id')

# save.image(paste0('~/GitHub/NextBigCrypto-Senti/Crypto_Reddit_',Sys.Date(),'.RData'))
#load('~/GitHub/NextBigCrypto-Senti/Crypto_Reddit_2018-05-30.RData')

#######################################################################
#######################################################################
######## DEBUG ZONE

url <- "https://api.pushshift.io"
path <- "/reddit/submission/comment_ids/" 
query <- post_id
#query <- '73imur'

# extract list of comments id belong to a post
API.call <- paste0(url,path,query)
raw_data <- fromJSON(API.call)

API.call
raw_data$data
post_id
# extract comment contents 
list_comments <- NA
comments_crawl_url <- "https://api.pushshift.io/reddit/comment/search?ids="

# generate list of comments
# depends on length of comment list ==> can only retrieve 100/time
# Case 1 - comments list < 100
if (length(raw_data$data) < 100){
  for (i in 1:length(raw_data$data)){
    if (i==1){
      list_comments <- raw_data$data[i]
      next}
    list_comments <- paste0(list_comments,',',raw_data$data[i])
  }
  
  API.call2 <- paste0(comments_crawl_url,list_comments)
  comments_data <- fromJSON(API.call2)
  API.call2
  comments_data$data
  # some submissions don't have all columns
  main_col <- c('author','created_utc','body','id','parent_id','score')
  
  comments_data <- comments_data$data %>%
    dplyr::select(one_of(main_col))
  # rename columns
  namekey <- c(author = 'com_author', 
               body = 'com_content',
               id = 'com_id', 
               score = 'com_score',
               parent_id = 'parent_id',
               created_utc = 'created_utc')
  
  names(comments_data) <- namekey[names(comments_data)]
}


# Case 2 - comments list > 100
i <- 1
j <- 100
if (length(raw_data$data) >= 100){
  while (i <= j){
    for (k in i:j){
      if (k==i){
        list_comments <- raw_data$data[k]
        next}
      list_comments <- paste0(list_comments,',',raw_data$data[k])
    }
    
    API.call2 <- paste0(comments_crawl_url,list_comments)
    comments_data <- fromJSON(API.call2)
    
    if(i==1){
      # some submissions don't have all columns
      main_col <- c('author','created_utc','body','id','parent_id','score')
      
      comments.final <- comments_data$data %>%
        dplyr::select(one_of(main_col))
      # rename columns
      namekey <- c(author = 'com_author', 
                   body = 'com_content',
                   id = 'com_id', 
                   score = 'com_score',
                   parent_id = 'parent_id',
                   created_utc = 'created_utc')
      
      names(comments.final) <- namekey[names(comments.final)]
      
    }
    if(i!=1){
      # some submissions don't have all columns
      main_col <- c('author','created_utc','body','id','parent_id','score')
      
      new.comments <- comments_data$data %>%
        dplyr::select(one_of(main_col))
      # rename columns
      namekey <- c(author = 'com_author', 
                   body = 'com_content',
                   id = 'com_id', 
                   score = 'com_score',
                   parent_id = 'parent_id',
                   created_utc = 'created_utc')
      
      names(new.comments) <- namekey[names(new.comments)]
      comments.final <- bind_rows(comments.final,new.comments)}
    
    # Initiate next round
    i <- i + 100
    ifelse((j + 100) <= length(raw_data$data),j <- j + 100, j <- length(raw_data$data))
  }
}

# to make naming consistent
if (length(raw_data$data) >= 100){comments_data <- comments.final}

# Add post_id for primary key 
comments_data <- cbind(post_id, comments_data)
gc()

i <- 6000
post_id <- reddit.crypto$post_id[i]
flag <- comments.df %>% filter(post_id == reddit.crypto$post_id[i]) %>% tally 
flag <- flag$n