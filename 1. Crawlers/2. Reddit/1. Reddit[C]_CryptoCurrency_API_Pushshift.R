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

post_id <- reddit.crypto$id[1]

crawl_comments <- function(post_id){
  url <- "https://api.pushshift.io"
  path <- "/reddit/submission/comment_ids/" 
  query <- post_id
  
  # extract list of comments id belong to a post
  API.call <- paste0(url,path,query)
  raw_data <- fromJSON(API.call)
  
  # extract comment contents 
  list_comments <- NA
  comments_crawl_url <- "https://api.pushshift.io/reddit/comment/search?ids="

  # generate list of comments
  for (i in 1:length(raw_data$data)){
    if (i==1){
      list_comments <- raw_data$data[i]
      next}
    list_comments <- paste0(list_comments,',',raw_data$data[i])
  }
  
  API.call2 <- paste0(comments_crawl_url,list_comments)
  comments_data <- fromJSON(API.call2)
  
  # filter needed columns
  comments_data <- comments_data$data
  comments_data <- comments_data %>%
    dplyr::select(author, created_utc, 
           body, # content
           id, # comment id 
           parent_id, # comments's parent
           score) %>%
    dplyr::rename(com_author = author,
                  com_content = body,
                  com_id = id,
                  com_score = score)
  
  # Add post_id for primary key 
  comments_data <- cbind(post_id, comments_data)
  gc()
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
# GENESIS CRAWL (1st time)
# Start crawling

for (i in 1:nrow(reddit.crypto)){
  post_id <- reddit.crypto$post_id[i]
  comments_data <- crawl_comments(post_id)
  
  print(paste0('Complete crawling comments for post ',post_id,' with ',nrow(comments_data),' comments at position ',i,'/',nrow(reddit.crypto)))
  # Create comments dataframe
  if (i==1){
    comments.df <- comments_data
    next
  }
  comments.df <- rbind(comments.df, comments_data)
  gc()
}

# Backup comments data only
write_csv(comments.df,'~/GitHub/NextBigCrypto-Senti/1. Crawlers/2b. Reddit Report/Crypto_Reddit_comments.csv')

# Merge to get final data
final.df <- inner_join(reddit.crypto, comments_data, by = 'post_id')

#########################################################################
## DEBUG ZONE

testid <- reddit.crypto$post_id[49]
testdf <- crawl_comments(testid)
url <- "https://api.pushshift.io"
path <- "/reddit/submission/comment_ids/" 
query <- testid

# extract list of comments id belong to a post
API.call <- paste0(url,path,query)
raw_data <- fromJSON(API.call)

# extract comment contents 
list_comments <- NA
comments_crawl_url <- "https://api.pushshift.io/reddit/comment/search?ids="

# generate list of comments

if (length(raw_data$data) < 100){
  for (i in 1:length(raw_data$data)){
    if (i==1){
      list_comments <- raw_data$data[i]
      next}
    list_comments <- paste0(list_comments,',',raw_data$data[i])
  }
  # Split API call if number of comments > 100
  API.call2 <- paste0(comments_crawl_url,list_comments)
  comments_data <- fromJSON(API.call2)
}

if (length(raw_data$data) >= 100){
  # calculating rounds
  rounds <- ceiling(length(raw_data$data)/100)
  for (i in 1:length(raw_data$data) / 100)
}

x <- length(raw_data$data) / 100
ceiling(x)
# filter needed columns
comments_data <- comments_data$data
comments_data <- comments_data %>%
  dplyr::select(author, created_utc, 
                body, # content
                id, # comment id 
                parent_id, # comments's parent
                score) %>%
  dplyr::rename(com_author = author,
                com_content = body,
                com_id = id,
                com_score = score)

# Add post_id for primary key 
comments_data <- cbind(post_id, comments_data)
gc()