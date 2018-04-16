# Hybrid Pattern Matching (Aspect Based Sentiment)
# 1. Create list of aspects (base on pre-defined topic list: tokens, exchanges, regulations....etc.)
# 2. POS Tag to all processed dataset --> analyze pattern after nouns (match to each aspects)
# 3. Simple sentiment analyzer to check each polarization on each aspects
# 4. Test with manual dataset (currently 1000/5000 labeled)

# Treebank POS
# https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html


# 1. Page 60 (thesis ASB) - 2-stage approace: subjectivity detection -> polarity detection
# Pang and Lee, 2004; Wilson et al., 2005; Barbosa and Feng, 2010; Malakasiotis et al., 2013
# 1st classifier for neutral - the rest
# 2nd classifier for pos/neg


# Credited to https://github.com/svsuriyaa/NLP---Aspect-Based-Sentiment-Analysis-of-Customer-Reviews-using-Hybrid-Pattern-Matching/blob/master/RVesters_Web%20Scraping_Sentiment%20Analysis.R

#clear the environment
rm(list= ls())

# load packages and set options
options(stringsAsFactors = FALSE)

#Set up working directory
setwd("~/GitHub/NextBigCrypto-Senti/")

# install packages if not available
packages <- c("readr", #read data
              "lubridate", #date time conversion
              "dplyr", #date manipulation
              "data.table",
              "tm", #textmining package
              "NLP","openNLP", #NLP core for POS
              "hunspell", #high-performance spellchecker
              "stringi" #string manipulation
)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)

# ZIP PATH for dev tools
if(devtools::find_rtools()) Sys.setenv(R_ZIPCMD= file.path(devtools:::get_rtools_path(),"zip"))
# write and read xlsx files
library(openxlsx)

###################################################

# Read manual label dataset (testing 27.03.18)
manual.df <- read.xlsx('Manual_Dataset_2603_labeling.xlsx')

# Need to join manual df later with main df to fix date_time problem
manual.df <- manual.df[,which(colnames(manual.df) %in% c('status_id','text','processed','sentiment'))]

# tag POS function using core NLP 
tagPOS <-  function(a, ...)
{
  string <- as.String(a)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  ann_1 <- Annotation(1L, "sentence", 1L, nchar(string))
  ann_1 <- annotate(string, word_token_annotator, ann_1)
  ann_2 <- annotate(string, Maxent_POS_Tag_Annotator(), ann_1)
  ann_3 <- ann_2[ann_2$type == "word"]
  POStags <- unlist(lapply(ann_3$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", string[ann_3], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}

# need to rmv punctuation before POS tagging
manual.df$processed2 <- sapply(manual.df$processed, function(x) removePunctuation(x))

### TESTING GROUND

manual.df$text[58]
tagPOS(manual.df$text[58])

manual.df$processed[58]
tagPOS(manual.df$processed[58])

manual.df$processed2[58]
tagPOS(manual.df$processed2[58])


test <- 'This exchange provides good service, however, I dont really like the waiting time when trading XYZ'

test.POS <- tagPOS(test)

test.POS$POStags
test.extractPOS <- extractPOS(test,"NN")
test.extractPOS

### TESTING END

##################      MODULE-3 Extract specific parts of speech for each phrase/sentence      #######################

extractPOS <- function(string, thisPOS)
{
  string <- as.String(string)
  wordAnnotate <- annotate(string, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  POSAnnotate <- annotate(string, Maxent_POS_Tag_Annotator(), wordAnnotate)
  POS_words <- subset(POSAnnotate, type == "word")
  tags <- sapply(POS_words$features, '[[', "POS")
  thisPOSindex <- grep(thisPOS, tags)
  tokenized_Tagged <- sprintf("%s/%s", string[POS_words][thisPOSindex], tags[thisPOSindex])
  untokenized_Tagged <- paste(tokenized_Tagged, collapse = " ")
  untokenized_Tagged
}

##################      MODULE-4 Algorithm to identify phrases from sentences     #######################

pairs <- function(tags)
{
  words = data.frame()
  if(noun_flag==0) #If there is no noun in the sentence, prefix the word "product" to the sentence 
  {
    words = rbind(words,paste("product",s,sep=" ",collapse = NULL),stringsAsFactors=F) 
  }
  else
  {
    for(i in 1:length(tags)) #for each tag,
    { 
      prev.pos = tags[i-1] #identify and assign the previous tag
      this.pos = tags[i] #assign the current tag
      next.pos = tags[i+1] #identify and assign the next tag
      next.next.pos = tags[i+2] #identify and assign the next next tag
      #If the pattern of the phrase is JNN,VRN,NRJ, NRV, NVR, extract the phrase from the sentence
      if(((grepl(J,this.pos)) && (grepl(N,next.pos)) && (grepl(N,next.next.pos))) || ((grepl(V,this.pos)) && (grepl(R,next.pos)) && (grepl(N,next.next.pos))) || ((grepl(N,this.pos)) && (grepl(R,next.pos)) && (grepl(J,next.next.pos))) || ((grepl(N,this.pos)) && (grepl(R,next.pos)) && (grepl(V,next.next.pos))) || ((grepl(N,this.pos)) && (grepl(V,next.pos)) && (grepl(R,next.next.pos))))
      {
        words = rbind(words,paste(s1[i],s1[i+1],s1[i+2],sep =" ", collapse = NULL),stringsAsFactors=F) #s1 contains the unlisted sentence. Based on i value, retreive the actual word
      }
      #If the pattern of the phrase is RJN or RVN extract the phrase from the sentence
      else if (((grepl(R,this.pos)) && (grepl(J,next.pos)) && (grepl(N,next.next.pos))) || ((grepl(R,this.pos)) && (grepl(V,next.pos)) && (grepl(N,next.next.pos))))
      {
        words = rbind(words,paste(s1[i],s1[i+1],s1[i+2],sep =" ", collapse = NULL),stringsAsFactors=F) #s1 contains the unlisted sentence. Based on i value, retreive the actual word
      }
      #If the pattern of the phrase is JN or VN extract the phrase from the sentence
      else if(((grepl(J,this.pos)) && (grepl(N,next.pos))) || ((grepl(V,this.pos)) && (grepl(N,next.pos))))
      {
        {
          #Since JN and VN are subsets of RJN and RVN, phrase is ignored if the previous tag is R in order to avoid redundancy
          if(length(prev.pos)==0)
          {
            words = rbind(words,paste(s1[i],s1[i+1],sep =" ", collapse = NULL),stringsAsFactors=F) #s1 contains the unlisted sentence. Based on i value, retreive the actual word
          }
          else if(grepl(R,prev.pos))
          {
            next
          }
          else
          {
            words = rbind(words,paste(s1[i],s1[i+1],sep =" ", collapse = NULL),stringsAsFactors=F) #s1 contains the unlisted sentence. Based on i value, retreive the actual word
          }
        }
      }
      #If the pattern of the phrase is RJ, RV, or VR extract the phrase from the sentence, look for the nearest N (noun/aspect) to associate with
      else if(((grepl(R,this.pos)) && (grepl(J,next.pos))) || ((grepl(R,this.pos)) && (grepl(V,next.pos))) || ((grepl(V,this.pos)) && (grepl(R,next.pos))))
      {
        if(((length(prev.pos)!=0) && (grepl(N,prev.pos) == F)) && ((length(next.next.pos)!=0) && (grepl(N,next.next.pos) == F)))
        {
          #Three conditions based on the position of the tag. If the current tag is in-between a sentence,
          if(between(i,4,length(tags)-4))
          {
            str = tags[(i-3):(i+4)]
            phrase = s1[(i-3):(i+4)]
            left=i-max(grep(N,tags[1:(i-1)],perl = T)) #search for a noun tag to the left of the current tag and calculate the distance between the current tag and the left noun tag.
            right=min(grep(N,tags[(i+2):length(tags)],perl = T)) #search for a noun tag to the right of the current tag and calculate the distance between the current tag and the right noun tag.
            noun_pos =ifelse(left<=right,i-left,i+1+right) #Based on the nearest noun position, find the actual position in the actual sentence
            nrst_noun = phrase[noun_pos] #find the actual word
            words = rbind(words,paste(nrst_noun,s1[i],s1[i+1],sep =" ", collapse = NULL),stringsAsFactors=F)
          }
          #If the current tag is in the first three words of sentence,
          else if(between(i,1,3))
          {
            str = tags[1:(i+4)]
            phrase = s1[1:(i+4)]
            left=i-max(grep(N,tags[1:(i-1)],perl = T))
            right=min(grep(N,tags[(i+2):length(tags)],perl = T))
            noun_pos =ifelse(left<=right,i-left,i+1+right)
            nrst_noun = phrase[noun_pos]
            words = rbind(words,paste(nrst_noun,s1[i],s1[i+1],sep =" ", collapse = NULL),stringsAsFactors=F)
          }
          #If the current tag is in the last three words of sentence,
          else if(between(i,length(tags)-3,length(tags)))
          {
            str = tags[(i-3):length(tags)]
            phrase = s1[(i-3):length(tags)]
            left=i-max(grep(N,tags[1:(i-1)],perl = T))
            right=min(grep(N,tags[(i+2):length(tags)],perl = T))
            noun_pos =ifelse(left<=right,i-left,i+1+right)
            nrst_noun = phrase[noun_pos]
            words = rbind(words,paste(nrst_noun,s1[i],s1[i+1],sep =" ", collapse = NULL),stringsAsFactors=F)
          }
        }
      }
    }
  }
  if(nrow(words)==0)
  {
    return(NULL)
  }
  else
  {
    colnames(words) <- "Phrases"
    return(words)
  }
}

##################      Main Program     #######################


# identifying patterns in the tagged sentence

J <- c("JJ")   #Adjectives are tagged as JJ by tagPOS(). Hence, storing JJ as string for regex pattern matching in pairs()
N <- c("^N[A-Z]*")   #Nouns are tagged as N followed by alphabets by tagPOS(). Hence, storing ^N[A-Z] as string for regex pattern matching in pairs()
R <- c("^R[A-Z]*")   #Adverbs are tagged as R followed by alphabets by tagPOS(). Hence, storing ^R[A-Z] as string for regex pattern matching in pairs()
V <- c("^V[A-Z]*")   #Verbs are tagged as V followed by alphabets by tagPOS(). Hence, storing ^V[A-Z] as string for regex pattern matching in pairs()


phrases=data.frame()
for (k in 1:nrow(sentences_cleaned)) #for each sentence in the dataframe,
{
  s= as.character(sentences_cleaned[k,1]) #stores the sentence in the current iteration
  result <- lapply(s,tagPOS) #Tag each word in the sentence with appropriate parts of speech using tagPOS()
  result <- as.data.frame(do.call(rbind,result))
  tags = result["POStags"] #Extract tags alone and not the word
  tags = Corpus(VectorSource(result$POStags))
  rm_comma = tm_map(tags,removePunctuation) #Tags come with backslashes and commas. Removing the same.
  text <- data.frame(text=unlist(sapply(rm_comma, `[`, "content")), stringsAsFactors=F)
  tags <- unlist(lapply(text, function(x) { str_split(x, " ") }))
  s1 = unlist(strsplit(s," ",perl = F,useBytes = F)) #stores the POS of the sentence as vector
  noun_flag=ifelse(sum(grepl(N,tags))>0,1,0) #if there is a noun in the sentence, noun_flag is set to 1
  phrases=rbind(phrases,pairs(tags)) #call the pairs() to extract meaningful phrases from the senences
  print(k) #print the iteration variable to know the current loop in execution. Easier for debugging
}
View(phrases)

##################      MODULE-5 Identify the polarities for each phrase identified by pairs() function     #######################

#Customized a list of aspects that pertain to product and service categories

product_nouns=c("price","screen",	"android",	"featuress",	"money",	"apps",		"battery",	"carrier",		"quality",	"camera",	"brand",	"life",	"cell",	"size",	"speaker",	"memory",		"upgrade",			"model",		"storage",	"fingerprint",	"touch",	"button",	"charger",	"device",	"speed",	"version",	"sound",	"space",	"port",	"provider",	"unlock",	"charge",		"cover",		"performance",	"security",	"technology",	"voice",	"itunes",	"software",	"video", "music", "bluetooth", "pixel",	"volume",		"settings",	"videos",	"budget",	"cord",	"games",	"keyboard",	"protector",		"capabilities", "browser", "display",	"feature",	"resolution",	"siri",	"scanner",	"standby",	"thumbprint",			"adapter",	"audio",	"backup",	"durability",	"functionality",	"feature",	"headphone",	"interface",	"ios",	"jack",	"power",	"processor",	"specs",	"usb",	"weight",	"width",	"windows",	"wireless",	"antivirus","sim","picture",	"product",	"iphone",	"phone",	"ipad",	"smartphone",	"cellphone",	"gadget",	"iphone5",	"mac",	"macbook")
service_nouns=c("exchange","contract","coverage","signal","store","activation","network",	"discount", "deal",	"order",	"shipping",	"account",	"sale",	"delivery",	"package",	"refund",	"walmart",	"walmart.com",	"company",	"att",	"gsm","cdma",	"tmobile",	"return",	"packaging","cancellation","refund", "delivery",	"replacement",	"pickup",	"sales",	"services",	"warranty",	"defect",	"damage",	"insurance",	"offer",	"payment",	"rollback",	"shipment",	"ship",	"tracking","verizon","voucher","discount","coupons", "service")

#Creating a dataframe that will hold the columns: aspects, phrases, polarity, productAspect (1 or 0), serviceAspect (1 or 0)

phrases_polarities=as.data.frame(matrix(nrow=nrow(phrases),ncol=5),stringsAsFactors = F)
for (m in 1:nrow(phrases))
{
  print(m)
  polarities=data.frame(polarity(phrases[m,1])) #Identifying polarities of each phrase using polarity() function in qdap package. polarity() retuns a dataframe with multiple columns
  nouns<-lapply(phrases[m,1], extractPOS, "NN") #Extracting the noun part of the phrase
  wordstoremove <- c("/NN","/NNP","/NNPS")
  nouns_cleaned <- tolower(as.String(sapply(nouns, function(x) gsub(paste(wordstoremove, collapse = '|'), '', x))))
  phrases_polarities[m,1]=nouns_cleaned #Aspect in the phrase
  phrases_polarities[m,2]=polarities$all.polarity #store the numeric polarity value
  if(!is.na(wc(phrases_polarities[m,1])) && wc(phrases_polarities[m,1])>1) #when there are multiple words in the phrase,
  {
    sep_nouns=unlist(strsplit(phrases_polarities[m,1], " ")) #words are unlisted
    for (z in 1:length(sep_nouns)) #for each word,
    {
      if(!is.na(match(sep_nouns[z],product_nouns))) #word is checked for existence with product aspects list created
      {
        phrases_polarities[m,3]=1 
        phrases_polarities[m,4]=0
        break
      }
      else if(!is.na(match(sep_nouns[z],service_nouns))) #word is checked for existence with service aspects list created
      {
        phrases_polarities[m,3]=0
        phrases_polarities[m,4]=1
        break
      }
      else #if the word is not available in either of the lists, put zeroes in both columns
      {
        phrases_polarities[m,3]=0
        phrases_polarities[m,4]=0
      }
    }
  }
  else #if there is a single word in the phrase,
  {
    phrases_polarities[m,3]=ifelse(!is.na(match(phrases_polarities[m,1],product_nouns)),1,0)
    phrases_polarities[m,4]=ifelse(!is.na(match(phrases_polarities[m,1],service_nouns)),1,0)
  }
  phrases_polarities[m,5]=phrases[m,1]
}
colnames(phrases_polarities)=c("Aspects","Polarity","ProductAspect","ServiceAspect","Phrases")
View(phrases_polarities)

#Removing phrases that did not have product/service aspects
final_polarities=phrases_polarities[phrases_polarities$ProductAspect==1 | phrases_polarities$ServiceAspect==1,]
#Defining polarity thresholds for +ve, -ve and neutral classification
final_polarities=mutate(final_polarities,Sentiment=ifelse(final_polarities$Polarity==0,"Neutral",ifelse(final_polarities$Polarity>0,"Positive","Negative")))
View(final_polarities)
#Identifying frequency of aspects grouped by sentiment 
freq_table=sqldf("select final_polarities.Aspects, final_polarities.Sentiment, count(*) as Frequency,max(final_polarities.ProductAspect) as ProductAspect,max(final_polarities.ServiceAspect) as ServiceAspect from final_polarities group by final_polarities.Aspects, final_polarities.Sentiment order by Frequency desc")
View(freq_table)