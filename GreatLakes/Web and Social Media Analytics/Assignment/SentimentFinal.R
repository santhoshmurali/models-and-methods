# Twitter Sentiment Analysis
#Assignment: Social media analytics - Text mining
#Objective: Brand perception analysis
#Instructions for each group:
#  - Identify a brand - any global or Indian product, celebrity, company etc
#- For the identified brand download a minimum 1000 twitter messages for the most recent period
#- Conduct text mining on the data - Correlation, Frequency, Topic Modelling, Sentiment Analysis
#- Submit analysis report and code as part of the assignment - R Markdown document only if using R
#- Can leverage any tool for data visualization, text mining, ML etc
#- Can expand the source of data to face book or any other source as well, but twitter is a must
#- No two groups should have the same brand

#List of required libraries
#need twitter data
#install.packages("twitteR")
library(twitteR)

# for finding Geo location for the given city : It also got several other tools also : 
# Limitations : 2500 request per day
# geocodeQueryCheck to check the available limits
# install.packages('ggmap')
library(ggmap)

#Library Needed for Text Mining
library(tm)

#for string operations
library(stringi)

#for finding word Freequency
library(SnowballC)

library(RColorBrewer)
library(wordcloud)
library(qdap)
library(ggplot2)
library(topicmodels)
library(data.table)
library(sentiment)
library(dplyr)
library(streamgraph)

#Location Function
udf_geocode = function(place,radius){
  if(geocodeQueryCheck(userType = 'free') != '1000 geocoding queries remaining.') {
    temp = geocode(place)
    latLong = paste(temp$lat,temp$lon, paste(radius,'mi',sep=""), sep=",")
  } else {
    print('Warning : Google free services limits in nearing expiration')
  }
  return(latLong)
}

#Data Cleaning
cleanData = function(AnyText){
  some_txt = AnyText
  some_txt = genX(some_txt, " <", ">")
  some_txt = gsub(" {0,}http([[:punct:]]|[[:digit:]]|[a-z|A-Z])+", " ", some_txt)
  #Removing Single Characters
  some_txt = gsub("[[:punct:]]", " ", some_txt)
  some_txt = gsub("[[:digit:]]", " ", some_txt)
  some_txt = gsub("( ){1,}. "," ", some_txt)
  some_txt = gsub(" .( ){1,}"," ", some_txt)
  some_txt = gsub("^. {1,}"," ", some_txt)
  some_txt = gsub("{1,}.$"," ", some_txt)
  
  #Removing Numbers
  some_txt = gsub("[0-9]+","", some_txt)
  
  some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", " ", some_txt,perl=TRUE)
  some_txt = gsub("@\\w+", " ", some_txt)
  #some_txt = gsub("[ t]{2,}", " ", some_txt)
  #some_txt = gsub("\\t", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", " ", some_txt)
  some_txt = gsub("amp", "", some_txt)
  some_txt = gsub("^RT[ ]+","", some_txt)
  some_txt = gsub("[^[:alpha:][:space:]]*", "", some_txt) 
  some_txt = gsub("[ ]{2,}"," ", some_txt)
  some_txt = gsub("(\b|^)[a-zA-Z0-9]{2,2}( |\n|$)+","", some_txt)
  #some_txt = gsub("(^ )|( $)","", some_txt)
  return(some_txt)
}

#Function to correct/complete the text after stemming
stemCompletion2 <- function(x,dictionary) {
  x <- unlist(strsplit(as.character(x)," "))
  x <- x[x !=""]
  x <- stemCompletion(x, dictionary = dictionary, type="shortest")
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

getwd()

#Twitter Data
setup_twitter_oauth(consumer_key = 'mJsnRMKaLZ6nR6Zk55VvUV7jF',consumer_secret = 'nwwaMduWOFoxBT9ztZtMwRoUGDj0oFquFOKkG5UZpz7pXXxIih')
TweetData =  searchTwitteR(searchString = "bitcoin",n = 1000, resultType = 'mixed', lang = "en")
TweetDF = twListToDF(TweetData)
library(xlsx)
write.csv(TweetDF,file = "bitcoin.csv")

TweetDF$created = as.Date(TweetDF$created, format="%d-%m-%y")

#Cleaning the data
CleandedTWeetData = sapply(TweetDF$text, function(t) cleanData(t))
CleandedTWeetData[150]
RepeatTweets = data.frame(table(CleandedTWeetData))
RepeatTweets = RepeatTweets[order(RepeatTweets$Freq, decreasing = FALSE),]
nrow(RepeatTweets)
nrow(unique(RepeatTweets))

#Corpus
TweetDataCorpus = VCorpus(VectorSource(CleandedTWeetData))

#Converitng all to lower case
TweetDataCorpus = tm_map(TweetDataCorpus, content_transformer(stri_trans_tolower))
# Removing Stop Words
enStopWords<- c((stopwords('en')),c("rt", "use", "used", "via", "amp", "http","https"))
TweetDataCorpus = tm_map(TweetDataCorpus,removeWords , enStopWords) 

#Create Dictionary
TweetDataCorpusDict = TweetDataCorpus

#Lets Stem the Words
TweetDataCorpus<-tm_map(TweetDataCorpus, stemDocument)
writeLines(strwrap(TweetDataCorpus[[150]]$content,60))


#####Stem Complete and Display the same tweet above with the completed and corrected text. 
TweetDataCorpus <- lapply(TweetDataCorpus, stemCompletion2, dictionary=TweetDataCorpusDict)
TweetDataCorpus <- VCorpus(VectorSource(TweetDataCorpus))
writeLines(strwrap(TweetDataCorpus[[150]]$content, 60))






