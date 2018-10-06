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
  some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", " ", some_txt,perl=TRUE)
  some_txt = gsub("@\\w+", " ", some_txt)
  some_txt = gsub("[ ]http.+( |$)", " ", some_txt)
  some_txt = gsub("[[:punct:]]", " ", some_txt)
  some_txt = gsub("[[:digit:]]", " ", some_txt)
  #some_txt = gsub("[ t]{2,}", " ", some_txt)
  some_txt = gsub("\\t", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", " ", some_txt)
  some_txt = gsub("amp", "", some_txt)
  some_txt = gsub("^RT[ ]+","", some_txt)
  some_txt = gsub("[^[:alpha:][:space:]]*", "", some_txt) 
  some_txt = gsub("[ ]{2,}"," ", some_txt)
  return(some_txt)
}



#Twitter Data
setup_twitter_oauth(consumer_key = 'mJsnRMKaLZ6nR6Zk55VvUV7jF',consumer_secret = 'nwwaMduWOFoxBT9ztZtMwRoUGDj0oFquFOKkG5UZpz7pXXxIih')
TweetData =  searchTwitteR(searchString = "bitcoin",n = 2000, resultType = 'mixed')
 dim(TweetData$getText())
#geocode = udf_geocode("Nevada","150"),resultType = 'recent',,  since = '2017-08-01', until = '2017-09-16',, lang = 'en'
#Cleaning the data
onlyTweetData = sapply(TweetData, function(t) t$getText())
CleandedTWeetData = sapply(onlyTweetData, function(t) cleanData(t))
CleandedTWeetData = mapply(cleanData, onlyTweetData)
CleandedTWeetData = data.frame(CleandedTWeetData)
CleandedTWeetData = unique(CleandedTWeetData)
CleandedTWeetData1 = CleandedTWeetData 


dim(CleandedTWeetData)

#Creating Corpus
TweetDataCorpus = VCorpus(VectorSource(CleandedTWeetData$CleandedTWeetData))

#Further Data Preparation on Corpus
TweetDataCorpus = tm_map(TweetDataCorpus, content_transformer(stri_trans_tolower))
enStopWords<- c((stopwords('en')),c("rt", "use", "used", "via", "amp", "rahul","gandhi", "http","https"))
removeSingleChar = function(x) {gsub(" . "," ",x)} #
extraWhiteSpace = function(x) gsub('[ ]{2,}',' ', x)#
#twoLetterWord = function(x) gsub('(\\b|^)[a-zA-Z0-9]{2,2}( |\\n|$)+',' ', x, perl = TRUE)
#removeNumbers = function(x) gsub('[[:digit:]]','',x)
removeNumbers = function(x) gsub('[0-9]+','',x) #
TweetDataCorpus = tm_map(TweetDataCorpus,removeWords , enStopWords) 
TweetDataCorpus = tm_map(TweetDataCorpus, content_transformer(removeSingleChar))
TweetDataCorpus = tm_map(TweetDataCorpus, content_transformer(extraWhiteSpace))
TweetDataCorpus = tm_map(TweetDataCorpus, content_transformer(removeNumbers))
TweetDataCorpus = tm_map(TweetDataCorpus, content_transformer(twoLetterWord))

#Backup in case of Recovery and for Stem words treatmant
TweetDataCorpusCopy = TweetDataCorpus

#Stemming
TweetDataCorpus = tm_map(TweetDataCorpus, stemDocument)
TweetDataCorpus[[2]]$content


#####Function to correct/complete the text after stemming

stemCompletion2 <- function(x,dictionary) {
  x <- unlist(strsplit(as.character(x)," "))
  x <- x[x !=""]
  x <- stemCompletion(x, dictionary = dictionary, type = "shortest")
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

#####Stem Complete and Display the same tweet above with the completed and corrected text. 
TweetDataCorpus <- lapply(TweetDataCorpus, stemCompletion2, dictionary=TweetDataCorpusCopy)
TweetDataCorpus <- VCorpus(VectorSource(TweetDataCorpus))
writeLines(strwrap(TweetDataCorpus[[250]]$content, 60))

#mayweather is changed as mayweath, we will have to replace it again
#TweetDataCorpus = tm_map(TweetDataCorpus, content_transformer(gsub), pattern="mayweath",replacement="mayweather")


#Creating a Term Document Matrix
TweetTDM = TermDocumentMatrix(TweetDataCorpus, control= list(wordLengths= c(1, Inf)))
TweetTDM$j


#####Find the terms used most frequently
(freq.terms <- findFreqTerms(TweetTDM, lowfreq = 50))
term.freq <- rowSums(as.matrix(TweetTDM))
term.freq <- subset(term.freq, term.freq > 20)
df <- data.frame(term = names(term.freq), freq= term.freq)


#####plotting the graph of frequent terms
ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="Term Frequency Chart", x="Terms", y="Term Counts")) 




#####calculate the frequency of words and sort it by frequency and setting up the Wordcloud
word.freq <-sort(rowSums(as.matrix(TweetTDM)), decreasing= F)
pal<- brewer.pal(8, "Dark2")
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 2, random.order = F, colors = pal, max.words = 100)

# Identify and plot word correlations.
WordCorr <- apply_as_df(TweetDataCorpus[1:500], word_cor, word = "congress", r=.25) #Change
plot(WordCorr)

qheat(vect2df(WordCorr[[1]], "word", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

# Messages with word
df <- data.frame(text=sapply(TweetDataCorpus, `[[`, "content"), stringsAsFactors=FALSE)
head(unique(df[grep("congress", df$text), ]), n=10)


##### Find association with a specific keyword in the tweets - 
findAssocs(TweetTDM, "bjp", 0.2)
findAssocs(TweetTDM, "congress", 0.2)





##### Topic Modelling to identify latent/hidden topics using LDA technique
Tweetdtm <- as.DocumentTermMatrix(TweetTDM)

rowTotals <- apply(Tweetdtm , 1, sum)
NullDocs <- Tweetdtm[rowTotals==0, ]
Tweetdtm   <- Tweetdtm[rowTotals> 0, ]
NullDocs$dimnames$Docs

if (length(NullDocs$dimnames$Docs) > 0) {
  onlyTweetData <- onlyTweetData[-as.numeric(NullDocs$dimnames$Docs)]
}

lda <- LDA(Tweetdtm, k = 5) # find 5 topic
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics<- topics(lda)
#topics<- data.frame(date=(tweets.df$created), topic = topics)
topics<- data.frame( topic = topics)
qplot (date, ..count.., data=topics, geom ="density", fill= term[topic], position="stack")

