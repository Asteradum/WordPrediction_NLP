#set Working directory to Current File

# Library Loading
library(tm)
library(RWeka)
library(wordcloud)

# Data Downloading
fileURL <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if (!file.exists("../Source/Coursera-SwiftKey.zip")) 
{
   download.file(fileURL, destfile = "../Source/Coursera-SwiftKey.zip")
   unzip("../Source/Coursera-SwiftKey.zip")
}
rm(fileURL)

#################
# Basic Summary #
#################

# Filesize (Divided to get MB)
blogsFile <- file.info("../Source/final/en_US/en_US.blogs.txt")$size / 1024^2
newsFile <- file.info("../Source/final/en_US/en_US.news.txt")$size / 1024^2
twitterFile <- file.info("../Source/final/en_US/en_US.twitter.txt")$size / 1024^2

# Words (extra line for emoticons)
blogs <- readLines("../Source/final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul=TRUE)
blogs <- iconv(blogs, "latin1", "ASCII", sub="")
news <- readLines("../Source/final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul=TRUE)
news <- iconv(news, "latin1", "ASCII", sub="")
twitter <- readLines("../Source/final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul=TRUE)
twitter <- iconv(twitter, "latin1", "ASCII", sub="")

blogsWords <- sum(sapply(gregexpr("[[:alpha:]]+", blogs), function(x) sum(x > 0)))
newsWords <- sum(sapply(gregexpr("[[:alpha:]]+", news), function(x) sum(x > 0)))
twitterWords <- sum(sapply(gregexpr("[[:alpha:]]+", twitter), function(x) sum(x > 0)))

# Summary
summary <- data.frame(
  file = c("Blogs","News","Twitter"),
  size_MB = c(blogsFile, newsFile, twitterFile),
  words = c(blogsWords, newsWords, twitterWords),
  lines = c(length(blogs), length(news), length(twitter))
)

################################
# Sample creation and cleaning #
################################

#Take a Sample. Consider the distribution of the text random. Picking the first 10K of each
blogsSample <- blogs[1:100000]
newsSample <- news[1:100000]
tweetsSample <- twitter[1:100000]
textSample <- c(blogsSample, newsSample, tweetsSample)

#releasing space
rm(blogs)
rm(blogsFile)
rm(blogsWords)
rm(blogsSample)
rm(twitter)
rm(twitterFile)
rm(twitterWords)
rm(tweetsSample)
rm(news)
rm(newsFile)
rm(newsWords)
rm(newsSample)


# Create a Corpus and start working with Sample
# Clean punctuation marks, extra white spaces, numbers (cannot be used for prediction), profanity words, twitters users and hashtags, urls
# For profanity, twitter users and hastags might be better once the Ngrams are calculated
removeURLs <- function(x) gsub("http[s]?\\:\\/\\/[[:alnum:]]*", "", x)
removeEmails  <- function(x) gsub("[[:alnum:]]*@[[:alnum:]]+\\.[[:alnum:]]+", "", x) 
removeUsersHashtags <- function(x) gsub("[@|\\#][[:alnum:]]+", "", x) 
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
cleanCorpus <- function(corpus) {
  # Load Profanity
  fileURL <- "https://gist.githubusercontent.com/ryanlewis/a37739d710ccdb4b406d/raw/0fbd315eb2900bb736609ea894b9bde8217b991a/google_twunter_lol"
  if (!file.exists("../Source/ProfanityList.txt")) 
    download.file(fileURL, destfile = "../Source/ProfanityList.txt")
  profanity <- readLines("../Source/ProfanityList.txt", encoding = "UTF-8", skipNul=TRUE)
  
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(removeURLs))
  corpus <- tm_map(corpus, content_transformer(removeEmails))
  corpus <- tm_map(corpus, content_transformer(removeUsersHashtags))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, profanity)
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = T)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(trim))
  corpus
}
corpus <- VCorpus(VectorSource(textSample))
corpus <- cleanCorpus(corpus)
##################
# NGram Creation #
##################

#Convert into dataframe for working with RWeka 
cleanSample <-data.frame(text=unlist(sapply(corpus,`[`, "content")), 
                         stringsAsFactors = FALSE)

Tokenizer <- function(textDF, tokens) {
  #Create NGrams
  NGrams <- NGramTokenizer(textDF, 
                                  Weka_control(min = tokens, max = tokens, 
                                               delimiters = " \\\r\n\t,:.?!\""))
  #convert to table to group same hits
  NGramsDistribution <- data.frame(table(NGrams))
  #sort
  NGramsDistribution <- NGramsDistribution[order(NGramsDistribution$Freq, 
                                       decreasing = TRUE),]
  colnames(NGramsDistribution) <- c("NGram","Count")
  NGramsDistribution
}

options( java.parameters = "-Xmx10000m" )
library( "RWeka" )
unigram <- Tokenizer(cleanSample, 1)
bigram <- Tokenizer(cleanSample, 2)
trigram <- Tokenizer(cleanSample, 3)
quadgram <- Tokenizer(cleanSample, 4)

saveRDS(unigram, file = "../Staging/unigram.Rda")
saveRDS(bigram, file = "../Staging/bigram.Rda")
saveRDS(trigram, file = "../Staging/trigram.Rda")
saveRDS(quadgram, file = "../Staging/quadgram.Rda")

unigram <- readRDS("../Staging/unigram.Rda")

#Analysis on data
totalwords <- sum(unigram$Count)
paretto80 <- totalwords*.8
i <-1
cummulative <- 0
while (cummulative < paretto80)
{
  cummulative <- cummulative +  unigram[i,2]
  i<-i+1
}
paretto20 <- i / nrow(unigram)

paretto <- cbind(paretto20,i, nrow(unigram), paretto80, totalwords)
colnames(paretto)<- c("P20", "P20 Value", "Unique Words(P100 Value)",  "P80 Value", "Total Words (P100 Value)" )
paretto

#WordClouds
wordcloud(unigram$NGram,unigram$Count,
          min.freq=5,
          max.words=100,
          random.order=FALSE,
          colors=brewer.pal(12, "Paired"))
wordcloud(bigram$NGram,bigram$Count,
          min.freq=5,
          max.words=100,
          random.order=FALSE,
          colors=brewer.pal(8, "Set2"))
wordcloud(trigram$NGram,trigram$Count,
          scale= c(3,.25),
          min.freq=5,
          max.words=50,
          random.order=FALSE,
          colors=brewer.pal(8, "Dark2"))
wordcloud(quadgram$NGram,quadgram$Count,
          scale= c(2,.2),
          min.freq=5,
          max.words=50,
          random.order=FALSE,
          colors=brewer.pal(8, "Paired"))

#Cleaning
rm (paretto20)
rm(paretto80)
rm(i)
rm(totalwords)
rm(cummulative)
rm(corpus)
rm(textSample)
rm(cleanSample)

