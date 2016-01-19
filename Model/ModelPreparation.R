
# Sentence Parsing
library(tm)
library(RWeka)
library(stringr)


#Function loading

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

#NGRAM Loading
bigram <- readRDS("../Staging/bigram.Rda")
trigram <- readRDS("../Staging/trigram.Rda")
quadgram <- readRDS("../Staging/quadgram.Rda")

write.table(bigram, file = "bigram.csv",row.names=FALSE, sep="|")
write.table(trigram, file = "trigram.csv",row.names=FALSE, sep="|")
write.table(quadgram, file = "quadgram.csv",row.names=FALSE, sep="|")

#reduce the size of the NGRAMS
bigramRed <- bigram[bigram$Count>2,]
trigramRed <- trigram[trigram$Count>3,]
quadgramRed <- quadgram[quadgram$Count>4,]

saveRDS(bigramRed, file = "../Staging/ReducedBigram.Rda")
saveRDS(trigramRed, file = "../Staging/ReducedTrigram.Rda")
saveRDS(quadgramRed, file = "../Staging/ReducedQuadgram.Rda")

rm(bigram)
rm(trigram)
rm(quadgram)

bigramRed <- readRDS("../Staging/ReducedBigram.Rda")
trigramRed <- readRDS("../Staging/ReducedTrigram.Rda")
quadgramRed <- readRDS("../Staging/ReducedQuadgram.Rda")


# Prepare NGRAMs for the model
biModel <- t(data.frame(sapply(as.character(bigramRed$NGram), strsplit, split = " ")))
biModel <- cbind(bigramRed,biModel)
colnames(biModel) <- c("NGram", "Count", "Word1", "Word2")

triModel <- t(data.frame(sapply(as.character(trigramRed$NGram), strsplit, split = " ")))
triModel <- cbind(trigramRed,triModel)
colnames(triModel) <- c("NGram", "Count", "Word1", "Word2", "Word3")

quadModel <- t(data.frame(sapply(as.character(quadgramRed$NGram), strsplit, split = " ")))
quadModel <- cbind(quadgramRed,quadModel)
colnames(quadModel) <- c("NGram", "Count", "Word1", "Word2", "Word3", "Word4")


saveRDS(biModel, file = "../ShinyApp/biModel.Rda")
saveRDS(triModel, file = "../ShinyApp/triModel.Rda")
saveRDS(quadModel, file = "../ShinyApp/quadModel.Rda")

# Model Definition
wordPredictor <- function(sentence){
  
  corpus <- VCorpus(VectorSource(sentence))
  corpus <- cleanCorpus(corpus)
  cleanSentence <-data.frame(text=unlist(sapply(corpus,`[`, "content")), 
                             stringsAsFactors = FALSE)
  wordsCount <- sum(sapply(gregexpr("[[:alpha:]]+", cleanSentence), function(x) sum(x > 0)))

  #parsing words from Ngram and sentence
  if (wordsCount >=3)
  {
    input <- cbind(word(cleanSentence,-3),word(cleanSentence,-2), word(cleanSentence,-1))
    result <- as.character(quadModel[quadModel$Word1 == input[1] 
                           & quadModel$Word2 == input[2] 
                           & quadModel$Word3 == input[3],6][1])
  }
  
  if (wordsCount ==2 || is.na(result))
  {
    input <- cbind(word(cleanSentence,-2), word(cleanSentence,-1))
    result <- as.character(triModel[triModel$Word1 == input[1] 
                           & triModel$Word2 == input[2],5][1])
  }
  
  
  if (wordsCount ==1 || is.na(result))
  {
    input <- word(cleanSentence,-1)
    result <- as.character(biModel[biModel$Word1 == input,4][1])
  }
  
  result
}

#test
sentence <- "Give me one of the fucking #test"
wordPredictor(sentence)

rm(bigramRed)
rm(trigramRed)
rm(quadgramRed)

