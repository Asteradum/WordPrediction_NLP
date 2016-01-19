options(shiny.maxRequestSize=30*1024^2)

library(shiny)
library(tm)
library(RWeka)
library(stringr)

# Function loading
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

# Model Loading

biModel <- readRDS("biModel.Rda")
triModel <- readRDS("triModel.Rda")
quadModel <- readRDS("quadModel.Rda")


# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {    

  output$result <- renderText({
    sentence <- input$obs
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
    
    return(result)
  })
})