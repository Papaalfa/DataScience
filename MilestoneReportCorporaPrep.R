## This script has been used to prepare data files for future processing for
## purpose of the Capstone Milestone markdown Report creation.

## Four files are created as a result of this script:
## 1. "texts.RData" - raw texts from the corpora files
## 2. "cleanCorpora.RData" - sampled and cleaned corpora
## 3. "bigramCorpora.RData" - bigrams
## 4. "trigramCorpora.RData" - trigrams

## Libraries loading ###########################################################
library(tm)
library(readr)
library(RWeka)
################################################################################

## Downloading and unzipping the data files ####################################
fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(fileUrl, "./Coursera-SwiftKey/Coursera-SwiftKey.zip")
dateDownloaded <- date()
unzip("./Coursera-SwiftKey/Coursera-SwiftKey.zip", exdir = "./Coursera-SwiftKey")
################################################################################

## Texts loading from the files and then saving to an R object file ############
path <- "./Coursera-SwiftKey/final/en_US/"
fileNames <- list.files(path)

texts <- lapply(paste0(path, fileNames), read_lines)
corpNames <- gsub("^.*\\.(.*)\\..*$", "\\1", fileNames)
names(texts) <- corpNames

save(texts, file = "./Coursera-SwiftKey/texts.RData")
################################################################################

## Texts sampling ##############################################################
textsSample <- function(text) {
    text <- sample(text, ceiling(length(text) * .1))
}
textsSampled <- lapply(texts, textsSample)
################################################################################

## Removing wrong encoding in sampled text #####################################
cleanChars <- function(txt) {
    txt <- gsub("<..>", "", iconv(txt, to="ASCII", sub="byte"))
}
textsSampled <- lapply(textsSampled, cleanChars) 
################################################################################

## Making corpora ##############################################################
textsJoined <- lapply(textsSampled, function(x) {paste(x, collapse = " ")})
source <- lapply(textsJoined, VectorSource)
corpora <- lapply(source, Corpus)
################################################################################

## Corpora cleaning and saving to a file #######################################
cleanCorp <- function(corpus) {
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, stopwords("en"))
}
cleanCorpora <- lapply(corpora, cleanCorp)

save(cleanCorpora, file = "./Coursera-SwiftKey/cleanCorpora.RData")
################################################################################

## Creating and saving N-gram objects ##########################################
makeNgrams <- function(corpus, n) {
    ngramTokenizer <- function(x) NGramTokenizer(x
                                                 , Weka_control(min = n
                                                                , max = n))
    corpus <- TermDocumentMatrix(corpus
                                 , control = list(tokenize = ngramTokenizer))
    corpus <- data.frame(word = corpus$dimnames$Terms
                         , frequency = corpus$v
                         , stringsAsFactors = FALSE)
    corpus <- corpus[order(corpus$frequency, decreasing = T), ]    
}
trigramCorpora <- lapply(cleanCorpora, makeNgrams, n=3)
bigramCorpora <- lapply(cleanCorpora, makeNgrams, n=2)

save(trigramCorpora, file = "./Coursera-SwiftKey/trigramCorpora.RData")
save(bigramCorpora, file = "./Coursera-SwiftKey/bigramCorpora.RData")
################################################################################