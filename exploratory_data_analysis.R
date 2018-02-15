library(stringr)
library(dplyr)
library(tm)
library(ngram)
library(RWeka)

twitter <- readLines("..\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter.txt")
blogs <- readLines("..\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt")
news <- readLines("..\\Coursera-SwiftKey\\final\\en_US\\en_US.news.txt")

files <- c("..\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter.txt",
           "..\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt",
           "..\\Coursera-SwiftKey\\final\\en_US\\en_US.news.txt")

files <- unname(sapply(files, readLines))
files <- unlist(files)

CleanTheDataForNGrams <- function(text) {
        require(stringr)
        text <- tolower(text)
        text <- str_replace_all(text,"#\\S+", " ")              # remove hashtags if any
        text <- str_replace_all(text,"http:[[:alnum:]]*", " ")  # remove URL
        text <- str_replace_all(text,"\\.+", ". ")              # remove multiple dots
        text <- str_replace_all(text,"\\?|\\!", ". ")           # replace question and exclamation marks with dot
        text <- str_replace_all(text,"[^a-zA-Z0-9\\.\\,\\']", " ") # replace everything that is not a letter, number, dot, comma
        text <- str_replace_all(text,"[\\s]+", " ")             # collapse additional spaces
        text <- str_replace_all(text," $|\\.$|\\. $", "")       # fix the end of the sentence
        text <- str_replace_all(text,"\\. ", ".")               # remove spaces after dots
        return(text)
}

WordsDistribution <- function(text) {
        require(stringr)
        text <- tolower(text)
        text <- str_replace_all(text,"#\\S+", " ")              # remove hashtags if any
        text <- str_replace_all(text,"http:[[:alnum:]]*", " ")  # remove URL
        text <- str_replace_all(text,"[^a-zA-Z\\']", " ")       # replace everything that is not a letter
        text <- str_replace_all(text,"[\\s]+", " ")             # collapse additional spaces
        text <- str_replace_all(text," $", "")                  # fix the end of the sentence
        text <- str_split(text, " ")
        for (i in 1:length(text)) {
                if (any(text[[i]] == "")) {
                        myNullChar <- which(text[[i]] == "")
                        text[[i]] <- text[[i]][-myNullChar]
                }
        }
        text <- unlist(text)
        text <- table(text)
        text <- sort(text, decreasing = TRUE)
        return(text)
}

CreateNGrams <- function(text, n) {
        require(dplyr)
        text <- CleanTheData(text)
        if (n <= 0) stop("n should be minimum equal 1")
        splittedText = str_split(text, "\\.")
        tempNGramMatrix <- matrix(ncol = n)
        nGramsMatrix <- matrix(ncol = n)
        for (i in 1:length(splittedText)) {
                for (j in 1:length(splittedText[[i]])) {
                        temp <- str_split(splittedText[[i]][j], " ", simplify = TRUE)
                        if (ncol(temp) >= n) {
                                for (z in n:ncol(temp)) {
                                        for (count in 1:n) {
                                                tempNGramMatrix[1, count] <- temp[1, z - n + count]
                                        }
                                        nGramsMatrix <- rbind(nGramsMatrix, tempNGramMatrix)
                                }
                        }
                }
        }
        nGramsMatrix <- nGramsMatrix[-1, ]
        if (n > 2) {
                ngramCol <- capture.output(cat("paste(",
                                               paste0("nGramsMatrix[, ", 1:(n-2), "], "),
                                               "nGramsMatrix[, ", (n-1), "]", ")",
                                               sep = ""))
                nGramsDataFrame <- transmute(as.data.frame(nGramsMatrix), 
                                             ngram = eval(parse(text = ngramCol)), 
                                             prediction = nGramsMatrix[, n])
        } else {
                nGramsDataFrame <- as.data.frame(nGramsMatrix)
                colnames(nGramsDataFrame) <- c("ngram", "prediction")
        }
        return(nGramsDataFrame)
}

wordsCount <- WordsDistribution(allFiles)
topWords <- wordsCount[1:60]
topWords <- sort(topWords, decreasing = FALSE)
topWords <- as.data.frame(topWords)
g <- ggplot(topWords, aes(x=text, y = Freq)) 
g <- g + geom_bar(stat="identity") + coord_flip() + labs(title = "Most Frequent Words")


###############################################################################
library(stringr)
library(tm)
library(RWeka)
library(ggplot2)
library(wordcloud)
library(quanteda)
# sample to be created randomly

files <- c("..\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter.txt",
           "..\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt",
           "..\\Coursera-SwiftKey\\final\\en_US\\en_US.news.txt")

files <- unname(sapply(files, readLines))
files <- unlist(files)

set.seed(4783)
dataSample <- sample(files, length(files)*0.01)
dataSample <- tolower(dataSample)
dataSample <- str_replace_all(dataSample,"(f|ht)tp(s?)://(.*)[.][a-z]+", " ")
dataSample <- str_replace_all(dataSample,"#\\S+", " ")
dataSample <- str_replace_all(dataSample,"[^a-zA-Z\\']", " ")       # replace everything that is not a letter
dataSample <- str_replace_all(dataSample,"[\\s]+", " ")             # collapse additional spaces
dataSample <- str_replace_all(dataSample," $", "")                  # fix the end of the sentence

corpus <- VCorpus(VectorSource(dataSample))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, PlainTextDocument)

####################RWEKA#################################################
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
# QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))

unigrams <- TermDocumentMatrix(corpus)
bigrams <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
trigrams <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))

unigrams <- removeSparseTerms(unigrams, 0.9999)
unigrams <- sort(rowSums(as.matrix(unigrams)), decreasing = TRUE)
unigrams <- data.frame(n_gram = names(unigrams), freq = unigrams)

top <- unigrams[1:50, ]
g <- ggplot(top, aes(x = reorder(n_gram, freq), y = freq))
g <- g + labs(title = "50 top unigrams", x = "N-Gram", y = "Frequency")
g + geom_bar(stat="identity", fill="#00BA38", colour="black") + coord_flip()


bigrams <- removeSparseTerms(bigrams, 0.9999)
bigrams <- sort(rowSums(as.matrix(bigrams)), decreasing = TRUE)
bigrams <- data.frame(n_gram = names(bigrams), freq = bigrams)
top <- bigrams[1:50, ]
g <- ggplot(top, aes(x = reorder(n_gram, freq), y = freq))
g <- g + labs(title = "50 top unigrams", x = "N-Gram", y = "Frequency")
g + geom_bar(stat="identity", fill="#F8766D", colour="black") + coord_flip()


########################QUANTEDA############################################

set.seed(4783)
dataSample <- sample(files, length(files)*0.01)
dataSample <- tolower(dataSample)
dataSample <- str_replace_all(dataSample,"(f|ht)tp(s?)://(.*)[.][a-z]+", " ")
dataSample <- str_replace_all(dataSample,"#\\S+", " ")
dataSample <- str_replace_all(dataSample,"[^a-zA-Z\\']", " ")       # replace everything that is not a letter
dataSample <- str_replace_all(dataSample,"[\\s]+", " ")             # collapse additional spaces
dataSample <- str_replace_all(dataSample," $", "")                  # fix the end of the sentence

corpus <- quanteda::corpus(dataSample)
bigrams <- dfm(corpus, ngrams = 2, concatenator = " ")
bigrams <- sort(colSums(as.matrix(bigrams)), decreasing = TRUE)
bigrams <- data.frame(n_gram = names(bigrams), freq = bigrams, row.names = FALSE)

##F8766D
#619CFF

wordcloud(words = top[, 1], freq = top[, 2], 
          scale = c(5, 1), max.words = 100, random.order = FALSE, rot.per = 0.35, 
          use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))



##############################################################################

grep("(f|ht)tp(s?)://(.*)[.][a-z]+", allFiles, value = TRUE)

# Links:
# https://rpubs.com/brianzive/textmining
# https://rstudio-pubs-static.s3.amazonaws.com/265713_cbef910aee7642dc8b62996e38d2825d.html
# https://stackoverflow.com/questions/31316274/implementing-n-grams-for-next-word-prediction
# https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
# http://www.mjdenny.com/Text_Processing_In_R.html

CleanTheData <- function(text) {
        require(stringr)
        text <- tolower(text)
        text <- str_replace_all(text,"#\\S+", " ")              # remove hashtags if any
        text <- str_replace_all(text,"http:[[:alnum:]]*", " ")  # remove URL
        text <- str_replace_all(text,"\\.+", ". ")              # remove multiple dots
        text <- str_replace_all(text,"[0-9]", " ")              # remove all numbers
        text <- str_replace_all(text,"\\?|\\!", ". ")           # replace question and exclamation marks with dot
        text <- str_replace_all(text,"[\\s]+", " ")             # collapse additional spaces
        text <- str_replace_all(text," $|\\.$|\\. $", "")       # fix the end of the sentence
        text <- str_replace_all(text,"\\. ", ".")               # remove spaces after dots
        text <- str_replace_all(text,"\\. ", ".")
        return(text)
}