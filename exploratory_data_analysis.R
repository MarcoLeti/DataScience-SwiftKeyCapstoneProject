library(stringr)
library(dplyr)
library(tm)
library(ngram)

twitter <- readLines("..\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter.txt")
blogs <- readLines("..\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt")
news <- readLines("..\\Coursera-SwiftKey\\final\\en_US\\en_US.news.txt")

files <- c("..\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter.txt",
           "..\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt",
           "..\\Coursera-SwiftKey\\final\\en_US\\en_US.news.txt")

allFiles <- unname(sapply(files, readLines))
allFiles <- unlist(allFiles)

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

# estrapolate the number
as.numeric(myTab["what"])

grep("http:[[:alnum:]]*", twitter, value = TRUE) #remove url
# "#\\S+", "", x remove hashtag
# twitter handles "@\\S+", "", x ???

grep("\\.$", myText, value = TRUE)
str_replace_all(myText,"\\.", "\n")

# to plot
ggplot(as.data.frame(wordsCount[1:60]), aes(x=text, y = Freq)) + geom_bar(stat="identity") + coord_flip()
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