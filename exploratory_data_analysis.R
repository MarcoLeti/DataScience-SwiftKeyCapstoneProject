library(stringr)
library(tm)
library(ngram)

twitter <- readLines("..\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter.txt")
blogs <- readLines("..\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt")
news <- readLines("..\\Coursera-SwiftKey\\final\\en_US\\en_US.news.txt")

CleanTheData <- function(text) {
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

# step before call the below function str_split(text, "\\.")

CreateNGrams <- function(text, n) {
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


###############################################################################

myText <- twitter
myText <- tolower(myText)
# replace everything that is not a character or a number
myText <- str_replace_all(myText,"[^a-zA-Z0-9]", " ")
# collapse all the white spaces bigger than one
myText <- str_replace_all(myText,"[\\s]+", " ")
myText <- str_split(myText, " ")

for (i in 1:length(myText)) {
        if (any(myText[[i]] == "")) {
                myNullChar <- which(myText[[i]] == "")
                myText[[i]] <- myText[[i]][-myNullChar]
        }
}

myTab <- unlist(myText)
myTab <- table(myTab)
myTab <- sort(myTab, decreasing = TRUE)
myTab[30]

# estrapolate the number
as.numeric(myTab["what"])

grep("http:[[:alnum:]]*", twitter, value = TRUE) #remove url
# "#\\S+", "", x remove hashtag
# twitter handles "@\\S+", "", x ???

grep("\\.$", myText, value = TRUE)
str_replace_all(myText,"\\.", "\n")


# process data for prediction:

a <- as.data.frame(str_split(twitter[1:2000], "\\.+|\\?|\\!", simplify = TRUE))
b <- data.frame()
for (i in 1:nrow(a)) {
        for (j in 1:ncol(a)) {
                temp <- as.data.frame(str_split(a[i, j], " ", simplify = TRUE))
                b <- rbind.fill(b, temp)
        }
        
}

c <- matrix(ncol = 2)
d <- matrix(ncol = 2)
for (i in 1: length(myText)){
        a <- str_split(myText[i], "\\.+|\\?|\\!", simplify = TRUE)
        for (j in 1:ncol(a)) {
                temp <- str_split(a[1, j], " ", simplify = TRUE)
                for (z in 2:ncol(temp)) {
                        c[1, 1] <- temp[1, z - 1]
                        c[1, 2] <- temp[1, z]
                        d <- rbind(d, c)
                }
        }
}

k <- a[1, 1]
f <- ngrams(myTab, 2L)
# Links:
# https://rpubs.com/brianzive/textmining
# https://rstudio-pubs-static.s3.amazonaws.com/265713_cbef910aee7642dc8b62996e38d2825d.html
# https://stackoverflow.com/questions/31316274/implementing-n-grams-for-next-word-prediction
# https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
# http://www.mjdenny.com/Text_Processing_In_R.html