library(stringr)
library(tm)
library(ngram)

twitter <- readLines("..\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter.txt")
blogs <- readLines("..\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt")
news <- readLines("..\\Coursera-SwiftKey\\final\\en_US\\en_US.news.txt")

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



# Links:
# https://rpubs.com/brianzive/textmining
# https://rstudio-pubs-static.s3.amazonaws.com/265713_cbef910aee7642dc8b62996e38d2825d.html
# https://stackoverflow.com/questions/31316274/implementing-n-grams-for-next-word-prediction
# https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
# http://www.mjdenny.com/Text_Processing_In_R.html
