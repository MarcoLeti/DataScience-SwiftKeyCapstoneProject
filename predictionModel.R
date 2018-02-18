library(stringr)
library(quanteda)

twitter <- readLines(".\\Data\\final\\en_US\\en_US.twitter.txt")
blogs <- readLines(".\\Data\\final\\en_US\\en_US.blogs.txt")
news <- readLines(".\\Data\\final\\en_US\\en_US.news.txt")

files <- unlist(list(twitter, blogs, news))

PrepareTheData <- function(text, unigram = FALSE) {
        require(stringr)
        text <- tolower(text)
        text <- str_replace_all(text,"#\\S+", " ")
        text <- str_replace_all(text,"(f|ht)tp(s?)://(.*)[.][a-z]+", " ")
        text <- str_replace_all(text,"\\.+", ". ")
        text <- str_replace_all(text,"\\?|\\!", ". ")
        if (!unigram) {
                text <- str_replace_all(text,"[^a-zA-Z\\.\\,\\']", " ")
        } else {
                text <- str_replace_all(text,"[^a-zA-Z\\.\\']", " ")
        }
        text <- str_replace_all(text,"\\ ' |\\' |\\ '", " ")
        text <- str_replace_all(text,"[\\s]+", " ")
        text <- str_replace_all(text," s ", "'s ")
        text <- str_replace_all(text," t ", "'t ")
        text <- str_replace_all(text," u ", " you ")
        text <- str_replace_all(text," $|\\.$|\\. $", "")
        text <- str_replace_all(text,"\\. ", ".")
        text <- str_replace_all(text,"\\, ", ",")
        text <- str_split(text, "\\.|\\,")
        text <- unlist(text)
        return(text)
}

unigramsData <- PrepareTheData(files, unigram = TRUE)
multigramsData <- PrepareTheData(files)
unigramsCorpus <- corpus(unigramsData)
multigramsCorpus <- corpus(unigramsData)

unigrams <- dfm(unigramsCorpus, ngrams = 1, concatenator = " ",
                remove = stopwords("english"), removePunct = FALSE)

bigrams <- dfm(multigramsCorpus, ngrams = 2, concatenator = " ", 
               remove = stopwords("english"), removePunct = FALSE)

trigrams <- dfm(multigramsCorpus, ngrams = 3, concatenator = " ", 
                remove = stopwords("english"), removePunct = FALSE)