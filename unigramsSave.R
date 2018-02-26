library(quanteda)
library(data.table)
twitter <- readLines(".\\Data\\final\\en_US\\en_US.twitter.txt")
blogs <- readLines(".\\Data\\final\\en_US\\en_US.blogs.txt")
news <- readLines(".\\Data\\final\\en_US\\en_US.news.txt")

files <- unlist(list(twitter, blogs, news))
rm(twitter, blogs, news)
gc()

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
unigramsCorpus <- corpus(unigramsData)
rm(unigramsData)
gc()

unigrams <- dfm(unigramsCorpus, ngrams = 1, concatenator = " ",
                remove = stopwords("english"), removePunct = FALSE)
rm(unigramsCorpus)
gc()

unigramFrequencyMatrix <- textstat_frequency(unigrams, n = 1000)
rm(unigrams)
gc()
unigramFrequencyMatrix <- as.data.table(unigramFrequencyMatrix)
unigramFrequencyMatrix <- unigramFrequencyMatrix[!grepl("\\b\\w{1,2}\\b", feature)]
unigramFrequencyMatrix <- unigramFrequencyMatrix[grepl("^[a-zA-z]", feature)]

saveRDS(unigramFrequencyMatrix, "topUnigrams.rds")
