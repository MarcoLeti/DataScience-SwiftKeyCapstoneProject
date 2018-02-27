# This script models the data and generates the files needed for the shiny app.
# Due to the high volume it is necessary to call every single part separately in a new session.

library(stringr)
library(quanteda)
library(data.table)
library(tidyr)

twitter <- readLines(".\\Data\\final\\en_US\\en_US.twitter.txt")
blogs <- readLines(".\\Data\\final\\en_US\\en_US.blogs.txt")
news <- readLines(".\\Data\\final\\en_US\\en_US.news.txt")
files <- unlist(list(twitter, blogs, news))
rm(twitter, blogs, news)

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

ngramsData <- PrepareTheData(files)
ngramsCorpus <- corpus(ngramsData)
rm(ngramsData, files)
gc()

#################################### top unigrams #############################

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

######################################### bigrams #############################

bigrams <- dfm(ngramsCorpus, ngrams = 2, concatenator = " ", 
               remove = stopwords("english"), removePunct = FALSE)

rm(ngramsCorpus)
gc()

bigramFrequencyMatrix <- textstat_frequency(bigrams)

rm(trigrams)
gc()

bigramsDictionary <- as.data.table(bigramFrequencyMatrix)
rm(bigramFrequencyMatrix)
bigramsDictionary <- bigramsDictionary %>% 
                        separate(feature, c("base", "word1"), sep = " ")
bigramsDictionary <- bigramsDictionary[, head(.SD, 3), base]
bigramsDictionary[base %in% "'", .(word1, frequency)]

saveRDS(bigramsDictionary, "bigrams.RDS")

############################# trigrams ###########################################

trigrams <- dfm(ngramsCorpus, ngrams = 3, concatenator = " ", 
                remove = stopwords("english"), removePunct = FALSE)
trigramFrequencyMatrix <- textstat_frequency(trigrams)
trigramsDictionary <- as.data.table(trigramFrequencyMatrix)
rm(trigramFrequencyMatrix)
trigramsDictionary <- trigramsDictionary %>% 
        separate(feature, c("baseTemp", "base2Temp", "predWord"), sep = " ") %>%
        unite(base, c("baseTemp", "base2Temp"), sep = " ")
trigramsDictionary <- trigramsDictionary[, head(.SD, 3), base]
trigramsDictionary[base %in% "'", .(word1, frequency)]

saveRDS(trigramsDictionary, "trigrams.RDS")


######################## fourgrams ############################################

fourgrams <- dfm(ngramsCorpus, ngrams = 4, concatenator = " ", 
                 remove = stopwords("english"), removePunct = FALSE)
rm(ngramsCorpus)
gc()
fourFrequencyMatrix <- textstat_frequency(fourgrams)
rm(fourgrams)
gc()


fourgramsDictionary <- as.data.table(fourFrequencyMatrix)
rm(fourFrequencyMatrix)
fourgramsDictionary <- fourgramsDictionary %>% 
        separate(feature, c("baseTemp", "base2Temp", "base3Temp","predWord"), sep = " ") %>%
        unite(base, c("baseTemp", "base2Temp", "base3Temp"), sep = " ")
fourgramsDictionary <- fourgramsDictionary[, head(.SD, 3), base]
fourgramsDictionary[base %in% "'", .(word1, frequency)]

saveRDS(fourgramsDictionary, "fourgrams.RDS")


############################### fivegrams ####################################

fivegrams <- dfm(ngramsCorpus, ngrams = 5, concatenator = " ", 
                 remove = stopwords("english"), removePunct = FALSE)

rm(ngramsCorpus)
gc()

fivegramsFrequencyMatrix <- textstat_frequency(fivegrams)

rm(fivegrams)
gc()

fivegramsDictionary <- as.data.table(fivegramsFrequencyMatrix)
rm(fivegramsFrequencyMatrix)
fivegramsDictionary <- fivegramsDictionary %>% 
        separate(feature, c("baseTemp", "base2Temp", "base3Temp", "base4Temp", "predWord"),
                 sep = " ") %>%
        unite(base, c("baseTemp", "base2Temp", "base3Temp", "base4Temp"), sep = " ")

fivegramsDictionary <- fivegramsDictionary[, head(.SD, 3), base]

saveRDS(fivegramsDictionary, "fivegrams.RDS")