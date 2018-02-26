library(stringr)
library(quanteda)

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

bigrams <- dfm(ngramsCorpus, ngrams = 2, concatenator = " ", 
               remove = stopwords("english"), removePunct = FALSE)

trigrams <- dfm(ngramsCorpus, ngrams = 3, concatenator = " ", 
                remove = stopwords("english"), removePunct = FALSE)

rm(ngramsCorpus)
gc()

trigramFrequencyMatrix <- textstat_frequency(trigrams)

rm(trigrams)
gc()

library(data.table)
library(tidyr)
a <- head(as.data.table(trigramFrequencyMatrix))
bigramsDictionary <- as.data.table(bigramFrequencyMatrix)
rm(bigramFrequencyMatrix)
bigramsDictionary <- bigramsDictionary %>% 
                        separate(feature, c("base", "word1"), sep = " ")
bigramsDictionary <- bigramsDictionary[, head(.SD, 3), base]
good <- complete.cases(bigramsDictionary)
bigramsDictionary <- bigramsDictionary[good, ]
bigramsDictionary[base %in% "'", .(word1, frequency)]

saveRDS(bigramsDictionary, "bigrams.RDS")

#############################trigrams###########################################

trigramsDictionary <- as.data.table(trigramFrequencyMatrix)
temp <- copy(trigramFrequencyMatrix)
rm(trigramFrequencyMatrix)
trigramsDictionary <- trigramsDictionary %>% 
        separate(feature, c("baseTemp", "base2Temp", "predWord"), sep = " ") %>%
        unite(base, c("baseTemp", "base2Temp"), sep = " ")
trigramsDictionary <- trigramsDictionary[, head(.SD, 3), base]
good <- complete.cases(trigramsDictionary)
trigramsDictionary <- trigramsDictionary[good, ]
trigramsDictionary[base %in% "'", .(word1, frequency)]

saveRDS(trigramsDictionary, "trigrams.RDS")


########################fourgrams############################################


fourgrams <- dfm(ngramsCorpus, ngrams = 4, concatenator = " ", 
                 remove = stopwords("english"), removePunct = FALSE)


rm(ngramsCorpus)
gc()

fourFrequencyMatrix <- textstat_frequency(fourgrams)

rm(fourgrams)
gc()

library(data.table)
library(tidyr)
fourgramsDictionary <- as.data.table(fourFrequencyMatrix)
rm(bigramFrequencyMatrix)
fourgramsDictionary <- fourgramsDictionary %>% 
        separate(feature, c("baseTemp", "base2Temp", "base3Temp","predWord"), sep = " ") %>%
        unite(base, c("baseTemp", "base2Temp", "base3Temp"), sep = " ")
fourgramsDictionary <- fourgramsDictionary[, head(.SD, 3), base]
fourgramsDictionary[base %in% "'", .(word1, frequency)]

saveRDS(fourgramsDictionary, "fourgrams.RDS")


###############################fivegrams####################################

fivegrams <- dfm(ngramsCorpus, ngrams = 5, concatenator = " ", 
                 remove = stopwords("english"), removePunct = FALSE)

fivegramsFrequencyMatrix <- textstat_frequency(fivegrams)

fivegramsDictionary <- as.data.table(fivegramsFrequencyMatrix)

fivegramsDictionary <- fivegramsDictionary %>% 
        separate(feature, c("baseTemp", "base2Temp", "base3Temp", "base4Temp", "predWord"),
                 sep = " ") %>%
        unite(base, c("baseTemp", "base2Temp", "base3Temp", "base4Temp"), sep = " ")

fivegramsDictionary <- fivegramsDictionary[, head(.SD, 3), base]

saveRDS(fivegramsDictionary, "fivegrams.RDS")

as.data.frame(test) %>% separate(feature, c("base", "word2"), sep = " ") %>%
        group_by(base) %>% mutate(myc = paste(word2, collapse = " ")) %>%
        separate(myc, c("word2", "word3", "word4"), sep = " ", fill = "right")


c<-as.data.frame(bigramsFrequencyMatrix) %>% 
        separate(feature, c("base", "word2"), sep = " ") %>%
        group_by(base) %>% mutate(myc = paste(word2, collapse = " ")) %>%
        separate(myc, c("word2", "word3", "word4"), sep = " ", fill = "right")


DT <- as.data.table(testMatrix)
DT <- DT %>% separate(feature, c("base", "word2"), sep = " ")

DT %>% group_by(base) %>% filter(row_number(base) == 2)


b<-a %>% group_by(word1) %>% mutate(myc = paste(word2, frequency, collapse = " "))
c <- b %>% mutate(d = str_count(myc, "\\S+"))