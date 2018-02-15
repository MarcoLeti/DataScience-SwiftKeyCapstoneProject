PrepareTheData <- function(text, n = 2) {
        if (n < 1) stop("n must be bigger than 0")
        require(stringr)
        text <- tolower(text)
        text <- str_replace_all(text,"#\\S+", " ")
        text <- str_replace_all(text,"(f|ht)tp(s?)://(.*)[.][a-z]+", " ")
        text <- str_replace_all(text,"\\.+", ". ")
        text <- str_replace_all(text,"\\?|\\!", ". ")
        if (n >= 2) {
                text <- str_replace_all(text,"[^a-zA-Z\\.\\,\\']", " ")
        } else {
                text <- str_replace_all(text,"[^a-zA-Z\\.\\']", " ")
        }
        text <- str_replace_all(text,"[\\s]+", " ")
        text <- str_replace_all(text," $|\\.$|\\. $", "")
        text <- str_replace_all(text,"\\. ", ".")
        text <- str_replace_all(text,"\\, ", ",")
        text <- str_split(text, "\\.|\\,")
        text <- unlist(text)
        return(text)
}

set.seed(3783)
dataSample <- sample(files, length(files)*0.01)
dataSample <- PrepareTheData(dataSample)
#corpus <- VCorpus(VectorSource(dataSample))
corpus <- corpus(dataSample)
bigrams <- dfm(corpus, ngrams = 2, concatenator = " ", remove = stopwords("english"))
#bigrams <- sort(colSums(as.matrix(bigrams)), decreasing = TRUE)
#bigrams <- data.frame(n_gram = names(bigrams), freq = bigrams, row.names = seq(1:length(bigrams)))

bigrams <- textstat_frequency(bigrams, n = 50)

g <- ggplot(bigrams, aes(x = reorder(feature, frequency), y = frequency))
g <- g + labs(title = "50 top unigrams", x = "N-Gram", y = "Frequency")
g + geom_bar(stat="identity", fill="#F8766D", colour="black") + coord_flip()



plotTopFeatures <- function(frequencyMatrix, n = 50, fill = "black") {
        frequencyMatrix <- textstat_frequency(frequencyMatrix, n = n)
        g <- ggplot(frequencyMatrix, aes(x = reorder(feature, frequency), y = frequency))
        g <- g + labs(title = "50 top unigrams", x = "N-Gram", y = "Frequency")
        g + geom_bar(stat="identity", fill=fill, colour="black") + coord_flip()
}



#not needed anymore
bigrams <- data.frame(feature = bigrams$feature, frequency = bigrams$frequency)

top <- bigrams[1:50, ]
g <- ggplot(top, aes(x = reorder(feature, frequency), y = frequency))
g <- g + labs(title = "50 top unigrams", x = "N-Gram", y = "Frequency")
g + geom_bar(stat="identity", fill="#F8766D", colour="black") + coord_flip()