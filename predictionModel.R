library(stringr)
library(data.table)

#bigrams <- readRDS("bigrams.RDS")
#setkeyv(bigrams, "base")
#trigrams <- readRDS("trigrams.RDS")
#setkeyv(trigrams, "base")
#fourgrams <- readRDS("fourgrams.RDS")
#setkeyv(fourgrams, "base")
#fivegrams <- readRDS("fivegrams.RDS")
#setkeyv(fivegrams, "base")

#bigrams[base %in% "ice", .(predWord, frequency)]
#trigrams[base %in% "monitoring the", .(predWord, frequency)]
#fourgrams[base %in% "want an ice", .(predWord, frequency)]
#fivegrams[base %in% "a vote has been", .(predWord, frequency)]

#test[,extracted:=sapply(strsplit(original,'\\s+'),function(v) paste(collapse=' ',tail(v,5L)))]
#source("..\\numbers2words.R")
source("C:/Users/Marco_Letico/Desktop/DataScience-SwiftKeyCapstoneProject/numbers2words.R")
predictWord <- function(text) {
        predict <- str_detect(text, " $")
        if (predict & !is.na(text)) {
                text <- str_to_lower(text)
                text <- numbers2wordsAll(text)
                text <- str_replace_all(text,"[^a-zA-Z\\'\\ ]", "") 
                text <- str_replace_all(text,"[\\s]+$", "")
                text <- str_replace_all(text,"[\\s]+", " ")
                count <- str_count(text, "\\S+")
                pattern <- "\\w+( \\w+){0,%d}$"
                predictedWords <- vector()
                if (count >= 4) {
                        tmpText <- str_extract(text, sprintf(pattern, 3))
                        predictedWords <- fivegrams[base %in% tmpText, predWord]
                }
                
                if (length(predictedWords) == 3) return(predictedWords)
                
                if (count == 3 | length(predictedWords) < 3) {
                        tmpText <- str_extract(text, sprintf(pattern, 2))
                        tmp <- fourgrams[base %in% tmpText, predWord]
                        predictedWords <- c(predictedWords,
                                            tmp[!(tmp %in% predictedWords)])
                }
                
                if (length(predictedWords) >= 3) return(predictedWords[1:3])
                
                if (count == 2 | length(predictedWords) < 3) {
                        tmpText <- str_extract(text, sprintf(pattern, 1))
                        tmp <- trigrams[base %in% tmpText, predWord]
                        predictedWords <- c(predictedWords, 
                                            tmp[!(tmp %in% predictedWords)])
                }
                
                if (length(predictedWords) >= 3) return(predictedWords[1:3])
                
                if (count == 1 | length(predictedWords) < 3) {
                        tmpText <- str_extract(text, sprintf(pattern, 0))
                        tmp <-  bigrams[base %in% tmpText, predWord]
                        predictedWords <- c(predictedWords, 
                                            tmp[!(tmp %in% predictedWords)])
                }
                
                if (length(predictedWords) >= 3) return(predictedWords[1:3])
                
                randomWords <- sample(unigrams$feature, size = 3)
                predictedWords <- c(predictedWords, 
                                    randomWords[!(randomWords %in% predictedWords)])
                
                return(predictedWords[1:3])
        }
}
################################
