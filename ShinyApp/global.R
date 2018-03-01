library(shiny)
library(stringr)
library(data.table)
library(shinythemes)
source("functions/predictionModel.R")

unigrams <- readRDS("data/topUnigrams.rds")
bigrams <- readRDS("data/bigrams.rds")
setkeyv(bigrams, "base")
trigrams <- readRDS("data/trigrams.rds")
setkeyv(trigrams, "base")
fourgrams <- readRDS("data/fourgrams.rds")
setkeyv(fourgrams, "base")
fivegrams <- readRDS("data/fivegrams.rds")
setkeyv(fivegrams, "base")

#unigrams
#bigrams
#trigrams
#fourgrams
#fivegrams