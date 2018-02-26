library(shiny)
library(stringr)
library(data.table)
library(shinyjs)
library(shinythemes)
source("..\\predictionModel.R")

#unigrams <- readRDS("data/topUnigrams.rds")
#setkeyv(unigrams, "feature")
#bigrams <- readRDS("data/bigrams.rds")
#setkeyv(bigrams, "base")
#trigrams <- readRDS("data/trigrams.rds")
#setkeyv(trigrams, "base")
#fourgrams <- readRDS("data/fourgrams.rds")
#setkeyv(fourgrams, "base")
#fivegrams <- readRDS("data/fivegrams.rds")
#setkeyv(fivegrams, "base")

unigrams
bigrams
trigrams
fourgrams
fivegrams


# no deal with punctuation
# no text correction