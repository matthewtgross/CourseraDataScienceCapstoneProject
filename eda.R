## Coursera Data Science Capstone Project Exploratory Data Analysys

# load en_US data
save_wd <- getwd()

# set current working directory
#setwd("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\Coursera-SwiftKey\\final\\en_US")
#setwd("C:\\Users\\v-matgro\\Downloads\\Coursera-SwiftKey\\final\\en_US")
#setwd("C:\\Users\\v-matgro\\Downloads\\Coursera-SwiftKey\\final")
setwd("/home/matt/git/CourseraDataScienceCapstoneProject/final")

# load libraries
library(tm)
library(wordnet)
library(openNLP)
library(SnowballC)

# load data
blogs <- system.file("texts", "en_US.blogs.txt", package = "tm")

#bDir <- system.file("texts", "txt", package = "tm")
bDir <- "en_US"

b <-  Corpus(DirSource(bDir),readerControl = list(reader = readPlain, language = "en_US", load = TRUE))
#b <-  Corpus(SimpleSource(blogs),readerControl = list(reader = readPlain, language = "en_US", load = TRUE))
b <- tm_map(b, removeNumbers)
b <- tm_map(b, removePunctuation)
b <- tm_map(b, stripWhitespace)
b <- tm_map(b, tolower)

b <- tm_map(b, removeWords, stopwords("english"))
b <- tm_map(b, stemDocument, language = "english")
b <- tm_map(b, PlainTextDocument)

bdtm <-DocumentTermMatrix(b) 
bdtm <- removeSparseTerms(bdtm, 0.75)

summary(b)
