## Coursera Data Science Capstone Project Exploratory Data Analysys

# load en_US data
save_wd <- getwd()

# set current working directory
#setwd("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\Coursera-SwiftKey\\final\\en_US")
setwd("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\Coursera-SwiftKey\\final")
#setwd("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\Coursera-SwiftKey\\final\\en_US")
#setwd("C:\\Users\\v-matgro\\Downloads\\Coursera-SwiftKey\\final\\en_US")
#setwd("C:\\Users\\v-matgro\\Downloads\\Coursera-SwiftKey\\final")

# load libraries
library(tm)
library(wordnet)
setDict("C:/Program Files (x86)/WordNet/2.1/dict")
library(openNLP)
library(SnowballC)
library(RWeka)
library(wordcloud)
library(ggplot2)

# load data
#txt <- system.file("texts","txt",package="tm")
#(ovid <- Corpus(DirSource(txt),readerControl= list(render = readPlain, language = "la",load=TRUE)))

#rdevel <- Corpus(DirSource("en_US"),readerControl = list(reader = readPlain, language = "en_US", load = TRUE))


#blog <- system.file("en_US.blogs.txt","txt",package="tm")

#head(blog)


# load data
#blogs <- system.file("texts", "en_US.blogs.txt", package = "tm")

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

b <- head(blogs,20)
ggplot(b,aes(y=freq,x=word)) + geom_bar(stat="identity")

blogs <- read.csv("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\en_US.blogs-usort-c.txt",header=FALSE,sep=" ")
names(blogs) <- c("freq","word")

