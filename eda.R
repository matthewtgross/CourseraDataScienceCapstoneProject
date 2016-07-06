## Coursera Data Science Capstone Project Exploratory Data Analysys

# set folder locations

# load en_US data
save_wd <- getwd()

# set current working directory
#setwd("/home/matt/git/CourseraDataScienceCapstoneProject/final")
setwd("/home/matt/git/CourseraDataScienceCapstoneProject/test")
#setwd("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\Coursera-SwiftKey\\final\\en_US")
#setwd("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\Coursera-SwiftKey\\final")
#setwd("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\Coursera-SwiftKey\\final\\en_US")
#setwd("C:\\Users\\v-matgro\\Downloads\\Coursera-SwiftKey\\final\\en_US")
#setwd("C:\\Users\\v-matgro\\Downloads\\Coursera-SwiftKey\\final")

# Before we begin, this doc will serve as a source for all notes and links to complete all weeks of this project.


# Resources on Natural Language Processing:
# - Natural language processing Wikipedia page: https://en.wikipedia.org/wiki/Natural_language_processing
# - Text Mining infrastructure in R: https://www.jstatsoft.org/article/view/v025i05
# - CRAN Task View: Natural Language Processing: https://cran.r-project.org/web/views/NaturalLanguageProcessing.html
# - Coursera course on NLP (not in R): https://www.coursera.org/course/nlp

# Dataset
# Capstone Dataset: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
# Dataset README: http://www.corpora.heliohost.org/aboutcorpus.html


# More resources:
# https://www.kaggle.com/c/yelp-recruiting/forums/t/4803/text-analysis-dealing-with-memory-errors-due-to-the-large-corpus
# http://stackoverflow.com/questions/27393728/text-mining-with-r-package-tm-idf-for-a-large-corpus
# https://www.kaggle.com/c/job-salary-prediction/forums/t/4076/large-scale-text-mining-in-r/33781
# http://stackoverflow.com/questions/16287546/trying-to-remove-words-from-a-documenttermmatrix-in-order-to-use-topicmodels
# http://stackoverflow.com/questions/13366897/r-documenttermmatrix-control-list-not-working-silently-ignores-unknown-paramete/13370840#13370840
# http://stackoverflow.com/questions/25330753/more-efficient-means-of-creating-a-corpus-and-dtm
# (good) https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/
# (bigram) https://en.wikipedia.org/wiki/Bigram
# (good) https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
# https://deltadna.com/blog/text-mining-in-r-for-term-frequency/
# http://stackoverflow.com/questions/30435054/how-to-show-corpus-text-in-r-tm-package
# http://www.rdatamining.com/examples/text-mining
# (good) http://www.rdatamining.com/examples/text-mining

# PDF Resources on the web
# http://handsondatascience.com/TextMiningO.pdf

# stringi
# http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf





## Week 1

# Additional resources on tm
# News fror package 'tm' 
# Changes in tm version 0.6-2
# URL: https://cran.r-project.org/web/packages/tm/news.html



## Week 2

## Week 3

## Week 4

## Week 5

## Week 6

## Week 7


# Recommended tm (and related) libraries
library(boilerpipeR)
library(corpora)
library(gsubfn)
library(kernlab)
library(KoNLP)
library(koRpus)
library(languageR)
library(lda)
library(lsa)
library(maxent)
library(movMF)
library(openNLP)
library(qdap)
#library(RcmdrPlugin.temis) # install failed
library(RKEA)
library(RTextTools)
library(RWeka)
library(skmeans)
library(SnowballC)
library(stringi)
library(tau)
library(textcat)
library(textir)
library(textreuse)
library(tm)
library(tm.plugin.alceste)
library(tm.plugin.dc)
library(tm.plugin.europresse)
library(tm.plugin.factiva)
library(tm.plugin.lexisnexis)
library(tm.plugin.mail)
library(tm.plugin.webmining)
#library(topicmodels) # failed to install
library(wordcloud)
library(wordnet)
#Note: to get wordnet to load correctly had to add the following to .bashrc file: export WNHOME=/usr/share/wordnet-3.0/
library(zipfR)



# load libraries
#library(tm)
#library(openNLP)
#library(SnowballC)
#library(RWeka)
#library(wordcloud)
library(ggplot2)

## Not sure we need to use wordnet yet.  Commenting out.
#library(wordnet)
#setDict("C:/Program Files (x86)/WordNet/2.1/dict")

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
b <- tm_map(b, function(i) iconv(i, "latin1", "ASCII", sub=""))
b <- tm_map(b, tolower)
b <- tm_map(b, removeWords, stopwords("english"))
b <- tm_map(b, stemDocument, language = "english")
b <- tm_map(b, PlainTextDocument)

#bdtm <-DocumentTermMatrix(b) 
bdtm <-DocumentTermMatrix(b, control=list(wordLengths=c(3, 30), bounds = list(global = c(3,3))))
bdtm <- removeSparseTerms(bdtm, 0.75)

#inspect(bdtm[1:3,1:100])
#freq <- colSums(as.matrix(bdtm))
#freq
freq <- colSums(as.matrix(bdtm))
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]

findFreqTerms(bdtm,lowfreq=6)

summary(b)

b <- head(blogs,20)
ggplot(b,aes(y=freq,x=word)) + geom_bar(stat="identity")

blogs <- read.csv("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\en_US.blogs-usort-c.txt",header=FALSE,sep=" ")
names(blogs) <- c("freq","word")

