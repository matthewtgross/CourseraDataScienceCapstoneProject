# Title: Text Mining - R-bloggers: 
# URL: http://www.r-bloggers.com/text-mining/

# NOTE: Could not get this to work correctly.

library(tm)
library(RTextTools)
library(topicmodels)
library(twitteR)


#These files can be just raw text. For example it could be simply copied and pasted from a Web site.
dir = "C:\\Documents and Settings\\clints\\My Documents\\LDA-S";
filenames = list.files(path=dir,pattern="\\.txt");
setwd(dir);

docs = NULL;
titles = NULL;

for (filename in filenames){
  #here I specify a file that contains all the titles of the documents
  if(filename=="titles.txt"){
    titles = paste(readLines(file(filename)));
  } else {
    docs = c(docs,list( paste(readLines(file(filename)), collapse="\n") ));
  }
}

# error on the line below: Error in get_oauth_sig() : OAuth has not been registered for this session
#twitter_feed <- searchTwitter('@cnn',n=150)
# had to change to (Ref: http://thinktostart.com/twitter-authentification-with-r/)
api_key <- "U5QwfYL9OTdPbPvwd7N7CiZNv"
api_secret <- "lL9W4DJ0QaFWbXJ3TXyt8K9Q6zsNbAj06UEZxpoiAZl2kv2Ktr"
access_token <- "29567102-erOe56crswzPjU3uw5EfahTfmPSJlHIK9LwV8lPLX"
access_token_secret <- "PVH0Mvuw0KcSWD1hOQwjuZKL8rpLYSAkt2AzhWz4uYLvz"
#api_key <- "U5QwfYL9OTdPbPvwd7N7CiZN"
#api_secret <- "lL9W4DJ0QaFWbXJ3TXyt8K9Q6zsNbAj06UEZxpoiAZl2kv2Ktr"
#access_token <- "29567102-erOe56crswzPjU3uw5EfahTfmPSJlHIK9LwV8lPLX"
#access_token_secret <- "PVH0Mvuw0KcSWD1hOQwjuZKL8rpLYSAkt2AzhWz4uYLvz"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# test with
#searchTwitter("iphone")


twitter_feed <- searchTwitter('@cnn',n=150)

### Optional twitter feed retrieval
##twitter_feed <- userTimeline("rdatamining", n=150);
###

df <- do.call("rbind", lapply(twitter_feed, as.data.frame))
myCorpus <- Corpus(VectorSource(df$text))

k = length(docs3)
myCorpus = Corpus(VectorSource(docs))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myStopwords = c(stopwords('english'), "available", "via")
idx = which(myStopwords == "r")
myStopwords = myStopwords[-idx]
myCorpus = tm_map(myCorpus, removeWords, myStopwords)

dictCorpus = myCorpus

myCorpus = tm_map(myCorpus, stemDocument)

# Error, had to change this
#myCorpus = tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)
# to this
myCorpus = tm_map(myCorpus, stemCompletion, dictionary=dictCorpus, lazy=TRUE)

myDtm = DocumentTermMatrix(myCorpus, control = list(minWordLength = 3))

findFreqTerms(myDtm, lowfreq=50)
#find the probability a word is associated
findAssocs(myDtm, 'find_a_word', 0.5)

# Wordcloud
library(wordcloud)
m = as.matrix(myDtm)
v = sort(colSums(m), decreasing=TRUE)
myNames = names(v)
k = which(names(v)=="miners")
myNames[k] = "mining"
d = data.frame(word=myNames, freq=v)
wordcloud(d$word, colors=c(3,4), random.color=FALSE, d$freq, min.freq=20)

# Latent Dirichlet Allocation
k = 2
SEED = 1234
my_TM =
  list(VEM = LDA(myDtm, k = k, control = list(seed = SEED)),
       VEM_fixed = LDA(myDtm, k = k,
                       control = list(estimate.alpha = FALSE, seed = SEED)),
       Gibbs = LDA(myDtm, k = k, method = "Gibbs",
                   control = list(seed = SEED, burnin = 1000,
                                  thin = 100, iter = 1000)),
       CTM = CTM(myDtm, k = k,
                 control = list(seed = SEED,
                                var = list(tol = 10^-4), em = list(tol = 10^-3))))

Topic = topics(my_TM[["VEM"]], 1)

#top 5 terms for each topic in LDA
Terms = terms(my_TM[["VEM"]], 5)
#Terms

(my_topics =
   topics(my_TM[["VEM"]]))

most_frequent = which.max(tabulate(my_topics))

terms(my_TM[["VEM"]], 10)[, most_frequent]