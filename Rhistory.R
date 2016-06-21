data <- cbind(data, means40, sd40, var40)
data
head(data)
hist(data$var40)
mean(data$var40)
((data[,1:40] - mean(data[,1:40]))^2)/19
(sum(data[,1:40] - mean(data[,1:40]))^2)/19
data[1,1:40]
sum(data[1,1:40] - mean(data[1,1:40]))
sum(data[1,1:40] - mean(as.list(data[1,1:40])))
mean(data[1,1:40])
mean(as.list(data[1,1:40]))
apply(data[1,1:40], mean)
?apply
dim(data)
dim(data[1,1:40])
?dim
dimnames(data[1,1:40])
data[1,1:40]
apply(data[1,1:40], 1, mean)
sum(data[1,1:40] - apply(data[1,1:40],1,mean))
sum(data[1,1:40] - apply(data[1,1:40],1,mean))^2
sum(data[1,1:40] - apply(data[1,1:40],1,mean))^2/19
(sum(data[1,1:40] - apply(data[1,1:40],1,mean))^2)/19
apply(data[1,1:40],1,var)
apply(data[1,1:40],1,sd)
apply(data[2,1:40],1,var)
apply(data[1:5,1:40],1,var)
mean(data$sd40)
lambda <- 0.2
no <- 40
sims <- 1000
ff <- function(n, l) { rexp(n,l)}
data <- data.frame(t(replicate(expr = ff(no,lambda), n=sims)))
names(data) <- paste("value_",rep(1:40), sep="")
means40 <- apply(data, 1, mean)
sd40 <- apply(data, 1, sd)
var40 <- apply(data, 1, var)
meanvar40 <- mean(var40)
meansd40 <- mean(sd40)
data <- cbind(data, means40, sd40, var40)
meansd40
meanvar40
?round
meanmeans40
lambda <- 0.2
no <- 40
sims <- 1000
ff <- function(n, l) { rexp(n,l)}
data <- data.frame(t(replicate(expr = ff(no,lambda), n=sims)))
names(data) <- paste("value_",rep(1:40), sep="")
means40 <- apply(data, 1, mean)
sd40 <- apply(data, 1, sd)
var40 <- apply(data, 1, var)
meanmeans40 <- mean(means40)
meanvar40 <- mean(var40)
meansd40 <- mean(sd40)
data <- cbind(data, means40, sd40, var40)
meanmeans40
round(meanmeans40,2)
round(meanmeans40,3)
a <- 1; b<- 2; c<-3
ggplot(data, aes(x=var40)) + geom_histogram(binwidth=2, fill="cornsilk",colour="grey60",size=.2) + stat_vline(xintercept = 25, color = "blue", linetype="solid", size = 1) + stat_vline(xintercept = meanvar40, color = "red", linetype="dotted", size = 1) + xlab("Sample Variance") + ylab("Frequency")
lambda <- 0.2; no <- 40; sims <- 1000
ff <- function(n, l) { rexp(n,l)}
data <- data.frame(t(replicate(expr = ff(no,lambda), n=sims)))
names(data) <- paste("value_",rep(1:40), sep="")
means40 <- apply(data, 1, mean); sd40 <- apply(data, 1, sd); var40 <- apply(data, 1, var)
meanmeans40 <- mean(means40); meanvar40 <- mean(var40); meansd40 <- mean(sd40)
sum(data - means40)^2/39
sqrt(sum(data - means40)^2/39)
round(sqrt(sum(data - means40)^2/39),3)
(data - means40)^2
a <- (data - means40)^2
sum(a)
sum(a)/39
a <- sum((data - means40)^2/39)
a
sqrt(a)
data("ToothGrowth")
library(UsingR)
tooth <- data("ToothGrowth")
summary(tooth)
?ToothGrowth
library(datasets)
tooth <- data("ToothGrowth")
summary(tooth)
library(datasets)
data("ToothGrowth")
summary(ToothGrowth)
nrow(ToothGrowth)
ncol(ToothGrowth)
head(ToothGrowth)
ggplot(ToothGrowth, aes(x=len, y=dose, color-supp)) + geom_point()
ggplot(ToothGrowth, aes(x=len, y=dose, color=supp)) + geom_point()
ggplot(ToothGrowth, aes(x=len, y=dose, color=supp)) + geom_point() + facet_wrap(. ~ supp)
ggplot(ToothGrowth, aes(x=len, y=dose, color=supp)) + geom_point() + facet_grid(. ~ supp)
ggplot(ToothGrowth, aes(x=len, y=dose, color=supp)) + geom_boxplot() + facet_grid(. ~ supp)
ggplot(ToothGrowth, aes(x=len, color=supp)) + geom_boxplot() + facet_grid(. ~ supp)
ggplot(ToothGrowth, aes(x=len, color=factor(supp)) + geom_boxplot()
)
ggplot(ToothGrowth, aes(x=len, color=factor(supp))) + geom_boxplot()
ggplot(ToothGrowth, aes(x=len, y=dose, color=factor(supp))) + geom_point()
ggplot(ToothGrowth, aes(x=len, y=dose, color=supp)) + geom_point() + facet_grid(. ~ supp)
ggplot(ToothGrowth, aes(x=len, y=dose, color=supp)) + geom_point()
ggplot(ToothGrowth, aes(x=len, color=factor(supp))) + geom_boxplot()
ggplot(ToothGrowth, aes(x=len, y=factor(dose), color=factor(supp))) + geom_boxplot()
ggplot(ToothGrowth, aes(x=len, y=factor(dose)) + geom_boxplot()
)
ggplot(ToothGrowth, aes(x=len, y=factor(dose))) + geom_boxplot()
ggplot(ToothGrowth, aes(x=factor(dose), y=len)) + geom_boxplot()
ggplot(ToothGrowth, aes(x=factor(dose), y=len)) + geom_boxplot() + facet_grid(. ~ supp)
ggplot(ToothGrowth, aes(x=factor(dose), y=len)) + geom_boxplot() + facet_grid(. ~ dose)
ggplot(ToothGrowth, aes(x=factor(supp), y=len)) + geom_boxplot() + facet_grid(. ~ dose)
?ToothGrowth
head(ToothGrowth)
library(dplyr)
tVC <- filter(ToothGrowth, supp == 'VC')
tOJ <- filter(ToothGrowth, supp == 'OJ')
tVC
t.test(tVC,tOJ)
t.test(tVC,tOJ, pairs=FALSE)
t.test(tVC,tOJ, paire, var.equal = TRUEd=FALSE)$conf
t.test(tVC,tOJ, paired = FALSE, var.equal = TRUE)$conf
t.test(tVC$len,tOJ$len, paired = FALSE, var.equal = TRUE)$conf
t.test(tVC$len,tOJ$len, paired = FALSE, var.equal = FALSE)$conf
t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = ToothGrowth)$conf
t.test(tOJ$len,tVC$len, paired = FALSE, var.equal = FALSE)$conf
t.test(tOJ$len,tVC$len, paired = FALSE, var.equal = FALSE)
?ToothGrowth
tVC
tOJ
library(dplyr)
tVC <- filter(ToothGrowth, supp == 'VC')
tOJ <- filter(ToothGrowth, supp == 'OJ')
t.test(tOJ$len,tVC$len, paired = FALSE, var.equal = FALSE)
t.test(tOJ$len,tVC$len, paired = FALSE, var.equal = FALSE)$conf
t.test(tOJ$len,tVC$len, paired = FALSE, var.equal = FALSE)$conf[[1]]
t.test(tOJ$len,tVC$len, paired = FALSE, var.equal = FALSE)$conf[[1:2]]
t.test(tOJ$len,tVC$len, paired = FALSE, var.equal = FALSE)$conf[1:2]
summary(ToothGrowth)
t.test(len ~ supp, paired = FALSE, var.equal = TRUE, data = ToothGrowth)
t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = ToothGrowth)
t.test(tOJ$len,tVC$len, paired = FALSE, var.equal = FALSE)
library(reshape2)
t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = ToothGrowth)
ggplot(ToothGrowth, aes(x=dose, y=len, color=supp)) + geom_point()
?subset
subToothGrowth <- subset(ToothGrowth, dose %in% c(0.5,1.0))
subToothGrowth
t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data = ToothGrowth)
t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data = subToothGrowth)
subToothGrowth <- subset(ToothGrowth, dose %in% c(0.5,2.0))
t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data = subToothGrowth)
subToothGrowth <- subset(ToothGrowth, dose %in% c(0.5,2.0))
t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data = subToothGrowth)
subToothGrowth <- subset(ToothGrowth, dose %in% c(1.0,2.0))
t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data = subToothGrowth)
?t.test
mn <- 1100
s<- 30
mn + c(-1,1) + qt(.95,8) * s/sqrt(9)
mn + c(-1,1) + qt(.975,8) * s/sqrt(9)
mn + c(-1,1) + qt(.975,8) * s
mn + c(-1,1) + qt(.975,8) * s^2
mn + c(-1,1) + qt(.975,8) * s
mn + c(-1,1) + qt(.95,8) * s
mn + c(-1,1) + qt(.975,8) * s/sqrt(8)
?qt
mn + c(-1,1) + qt(.975,9) * s/sqrt(8)
mn + c(-1,1) + qt(.975,9) * s/sqrt(9)
mn + c(-1,1) * qt(.975,9) * s/sqrt(9)
mn + c(-1,1) * qt(.975,8) * s/sqrt(9)
mn <- 1100
s <- 30
n <- 9
mn + c(-1,1) * qt(.975,n-1) * s/sqrt(n)
qt(.975,9-1)
sqrt(9)
2.306004/3
s <- 2/0.768668
s
wks <- 6
n <- 9
mn <- -2
mn * (-1) / (sqrt(n)/qt(.975,n-1))
mn * (-1) * (sqrt(n)/qt(.975,n-1))
n <- 10
mwt_new <- 3
var_new <- 0.6
mwt_old <- 5
var_old <- 0.68
?t.test
(mwt_new - mwt_old) + c(-1,1) + qt(0.975,n-1) * sqrt(((.6+.68)/2)/n)
sp <- sqrt((9*var_new + 9 * var_old)/ (10 + 10 -2))
sp
mwt_new - mwt_old + c(-1,1) * qt(.975,20) * sp * (1/10 + 1/10) ^ 0.5
n1 <- 100
n2 <- 100
mwt_new <- 4
var_new <- 0.5^2
mwt_old <- 5
var_old <- 2^2
#t.test(mwt_new - mwt_old, paired = FALSE, )
#(mwt_new - mwt_old) + c(-1,1) + qt(0.975,n-1) * sqrt(((.6+.68)/2)/n)
sp <- sqrt(((n1-1)*var_new + (n2-1) * var_old)/ (n1 + n2 -2))
mwt_new - mwt_old + c(-1,1) * qt(.975,n1+n2) * sp * (1/n1 + 1/n2) ^ 0.5
sp <- sqrt(((n1-1)*var_new + (n2-1) * var_old) / (n1 + n2 -2))
mwt_old - mwt_new + c(-1,1) * qt(.975,n1+n2) * sp * (1/n1 + 1/n2) ^ 0.5
n <- 18
# treatment group
n1 <- 9
# placebo group
n2 <- 9
avgdiff_1 <- -3
avgdiff_2 <- 1
var_1 <- 1.5^2
var_2 <- 1.8^2
sp <- sqrt(((n1-1)*var_1 + (n2-1) * var_2) / (n1 + n2 -2))
avgdiff_1 - avgdiff_2 + c(-1,1) * qt(.975,n1+n2) * sp * (1/n1 + 1/n2) ^ 0.5
avgdiff_1 - avgdiff_2 + c(-1,1) * qt(.95,n1+n2) * sp * (1/n1 + 1/n2) ^ 0.5
avgdiff_2 - avgdiff_1 + c(-1,1) * qt(.95,n1+n2) * sp * (1/n1 + 1/n2) ^ 0.5
avgdiff_2 - avgdiff_1 + c(-1,1) * qt(.90,n1+n2) * sp * (1/n1 + 1/n2) ^ 0.5
avgdiff_1 - avgdiff_2 + c(-1,1) * qt(.90,n1+n2) * sp * (1/n1 + 1/n2) ^ 0.5
avgdiff_1 - avgdiff_2 + c(-1,1) * qt(.95,n1+n2) * sp * (1/n1 + 1/n2) ^ 0.5
avgdiff_1 - avgdiff_2
n1 <- 9
# placebo group
n2 <- 9
avgdiff_1 <- -3
avgdiff_2 <- 1
var_1 <- 1.5^2
var_2 <- 1.8^2
sp <- sqrt(((n1-1)*var_1 + (n2-1) * var_2) / (n1 + n2 - 2))
avgdiff_1 - avgdiff_2 + c(-1,1) * qt(.95,n1+n2) * sp * (1/n1 + 1/n2) ^ 0.5
var_1
var_2
sp
avgdiff_1 - avgdiff_2 + c(-1,1) * qt(.95,n1+n2) * sp * (1/n1 + 1/n2)^0.5
library(swirl)
install_from_swirl("Statistical Inference")
swirl()
(1-.985)*(0.001)
(0.985)*(0.001)
(0.997)*(0.001)
(0.985)*(0.001)
(0.985)*(1-0.001)
(1-0.985)*(1-0.001)
(0.997)*(0.001)/ (1-0.985)*(1-0.001)
(.997*.001) by (.997*.001 + .015*.999)
(.997*.001) / (.997*.001 + .015*.999)
2.5
(1+2+3+4+5+6)/6
expert_dice
expect_dice
dice_high
dice_high/6
dice_high/length(dice_high)
expect_dice(dice_high)
expect_dice(dice_low)
(expect_dice(dice_low)+expect_dice(dice_high))*0.5
integrate(myfunc)
integrate(myfunc,0,2)
spop
mean(spop)
allsam
mean(allsam)
apply(allsam,1,mean)
smeans
mean(smeans)
?I
data(swiss)
par(mfrow = c(2,2))
fit <- lm(Fertility ~ ., data = swiss)
plot(fit)
?influence.measures
install.packages("AnomalyDetection")
install.packages("devtools")
devtools::install_github("petermeissner/wikipediatrend")
devtools::install_github("twitter/AnomalyDetection")
install.packages("Rcpp")
install.packages("Rcpp")
install_github("hadley/httr")
library(devtools)
install_github("hadley/httr")
install.packages("stringi")
install_github("hadley/httr")
install.packages("curl")
install_github("hadley/httr")
install.packages("Rcpp")
library(wikipediatrend) ## Library containing API wikipedia access
library(AnomalyDetection)
library(ggplot2)
devtools::install_github("petermeissner/wikipediatrend")
devtools::install_github("twitter/AnomalyDetection")
devtools::install_github("petermeissner/wikipediatrend")
library(wikipediatrend)
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
install.packages("AnomalyDetection")
devtools::install_github("twitter/AnomalyDetection")
sessionInfo()
x <- getBinaryURL(
url='https://github.com/ramnathv/slidify/archive/master.zip',
followlocation=1L
)
library(devtools)
x <- getBinaryURL(
url='https://github.com/ramnathv/slidify/archive/master.zip',
followlocation=1L
)
library(RCurl)
x <- getBinaryURL(
url='https://github.com/ramnathv/slidify/archive/master.zip',
followlocation=1L
)
rm(x)
devtools::install_github("twitter/AnomalyDetection")
install_github('ramnathv/slidify')
httr::set_config( httr::config( ssl_verifypeer = 0L ) )
devtools::install_github("twitter/AnomalyDetection")
remove.packages('curl')
q()
library("curl")
library(devtools)
devtools::install_github("twitter/AnomalyDetection")
library(RCurl)
devtools::install_github("twitter/AnomalyDetection")
install.packages("curl")
devtools::install_github("twitter/AnomalyDetection")
library(wikipediatrend) ## Library containing API wikipedia access
library(AnomalyDetection)
library(ggplot2)
library(ggplot2)
remove.packages('ggplot2')
q()
install.packages("ggplot2")
library(ggplot2)
library(wikipediatrend)
library(AnomalyDetection)
fifa_data = wp_trend("fifa", from="2013-03-18", lang = "en")
install.packages("Rbitcoin")
ggplot(fifa_data, aes(x=date, y=count, color=count)) + geom_line()
fifa_data$date = as.POSIXct(fifa_data$date)
fifa_data=fifa_data[,c(1,2)]
data_anomaly = AnomalyDetectionTs(fifa_data, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
data_anomaly$plot
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)
## Plot anomalies table
anomaly_table=data_anomaly$anoms
data_anomaly$anoms$perc_diff
anomaly_table
fifa_data = wp_trend("bitcoin", from="2010-01-01", lang = "en")
ggplot(fifa_data, aes(x=date, y=count, color=count)) + geom_line()
fifa_data$date = as.POSIXct(fifa_data$date)
fifa_data=fifa_data[,c(1,2)]
data_anomaly = AnomalyDetectionTs(fifa_data, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
data_anomaly$plot
data_anomaly$anoms$perc_diff=round(100*(data_anomaly$anoms$expected_value-data_anomaly$anoms$anoms)/data_anomaly$anoms$expected_value)
anomaly_table=data_anomaly$anoms
data_anomaly$anoms$perc_diff
anomaly_table
install.packages(
lib  = lib <- .libPaths()[1],
pkgs = as.data.frame(installed.packages(lib), stringsAsFactors=FALSE)$Package,
type = 'source'
)
install.packages(
lib  = lib <- .libPaths()[1],
pkgs = as.data.frame(installed.packages(lib), stringsAsFactors=FALSE)$Package,
type = 'source'
)
q()
rm(list = ls())
library(devtools)
install.packages("Rcpp")
install.packages("Rcpp")
install.packages("Rcpp")
install.packages("Rcpp")
head(InsectSprays)
iData <- InsectSprays
iData$spray <- factor(iData$spray)
str(iData)
log(15)
ln(15)
exp(log(15))
log(exp(15))
round(exp(coef(lm(I(log(iData$count + 1)) ~ idata$spra y))),5)
round(exp(coef(lm(I(log(iData$count + 1)) ~ idata$spray))),5)
round(exp(coef(lm(I(log(iData$count + 1)) ~ iData$spray))),5)
coef(lm(I(log(iData$count + 1)) ~ iData$spray))
exp(coef(lm(I(log(iData$count + 1)) ~ iData$spray)))
14.8312779/1.0616285
2.69673832/0.05980403
glm( formula = iData$count ~ iData$spray, family = "poisson")
summary(glm( formula = iData$count ~ iData$spray, family = "poisson"))
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
x
y
length(y)
length(x)
knots <- rep(0, 11)
knots
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1,x,splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
summary(yhat)
yhat
lm(y ~ xMat - 1)
summary(lm(y ~ xMat - 1))
xMat
plot(x,y,frame = FALSE, pch = 21, cex = 2)
lines(x, yhat, col = "red", lwd = 2)
glm( formula = iData$count ~ iData$spray, family = "poisson")
summary(glm( formula = iData$count ~ iData$spray, family = "poisson"))
summary(glm( formula = iData$count ~ iData$spray - 1, family = "poisson"))
2.73003 - 2.67415
exp(0.05588)
exp(2.73003) - exp(2.67415)
exp(2.73003 - 2.67415)
exp(2.73003)/exp(2.67415)
exp(2.67415)/exp(2.73003)
head(shuttle)
head(Shuttle)
library(MASS)
head(Shuttle)
head(shuttle)
str(shuttle)
glm(formula = shuttle$use ~ shuttle$wind)
glm(formula = shuttle$use ~ shuttle$wind,family = "binomial")
summary(glm(formula = shuttle$use ~ shuttle$wind,family = "binomial"))
logShuttle <- glm(formula = shuttle$use ~ shuttle$wind,family = "binomial")
exp(1-0.03181)/exp(-0.03181)
exp(-0.03181)/exp(1-0.03181)
exp(0.969)/(1+exp(0.969))
exp(logShuttle$coeff)
logShuttle$coeff
1-0.03181183
exp(confint(logShuttle$coeff))
exp(logShuttle$coeff)
load("E:/Documents/Coursera/Practical Machine Learning/Project/.RData")
pred_rf_80pct
?read.csv
train <- read.csv(file = url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"), header = TRUE)
install.packages("doMC")
install.packages("DoMC")
install.packages("domc")
install.packages("NaturalLanguageProcessing")
install.packages("tm")
install.packages("openNLP")
library(wordcloud)
install.packages("wordcloud")
blogs <- read.csv("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\en_US.blogs-usort.tst",header=FALSE)
blogs <- read.csv("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\en_US.blogs-usort.txt",header=FALSE)
head(blogs)
blogs <- read.csv("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\en_US.blogs-usort.txt",header=FALSE,sep="\t")
names(blogs)
head(blogs)
str(blogs)
blogs <- read.csv("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\en_US.blogs-usort.txt",header=FALSE,sep=" ")
str(blogs)
head(blogs)
blogs <- read.csv("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\en_US.blogs-usort-c.txt",header=FALSE,sep=" ")
head(blogs)
names(blogs) <- c("count","word")
library(ggplot2)
geom(head(blogs,40),aes(x=word,y=count)) + geom_histogram()
geom(head(blogs,40),aes(x=word,y=count)) + geom_histogram(stat="identity")
ggplot(head(blogs,40),aes(x=word,y=count)) + geom_histogram(stat="identity")
ggplot(head(blogs,40),aes(x=word,y=count)) + geom_histogram()
ggplot(head(blogs,40),aes(x=word)) + geom_histogram()
ggplot(head(blogs,40),aes(x=word,y=count)) + geom_bar()
ggplot(head(blogs,40),aes(x=word,y=count)) + geom_bar(stat=identity
)
b <- head(blogs,40)
barchart(b$word,b$count)
barplot(b$word,b$count)
ggplot(b,aes(x=factor(word),y=count)) + geom_bar()
ggplot(b,aes(x=factor(word),y="count")) + geom_bar()
names(b) <- c("word","f")
ggplot(b,aes(x=factor(word),y=f)) + geom_bar()
ggplot(b,aes(x=factor(word),y=f)) + geom_bar(stat="identity")
ggplot(b,aes(y=factor(word),x=f)) + geom_bar(stat="identity")
ggplot(b,aes(y=word,x=f)) + geom_bar(stat="identity")
b <- head(blogs,20)
ggplot(b,aes(y=word,x=f)) + geom_bar(stat="identity")
names(b) <- c("word","f")
ggplot(b,aes(y=word,x=f)) + geom_bar(stat="identity")
library(wordcloud)
wordcloud(b)
?wordcloud
wordcloud(b$word,b$f)
wordcloud(b$word,b$f,min.freq=1,max.words=10)
wordcloud(b$word,b$f,min.freq=1)
b
wordcloud(b$f,b$word,min.freq=1)
blogs <- read.csv("E:\\Documents\\Coursera\\DataScienceCapstoneProject\\en_US.blogs-usort-c.txt",header=FALSE,sep=" ")
names(blogs) <- c("freq","word")
wordcloud(blogs$freq,blogs$word,min.freq=100)
head(blogs)
wordcloud(blogs$freq,blogs$word,min.freq=1000)
wordcloud(blogs$word,blogs$freq,min.freq=1000)
wordcloud(blogs$freq,blogs$word,min.freq=100000)
wordcloud(blogs$freq,blogs$word,min.freq=800000)
wordcloud(blogs$word,blogs$freq,min.freq=800000)
wordcloud(blogs$word,blogs$freq,min.freq=700000)
wordcloud(blogs$word,blogs$freq,min.freq=100000)
wordcloud(blogs$word,blogs$freq,min.freq=10000)
b <- head(blogs,20)
ggplot(b,aes(y=freq,x=word)) + geom_bar(stat="identity")
savehistory("C:/Users/matt/Desktop/Rhistory.R")

