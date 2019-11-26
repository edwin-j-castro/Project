#Load JSON into R.
#Takes about 15 minutes to load

library(jsonlite)
data <- stream_in(file("C:\\Users\\edwin\\Documents\\review.json"), pagesize = 10000)
data1<-data[1:500000,4:9]
data2<-data[500001:1000000,4:9]
data3<-data[1000001:1500000,4:9]
data4<-data[1500001:2000000,4:9]
data5<-data[2000001:2500000,4:9]
data6<-data[2500001:3000000,4:9]
data7<-data[3000001:3500000,4:9]
data8<-data[3500001:4000000,4:9]
data9<-data[4000001:4500000,4:9]
data10<-data[4500001:5000000,4:9]

write.csv(data1, file = "C:\\Users\\edwin\\Documents\\data1.csv")
write.csv(data2, file = "C:\\Users\\edwin\\Documents\\data2.csv")
write.csv(data3, file = "C:\\Users\\edwin\\Documents\\data3.csv")
write.csv(data4, file = "C:\\Users\\edwin\\Documents\\data4.csv")
write.csv(data5, file = "C:\\Users\\edwin\\Documents\\data5.csv")

#have observations in excel file with basic columns

install.packages("tidyr") #install needed libraries

install.packages("tidytext") #need for sentiment analysis
library("tidytext")

install.packages("textdata") #also needed for NRC sentiment analysis
library(textdata)

library(dplyr) #data manipulation

library(ggplot2) #visualizations

install.packages("qdap", INSTALL_opts = "--no-multiarch")
library(qdap) #for word count

install.packages("tm")
library("tm")

install.packages('sentimentr')
library('sentimentr') #takes into account negators and amplifiers, and more
#https://towardsdatascience.com/sentiment-analysis-in-r-good-vs-not-good-handling-negations-2404ec9ff2ae

install.packages("randomForest")
library("randomForest")

require(caTools)

sentiment_score = NULL
yelp_review = NULL

yelp_data <- read.csv("C:\\Users\\edwin\\OneDrive\\Documents\\Ryerson University\\Big Data Analytics\\CMKE 136 - Data Analytics Capstone\\Yelp Data Set\\data1.csv")
names(yelp_data)

yelp_data<- as.data.frame(yelp_data)

typeof(yelp_data$useful)

dim(yelp_data)

yelp_review<-subset(yelp_data,yelp_data$useful>0)
#checking if usefull category is above stated amount
yelp_review$useful

# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# fix (expand) contractions
yelp_review$text_clean <- sapply(yelp_review$text, fix.contractions)

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

# remove special characters and punctuation. DO NOT USE THIS CODE. NEEDS PERIODS FOR SENTENCE
#yelp_review$text_clean <- sapply(yelp_review$text_clean, removeSpecialChars)

# convert everything to lower case
yelp_review$text_clean <- sapply(yelp_review$text_clean, tolower)

# Checking for frequent terms, will use 20 top terms as it doesn't include negators and makes up a large portion of common words
freqterms20<-freq_terms(yelp_review$text, 20)
freqterms50<-freq_terms(yelp_review$text, 50)
freqterms100<-freq_terms(yelp_review$text, 100)
freqterms1000<-freq_terms(yelp_review$text, 1000)

#To study what are the most frequent terms and what should be removed
#this needs to be revisited with the effects of the word "not" and other words that 
#plausibly shouln't be removed
write.csv(freqterms1000, file = "C:\\Users\\edwin\\Documents\\FREQUENT_TERMS_1000.csv")

#Frequently used words we do not want into a character vector
yelp_review$textfiltered <-removeWords(yelp_review$text_clean, freqterms20$WORD)

#OR you can use the package. So if yeilds different results
#stopwordsPACK <- stopwords("en")

#removing common nuetral words. Not using since it includes negators in the stop words
# yelp_review$textfiltered <-removeWords(yelp_review$text_clean, stopwordsPACK)

#remove numbers, Check if you need to do this
#yelp_review$textfiltered <-removeNumbers(yelp_review$textfiltered)

#remove white spaces
yelp_review$textfiltered <-stripWhitespace(yelp_review$textfiltered)

#This will give you the terms, please check as a sample
extract_sentiment_terms(yelp_review$textfiltered[1])

#Testing for negators
text1 <- "I am not good. I am not happy"
sentiment(text1)

#Score without negators
text2 <- "I am good. I am happy"
sentiment(text2)

#creating dataframe with the sentiment scores, std dev, and word count
sentiment_score <- as.data.frame(sentiment_by(yelp_review$textfiltered))

#transferring sentiment data to review data frame
yelp_review$wordcount <- sentiment_score$word_count
yelp_review$sd <- sentiment_score$sd
yelp_review$sentiment_score <- sentiment_score$ave_sentiment

#Check General Distribution of sentiment score per star rating
plot(yelp_review$sentiment_score,yelp_review$stars)

#Determining Mean Sentiment score of each star rating
mean(subset(yelp_review$sentiment_score, yelp_review$stars == 5))
mean(subset(yelp_review$sentiment_score, yelp_review$stars == 4))
mean(subset(yelp_review$sentiment_score, yelp_review$stars == 3))
mean(subset(yelp_review$sentiment_score, yelp_review$stars == 2))
mean(subset(yelp_review$sentiment_score, yelp_review$stars == 1))

one <- as.data.frame(subset(yelp_review$sentiment_score, yelp_review$stars == 1))
one <- as.data.frame(one[1:1000,])

two <- as.data.frame(subset(yelp_review$sentiment_score, yelp_review$stars == 2))
two <- as.data.frame(two[1:1000,])

three <- as.data.frame(subset(yelp_review$sentiment_score, yelp_review$stars == 3))
three <- as.data.frame(three[1:1000,])

four <- as.data.frame(subset(yelp_review$sentiment_score, yelp_review$stars == 4))
four <- as.data.frame(four[1:1000,])

five <- as.data.frame(subset(yelp_review$sentiment_score, yelp_review$stars == 5))
five <- as.data.frame(five[1:1000,])

yelp_star_sentiment <- data.frame(one, two, three, four, five)
names(yelp_star_sentiment) <- c('onestar','twostar','threestar','fourstar','fivestar')


wilcox.test(yelp_star_sentiment$onestar, yelp_star_sentiment$twostar, data = yelp_star_sentiment, paired = FALSE)
wilcox.test(yelp_star_sentiment$twostar, yelp_star_sentiment$threestar, data = yelp_star_sentiment, paired = FALSE)
wilcox.test(yelp_star_sentiment$threestar, yelp_star_sentiment$fourstar, data = yelp_star_sentiment, paired = FALSE)
wilcox.test(yelp_star_sentiment$fourstar, yelp_star_sentiment$fivestar, data = yelp_star_sentiment, paired = FALSE)

#Boxplot for each star rating
boxplot(yelp_review$sentiment_score ~ yelp_review$stars,main="Sentiment Score Boxplot",ylab="Sentiment Score", xlab="Star Rating")

#Check for negative scores on 5-star reviews
Negfive<-subset(yelp_review,yelp_review$stars==5)
Negfive<-subset(Negfive,Negfive$sentiment_score<0)
View(Negfive)

#Determine if data is significantly different  from each other.


#creata a dataframe to input it into the machine learning algorithm
yelp_ml_data <- yelp_review[1:200000,]
yelp_ml_data$X <- NULL
yelp_ml_data$text <- NULL
yelp_ml_data$text_clean <- NULL
yelp_ml_data$textfiltered <- NULL
yelp_ml_data$date <- NULL
yelp_ml_data$stars <- as.factor(yelp_ml_data$stars)

#Splitting for training and test sets
sample = sample.split(yelp_ml_data$stars, SplitRatio = .75)

train = subset(yelp_ml_data, sample == TRUE)
test = subset(yelp_ml_data, sample == FALSE)

dim(train)
dim(test)

#Perform Random Forest Algorithm
rf <- randomForest(stars ~ ., data = train, na.action = na.exclude)
pred = predict(rf, newdata=test)
cm = table(test, pred)
