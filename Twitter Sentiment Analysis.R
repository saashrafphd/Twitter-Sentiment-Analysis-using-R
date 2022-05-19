#  Install Requried Packages
install.packages("SnowballC")
install.packages("tm")
install.packages("twitteR")
install.packages("syuzhet")
install.packages("wordcloud")
install.packages("RColorBrewer")


# Load Requried Packages
library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")
library("wordcloud")
library("RColorBrewer")
library("twitteR")
library("igraph")
library("ggplot2")
# Authentication keys
api_key <- ""
api_secret <- ""
token <- ""
token_secret <-""

setup_twitter_oauth(api_key, api_secret, token, token_secret)
tweets <- searchTwitter("Dabangg3", n=2000, lang="en", since="2019-09-12")

n.tweet <- length(tweets)
tweets.df <- twListToDF(tweets) 

head(tweets.df)
head(tweets.df$text)

# WORD CLOUD

docs <- Corpus(VectorSource(tweets.df$text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# Specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("hrefhttp","tco","download","twitter","follow","https")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
print(dtm)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# SENTIMENT ANALYSIS

tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
head(tweets.df2)

word.df <- as.vector(tweets.df2)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(tweets.df2, emotion.df) 

head(emotion.df2)

sent.value <- get_sentiment(word.df)
head(sent.value)
most.positive <- word.df[sent.value == max(sent.value)]
most.positive

most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 

sent.value
print(sent.value)
print(word.df)
summary(word.df)

positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)
p1=length(positive.tweets)

negative.tweets <- word.df[sent.value < 0]
head(negative.tweets)
p2=length(negative.tweets)

neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)
p3=length(neutral.tweets)
print("possitive")
print(p1)
print("Negative")
print(p2)
print("Neutral")
print(p3)
#Table creation
H <- c(p1,p2,p3)
M <- c("Possitive","Negative","Neutral")

# Give the chart file a name
png(file = "C:/Users/Lenovo/Music/barchart_sentiment.png")

# Plot the bar chart 
barplot(H,names.arg=M,xlab="Sentiment",ylab="Count",col="blue",
        main="Sentiment Count Chart",border="red")

# Save the file
dev.off()


ew_sentiment<-get_nrc_sentiment((tweets.df2))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()