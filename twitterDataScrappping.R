#Install the required packages
#install.packages("twitteR")

#twitteR acts as an interface between R and Twitter and helps to scrap Twitter data
library("twitteR")
library("httr")
library("tm")
library("SnowballC")

#Create a Developer account in Twitter : https://apps.twitter.com
#Create a new App

#Get the key, secret, access token and access token secret from the Keys and Tokens tab
key = ""
secret = ""
accessToken = ""
accessTokenSecret = ""

#Also set your working directory using the setwd() function
#setwd("")

#Download the cacert.pem file and store it in your working directory
#Authenticate handshake. Get the PIN and just run it in R
#This comppletes the handshake between R and Twitter
setup_twitter_oauth(key,secret,accessToken,accessTokenSecret)

#Now R is connected to Twitter. Start mininig!
#Note that Twitter allows only 15 scrapes in 15 minutes
#The Twitter Search API searches against a sampling of recent Tweets published in the past 7 days.
#So only tweets in the last 7 days can be mined

#Get 1000 tweets containing '#DataScience'
tweets=searchTwitter("#DataScience", n=1000)

#Check the first 6 tweets
head(tweets)

#Get the text from these tweets using the sapply & getText function
mylist=sapply(tweets,function(x) x$getText())
#Converting the text to UTF-8
mylist=iconv(mylist, to= "utf-8", sub="")
#Assign the list to a Corpus
mycorpus = Corpus(VectorSource(mylist))

#Start the data cleaning process in the Corpus
#The transformations are done using the tm_map function

#Converting it to lower case
mycorpus <- tm_map(mycorpus,content_transformer(tolower))
#Removing punctuation
mycorpus <- tm_map(mycorpus, removePunctuation)
#Removing stop words in English
mycorpus <- tm_map(mycorpus,removeWords,stopwords("english"))
#Removing numbers
mycorpus <- tm_map(mycorpus, removeNumbers)

#Converting the corpus to a Document Term Matrix and Term Document Matrix
#The tweets are broken down into words here
dtm=DocumentTermMatrix(mycorpus)
tdm=TermDocumentMatrix(mycorpus)

#Converting dtm to a Matrix
dtmMatrix=as.matrix(dtm)

#Converting tdm to a Matrix
tdm2=as.matrix(tdm)

#Get the frequency of the words from the matrix
frequency=colSums(dtmMatrix)
#Sort the words in descending order of the frequency with which they occur 
frequency=sort(frequency,decreasing = TRUE)

#Creating Word Cloud:

#install the wordcloud and RColorBrewer packages
library("wordcloud")
library("RColorBrewer")

#Get the names of the words from the sorted frequency
words=names(frequency)
#Basic Word Cloud
#Output 1:
wordcloud(words[1:100], frequency[1:100])

#Give colors and order to the Word Cloud
col <- brewer.pal(5,"Dark2")
#Output 2:
wordcloud(words[1:100], frequency[1:100], scale = c(5,1), rot.per = 0, colors= col,random.color=F, max.word=90, random.order=F)

#Creating Dendrogram:
#finding the words which have a frequency of atleast 50
findFreqTerms(dtm, lowfreq=50)
#Finding the Associations of the word 'BigData' in the dtm matrix
findAssocs(dtm, 'BigData', 0.60)

#Remove sparse terms (infrequently used words) from the dtm matrix
dtm <-removeSparseTerms(dtm, sparse=0.9)
#Scaling the data
dtmscale <- scale(dtm)

#distance matrix
dtmdist <- dist(dtmscale, method = "euclidean")

#Hierarchical clustering
dtmfit <- hclust(dtmdist)

#Visualize the result
#This is a dendrogram of the frequencies and not the words
plot(dtmfit)

#Creating a dendrogram for the words
#Removing sparse terms
tdm <-removeSparseTerms(tdm, sparse=0.9)
tdmscale <- scale(tdm)
#Distance matrix
tdmdist <- dist(tdmscale, method = "euclidean")

#Hierarchical clustering
tdmfit <- hclust(tdmdist)

#Visualize the result
plot(tdmfit)
              
#Cutting the dendrogram into 6 groups
cutree(tdmfit, k=6)

#Giving a red colored border for the 6 groups
rect.hclust(tdmfit, k=6, border="red")

#Try out the different types of Dendrograms:
#Install the 'dendextend' package
install.packages("dendextend")
library(dendextend)

#converting the tdmfit to a dendrogram
mydend <- as.dendrogram(tdmfit)
plot(mydend)
              
#Coloring the labels
labels_colors(mydend) <- 1:3
labels_colors(mydend)
plot(mydend)

#Circlize the dendrogram
install.packages("circlize")
library(circlize)
mydend <- mydend %>% color_branches(k=4) %>% color_labels

#Radial Plot
par(mar = rep(0,4))
circlize_dendrogram(mydend, labels_track_height = NA, dend_track_height = .3) 

mydend <- mydend %>% color_branches(k=3) %>% color_labels
#Horizontal Version
par(mar = c(3,1,1,7))
plot(mydend, horiz  = TRUE)

#Horizontal Mirror version
par(mar = c(3,7,1,1))
plot_horiz.dendrogram(mydend, side = TRUE)

#Triangular
plot(mydend, type = "triangle")
#Rectangular
plot(mydend, type = "rectangle")

#Install the 'ape' package. It allows the reading, writing, manipulating and analysing of phylogenetic trees
#install.packages("ape")
library("ape")

#Different versions:
plot(as.phylo(mydend), type = "unrooted")
plot(as.phylo(mydend), type = "fan")
plot(as.phylo(mydend), type = "radial")

plot(as.phylo(mydend), type = "fan", tip.color = hsv(runif(15, 0.65, 0.95), 1, 1, 0.7), 
edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), edge.width = runif(20,0.5, 3), use.edge.length = TRUE, col = "gray80")

mypal = rainbow(1:5)
#Cutting the dendrogram into 5 clusters
clus5 = cutree(mydend, 5)
#Plotting a fan type dendrogram with a background color
op = par(bg = "#E8DDCB")
plot(as.phylo(mydend), type = "fan", tip.color = mypal[clus5], label.offset = 1,cex = log(mtcars$mpg, 10), col = "red")
