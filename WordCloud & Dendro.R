library("twitteR")
library("httr")
library("tm")
library("SnowballC")

key = "5ECBtAnsp3kErElYRzViOLNDw"

secret = "zYGwPa14vDAOcnOR1cCg3mOnjZFkFeyLmkxlzTWnzNn7wLdOva"

accessToken = "2179932235-Deu3ugJVB9eyrwR52JMAAsAETvsCcJ7yknovCZj"

accessTokenSecret = "ukhUYD2YtImCFpbSprzBMt1wwExbvQUm5qsizOO7SHTAn"

setup_twitter_oauth(key,secret,accessToken,accessTokenSecret)
tweets=searchTwitter("#DataScience", n=1000)
head(tweets)

mylist=sapply(tweets,function(x) x$getText())

mylist=iconv(mylist, to= "utf-8", sub="")

mycorpus = Corpus(VectorSource(mylist))

mycorpus <- tm_map(mycorpus,content_transformer(tolower))
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus,removeWords,stopwords("english"))
mycorpus <- tm_map(mycorpus, removeNumbers)

dtm=DocumentTermMatrix(mycorpus)
tdm=TermDocumentMatrix(mycorpus)

dtmMatrix=as.matrix(dtm)
dtmMatrix

tdm2=as.matrix(tdm)
tdm2

frequency=colSums(dtmMatrix)
frequency=sort(frequency,decreasing = TRUE)

library("wordcloud")
library("RColorBrewer")

words=names(frequency)
wordcloud(words[1:100], frequency[1:100])

col <- brewer.pal(5,"Dark2")
wordcloud(words[1:100], frequency[1:100], scale = c(5,1), rot.per = 0, colors= col,random.color=F, max.word=90, random.order=F)


# WordCloud over ---------------------------------

# Dendrogram -------------------------------------

findFreqTerms(dtm, lowfreq=50)
findAssocs(dtm, 'virat', 0.60)

dtm <-removeSparseTerms(dtm, sparse=0.9)
dtmscale <- scale(dtm)

# distance matrix
dtmdist <- dist(dtmscale, method = "euclidean")
#dtmdist <- dist(which(dtmscale>-0.1 & dtmscale<0.2), method = "euclidean")
# hierarchical clustering

dtmfit <- hclust(dtmdist)

# Visualize the result
plot(dtmfit)
#-------------------------------------------

tdm <-removeSparseTerms(tdm, sparse=0.9)
tdmscale <- scale(tdm)
# distance matrix
tdmdist <- dist(tdmscale, method = "euclidean")
#dtmdist <- dist(which(dtmscale>-0.1 & dtmscale<0.2), method = "euclidean")
# hierarchical clustering
tdmfit <- hclust(tdmdist)

# Visualize the result
plot(tdmfit)
# to calculate a certain number of groups
cutree(tdmfit, k=6)

# we can even color the 6 groups and plot them
rect.hclust(tdmfit, k=6, border="red")

install.packages("dendextend")
library(dendextend)

mydend <- as.dendrogram(tdmfit)
plot(mydend)

labels_colors(mydend) <- 1:3
labels_colors(mydend)
plot(mydend)

#Method 2:
colors_to_use <- as.numeric(mydend[,14])
colors_to_use <- colors_to_use[order.dendrogram(mydend)]
labels_colors(mydend) <- order.dendrogram(mydend)
labels_colors(mydend)

plot(mydend)
#-----------------------------
#Circlize:
install.packages("circlize")
library(circlize)
mydend <- mydend %>% 
  color_branches(k=4) %>% 
  color_labels

# plot the radial plot
par(mar = rep(0,4))
# circlize_dendrogram(dend, dend_track_height = 0.8) 
circlize_dendrogram(mydend, labels_track_height = NA, dend_track_height = .3) 

#-------------------------

mydend <- mydend %>% color_branches(k=3) %>% color_labels

# horiz normal version
par(mar = c(3,1,1,7))
plot(mydend, horiz  = TRUE)

# horiz mirror version
par(mar = c(3,7,1,1))
plot_horiz.dendrogram(mydend, side = TRUE)

plot(mydend, type = "triangle")

plot(mydend, type = "rectangle")

install.packages("ape")

library("ape")
plot(as.phylo(mydend), type = "unrooted")
plot(as.phylo(mydend), type = "fan")
plot(as.phylo(mydend), type = "radial")

plot(as.phylo(mydend), type = "fan", tip.color = hsv(runif(15, 0.65, 
0.95), 1, 1, 0.7), edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), edge.width = runif(20,0.5, 3), 
use.edge.length = TRUE, col = "gray80")

mypal = rainbow(1:5)
# cutting dendrogram in 5 clusters
clus5 = cutree(mydend, 5)
# plot
op = par(bg = "#E8DDCB")
# Size reflects miles per gallon
plot(as.phylo(mydend), type = "fan", tip.color = mypal[clus5], label.offset = 1, 
     cex = log(mtcars$mpg, 10), col = "red")
#------------------------------

#rainbow(7)
#heatmap(matrix((1:6), nrow =2, ncol =3))
mat <- dtm2[3:15, 3:15]
heatmap(mat)
dtm2
# to calculate a certain number of groups
cutree(dtmfit, k=2)

# we can even color the 6 groups and plot them
rect.hclust(dtmfit, k=2, border="red")
