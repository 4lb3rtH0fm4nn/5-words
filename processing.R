#input
args = commandArgs(trailingOnly=TRUE)
input="G:/Program Files/R/data/data2.csv"
library(tm)
library(party)
library(SnowballC)
library(wordcloud)
library(cluster)
eventdata<-read.csv("G:/Program Files/R/data/data2.csv", header=T,sep=";")
eventwords<-eventdata[,4:8]
userwordsvector<-paste(eventwords[,1],eventwords[,2],eventwords[,3],eventwords[,4],eventwords[,5])
wmatrix<-as.matrix(userwordsvector)
rownames(wmatrix)<-c(eventdata[,1])
wordscorpus<-Corpus(VectorSource(wmatrix))
wordscorpus<- tm_map(wordscorpus, content_transformer(tolower))
wordscorpus<- tm_map(wordscorpus, removePunctuation)
wordscorpus <- tm_map(wordscorpus, stripWhitespace)
wordscorpus <- tm_map(wordscorpus, stemDocument)
dtm<-DocumentTermMatrix(wordscorpus)
dtm_m<-as.matrix(dtm)
tdm<-TermDocumentMatrix(wordscorpus)
tdm_m<-as.matrix(tdm)
colnames(tdm) <- c(eventdata[,1])
rownames(dtm)<- c(eventdata[,1])
sumv<-as.matrix(sort(rowSums(tdm_m),decreasing=TRUE))
res <- data.frame(term=rownames(as.matrix(sumv)),frequency=rowSums(as.matrix(sumv))) 
row.names(res)<-NULL
write.table(sumv,"g:/webpagefinal/cwsum")
write.table(tdm_m,"g:/webpagefinal/dtm")
d <- dist(dtm_m)
groups <- hclust(d,method="ward.D")
plot(groups, hang=-1)
rmarkdown::render_site()







