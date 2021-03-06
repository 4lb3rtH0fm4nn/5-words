#input
args = commandArgs(trailingOnly=TRUE)

urldata="/home/webadmin/R/dtm-test/data/"
urlout="/home/webadmin/R/dtm-test/data/"
library(tm)
library(party)
library(SnowballC)
library(wordcloud)
library(cluster)
library(plyr)
eventdata<-read.csv(paste0(urldata,"data.csv"), header=T,sep=",")
eventwords<-eventdata[,5:9]
userwordsvector<-paste(eventwords[,1],eventwords[,2],eventwords[,3],eventwords[,4],eventwords[,5])
wmatrix<-as.matrix(userwordsvector)
rownames(wmatrix)<-c(eventdata[,1])
wordscorpus<-Corpus(VectorSource(wmatrix))
wordscorpus<- tm_map(wordscorpus, content_transformer(tolower))
wordscorpus<- tm_map(wordscorpus, removePunctuation)
wordscorpus <- tm_map(wordscorpus, stripWhitespace)
dtm<-DocumentTermMatrix(wordscorpus)
dtm_m<-as.matrix(dtm)
tdm<-TermDocumentMatrix(wordscorpus)
tdm_m<-as.matrix(tdm)
colnames(tdm) <- c(eventdata[,1])
rownames(dtm)<- c(eventdata[,1])
sumv<-as.matrix(sort(rowSums(tdm_m),decreasing=TRUE))
res <- data.frame(term=rownames(as.matrix(sumv)),frequency=rowSums(as.matrix(sumv))) 
row.names(res)<-NULL
write.table(sumv,paste0(urlout,"cwsum"))
write.table(res,paste0(urlout,"wc2sum"))
write.table(tdm_m,paste0(urlout,"tdm"))
write.table(dtm_m,paste0(urlout,"dtm"))
d <- dist(dtm_m)
groups <- hclust(d,method="ward.D")


#wordcloud2
mindex<-which(eventdata[,3]==0,arr.ind=T)
findex<-which(eventdata[,3]==1,arr.ind=T)
tindex<-which(eventdata[,3]==2,arr.ind=T)
mdata<-eventdata[mindex,]
fdata<-eventdata[findex,]
tdata<-eventdata[tindex,]
mwords<-mdata[,5:9]
fwords<-fdata[,5:9]
twords<-tdata[,5:9]
mwordsvector<-paste(mwords[,1],mwords[,2],mwords[,3],mwords[,4],mwords[,5])
fwordsvector<-paste(fwords[,1],fwords[,2],fwords[,3],fwords[,4],fwords[,5])
twordsvector<-paste(twords[,1],twords[,2],twords[,3],twords[,4],twords[,5])
mm<-as.matrix(mwordsvector)
fm<-as.matrix(fwordsvector)
tm<-as.matrix(twordsvector)
rownames(mm)<-c(mdata[,1])
rownames(fm)<-c(fdata[,1])
rownames(tm)<-c(tdata[,1])
mcorp<-Corpus(VectorSource(mm))
fcorp<-Corpus(VectorSource(fm))
tcorp<-Corpus(VectorSource(tm))
mcorp<- tm_map(mcorp, content_transformer(tolower))
fcorp<- tm_map(fcorp, content_transformer(tolower))
tcorp<- tm_map(tcorp, content_transformer(tolower))
mcorp<- tm_map(mcorp, removePunctuation)
fcorp<- tm_map(fcorp, removePunctuation)
tcorp<- tm_map(tcorp, removePunctuation)
mcorp<- tm_map(mcorp, stripWhitespace)
fcorp<- tm_map(fcorp, stripWhitespace)
tcorp<- tm_map(tcorp, stripWhitespace)
mtdm<-TermDocumentMatrix(mcorp)
mtdm_m<-as.matrix(mtdm)
colnames(mtdm) <- c(mdata[,1])
msumv<-as.matrix(sort(rowSums(mtdm_m),decreasing=TRUE))

ftdm<-TermDocumentMatrix(fcorp)
ftdm_m<-as.matrix(ftdm)
colnames(ftdm) <- c(fdata[,1])
fsumv<-as.matrix(sort(rowSums(ftdm_m),decreasing=TRUE))

ttdm<-TermDocumentMatrix(tcorp)
ttdm_m<-as.matrix(ttdm)
colnames(ttdm) <- c(tdata[,1])
tsumv<-as.matrix(sort(rowSums(ttdm_m),decreasing=TRUE))
tmsumv<-t(msumv)
tfsumv<-t(fsumv)
ttsumv<-t(tsumv)

gsum<-rbind.fill.matrix(tmsumv,tfsumv,ttsumv)
gsum<-t(gsum)
gsum[is.na(gsum)] <- 0
write.table(gsum,paste0(urlout,"gsum"))
#number of genders
numberofm<-dim(mdata)
numberofm<-numberofm[1]
numberoff<-dim(fdata)
numberoff<-numberoff[1]
numberoft<-dim(tdata)
numberoft<-numberoft[1]
genderv<-c(numberofm,numberoff,numberoft)
write.table(genderv,paste0(urlout,"genderv"))
#age
ages<-eventdata[,2]

rmarkdown::render("index.rmd")
