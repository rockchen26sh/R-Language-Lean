library(tm)
library(ggplot2)

#设置文件路径
spam.path<-"E:/ML_for_Hackers-master/03-Classification/data/spam/"
spam2.path<-"E:/ML_for_Hackers-master/03-Classification/data/spam_2/"
easyham.path<-"E:/ML_for_Hackers-master/03-Classification/data/easy_ham/"
easyham2.path<-"E:/ML_for_Hackers-master/03-Classification/data/easy_ham_2/"
hardham.path<-"E:/ML_for_Hackers-master/03-Classification/data/hard_ham/"
hardham2.path<-"E:/ML_for_Hackers-master/03-Classification/data/hard_ham_2/"

#批量打开文件找到空行函数
get.msg<- function(path){
  con<- file(path,open = "rt", encoding = "latin1") #rt：read as text以文本形式读取
  text<- readLines(con)
  #文件存到con，打开文件到text
  msg<- text[seq(which(text=="")[1]+1,length(text),1)]
  #提取内容到msg
  close(con) #关闭文件
  return(paste(msg,collapse="\n"))#粘贴内容并已回车分格
}

spam.docs<- dir(spam.path) #DIR函数得到data/spam下所有文件名列表
spam.docs<- spam.docs[which(spam.docs!="cmds")] #剔除cmds文件


all.spam<-sapply(spam.docs,
                 function(p) get.msg(file.path(spam.path, p)))

#sapply应用：sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
#X:参数 FUN：函数

#构造文档矩阵
get.tdm<-function(doc.vec){
  doc.corpus<- Corpus(VectorSource(doc.vec))
  control<- list(stopwords=TRUE,removePunctuation=TRUE,removeNumbers=TRUE,minDocFreq=2)
  doc.dtm<-TermDocumentMatrix(doc.corpus,control)
  return(doc.dtm)
}
spam.tdm<-get.tdm(all.spam)

spam.matrix<- as.matrix(spam.tdm)
spam.counts<- rowSums(spam.matrix)
spam.df<- data.frame(cbind(names(spam.counts),as.numeric(spam.counts)),stringAsFactors=FALSE)
names(spam.df)<- c("term","frequency")
spam.df$frequency<- as.numeric(spam.df$frequency)
spam.occurrence<- sapply(1:nrow(spam.matrix), function(i) {
  which(spam.matrix[i,]>0)) /ncol(spam.matrix)
})


