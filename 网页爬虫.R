smzdmhyk<- function(x){
  fx<- as.data.frame(NULL)
  for ( i in 1 : x) {
    web<-readLines(paste("http://faxian.smzdm.com/h0s0t0f0c0p",i,sep=""),encoding = 'UTF-8')
    timesort<- web[grep("<li timesort=",web)]
    timesort<- substr(timesort,regexpr("timesort=",timesort)+10,regexpr("articleid",timesort)-4)
    href<- web[grep("feed-ver-title",web)]
    href<- substr(href,regexpr("href=",href)+6,nchar(href))
    href1<- substr(href,0,regexpr("\">",href)-1)
    title<- substr(href,regexpr("\">",href)+1,regexpr("/a",href)-2)
    price<- web[grep("<div class=\"z-highlight z-ellipsis\">",web)]
    price<- substr(price,regexpr(">",price)+1,regexpr("</div>",price)-1)
    smzdmtemp<- cbind(title,price,href1,timesort)
    fx<- rbind(fx,smzdmtemp)
  }
 fx<- fx[fx$timesort!="{{ timesort }",]
 return(fx) 
}

smzdmfx<- as.data.frame(NULL)
smzdmfx<- smzdmhyk(5)


smzdmfx<- smzdmfx[,c(-1,-2,-3,-4)]
web


web<-readLines("http://faxian.smzdm.com/h0s0t0f0c0p2",encoding = 'UTF-8')
timesort<- web[grep("<li timesort=",web)]
timesort<- substr(timesort,regexpr("timesort=",timesort)+10,regexpr("articleid",timesort)-4)
href<- web[grep("feed-ver-title",web)]
href<- substr(href,regexpr("href=",href)+6,nchar(href))
href1<- substr(href,0,regexpr("\">",href)-1)
title<- substr(href,regexpr("\">",href)+1,regexpr("/a",href)-2)
price<- web[grep("<div class=\"z-highlight z-ellipsis\">",web)]
price<- substr(price,regexpr(">",price)+1,regexpr("</div>",price)-1)
info<- web[grep("z-btn z-btn-red",web)]
go<- substr(info,regexpr("href=",info)+6,regexpr("target",info)-4)
price1<- substr(info,regexpr("price",info)+8,regexpr("brand",info)-4)
brand<- substr(info,regexpr("brand",info)+8,regexpr("mall",info)-5)
mall<- substr(info,regexpr("mall",info)+7,regexpr("category",info)-5)
category<- substr(info,regexpr("category",info)+11,regexpr("metric1",info)-4)
category<- substr(category,0,4)
time<- web[grep("feed-ver-row-r feed-ver-date",web)]
time<- substr(time,regexpr("feed-ver-date",time)+15,regexpr("</div>",time)-1)
smzdmtemp<- cbind(category,brand,title,price,time,go,href1)


smzdmhyk<- function(x){
  fx<- as.data.frame(NULL)
  for ( i in 1 : x) {
    web<-readLines(paste("http://faxian.smzdm.com/h1s0t0f163c0p",i,sep=""),encoding = 'UTF-8')
    timesort<- web[grep("<li timesort=",web)]
    timesort<- substr(timesort,regexpr("timesort=",timesort)+10,regexpr("articleid",timesort)-4)
    href<- web[grep("feed-ver-title",web)]
    href<- substr(href,regexpr("href=",href)+6,nchar(href))
    href1<- substr(href,0,regexpr("\">",href)-1)
    title<- substr(href,regexpr("\">",href)+1,regexpr("/a",href)-2)
    price<- web[grep("<div class=\"z-highlight z-ellipsis\">",web)]
    price<- substr(price,regexpr(">",price)+1,regexpr("</div>",price)-1)
    info<- web[grep("z-btn z-btn-red",web)]
    go<- substr(info,regexpr("href=",info)+6,regexpr("target",info)-4)
    price1<- substr(info,regexpr("price",info)+8,regexpr("brand",info)-4)
    brand<- substr(info,regexpr("brand",info)+8,regexpr("mall",info)-5)
    mall<- substr(info,regexpr("mall",info)+7,regexpr("category",info)-5)
    category<- substr(info,regexpr("category",info)+11,regexpr("metric1",info)-4)
    category<- substr(category,0,4)
    time<- web[grep("feed-ver-row-r feed-ver-date",web)]
    time<- substr(time,regexpr("feed-ver-date",time)+15,regexpr("</div>",time)-1)
    smzdmtemp<- cbind(category,brand,title,price,time,go,href1)
    fx<- rbind(fx,smzdmtemp)
  }
  fx<- fx[fx$price!="{{article_price}}",]
  return(fx) 
}




feedtitle
href<- substr(feedtitle,regexpr("href",feedtitle)+6,regexpr("target",feedtitle)-3)
names<- substr(feedtitle,regexpr("'name'",feedtitle)+8,regexpr("'id'",feedtitle)-3)
price<- substr(feedtitle,regexpr("'price'",feedtitle)+9,regexpr("'brand'",feedtitle)-3)
mall<- substr(feedtitle,regexpr("'mall'",feedtitle)+8,regexpr("'category'",feedtitle)-4)
brand<- substr(feedtitle,regexpr("'brand'",feedtitle)+9,regexpr("'mall'",feedtitle)-4)
category<- substr(feedtitle,regexpr("'category'",feedtitle)+12,regexpr("'metric1'",feedtitle)-3)
dimension11<- substr(feedtitle,regexpr("'dimension11'",feedtitle)+15,regexpr("'dimension12'",feedtitle)-3)
smzdmfx<- cbind(brand,names,category,mall,price,dimension11,href)

library(RCurl)
library(XML)
library(reshape)
myheader=c(
  
  "User-Agent"="Mozilla/5.0(Windows;U;Windows NT 5.1;zh-CN;rv:1.9.1.6",
  
  "Accept"="text/htmal,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  
  "Accept-Language"="en-us",
  
  "Connection"="keep-alive",
  
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
  
)
url<-"http://www.chehang168.com/index.php?c=index&m=brand&pbid=1"
url.exists(url)

library(RCurl)

#Set your browsing links 
loginurl = "http://www.chehang168.com/index.php?c=login&m=index"
dataurl  = "http://www.chehang168.com/index.php?c=index&m=brand&pbid=1"

#Set user account data and agent
pars=list(
  username="18521505736",
  password="cheku602"
)
agent="Mozilla/5.0" #or whatever 

#Set RCurl pars
curl = getCurlHandle()
curlSetOpt(cookiejar="c:/cookiefile.txt",  useragent = agent, followlocation = TRUE, curl=curl)
#Also if you do not need to read the cookies. 
#curlSetOpt(  cookiejar="", useragent = agent, followlocation = TRUE, curl=curl)

#Post login form
html=postForm(loginurl, username='18521505736', password='cheku602', curl=curl)

#Go wherever you want
html=getURL(dataurl, curl=curl)

#Start parsing your page
matchref=gregexpr("... my regexp ...", html)

#... .... ...

#Clean up. This will also print the cookie file
rm(curl)
gc()

html


doc<- getURL(url,encoding="GBK")
doc
temp <- strsplit(doc,"rn")[[1]]

txt<-htmlParse(doc,asText = TRUE)
txt

txt1<- iconv(txt,"UTF-8","GBK")

txt1
a<- getNodeSet(txt,path="//a[@class='z-btn z-btn-red']")

txt1<- iconv(a,"UTF-8","GBK")
txt1


xiecheng<- readLines(
  "http://www.chehang168.com/index.php?c=index&m=brand&pbid=1",
  encoding = "UTF-8")
write(xiecheng,"xiecheng.txt")

d2 =debugGatherer()
cHandle2<- getCurlHandle(followlocation=1,
                         debugfunction=d2$update,verbose=TRUE,
                         cookiefile="c:/cookiefile.txt")
temp<- getURL(url,curl=cHandle2,.encoding="gbk")
temp
cHandle2
