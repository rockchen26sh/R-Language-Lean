library(ggplot2)
library(reshape)
library(dplyr)
hisdata<- read.csv(file = "C:/Users/chenshengkang/Desktop/hupai.csv",header = FALSE)
colnames(hisdata)<- c("Time","15Y11M","15Y12M","16Y01M","16Y02M","16Y03M",
                      "16Y04M","16Y05M","16Y06M","16Y07M","16Y08M","16Y09M",
                      "16Y10M","16Y11M","16Y12M")

hisdata
standprice<-as.vector(hisdata[61,-1] )

moni<- function(intime,addprice,outtime){
  #初始化
  outprice<- as.vector(NULL)
  standprice<- as.vector(NULL)
  lm1s<- as.vector(NULL)
  l0s<- as.vector(NULL)
  l1s<- as.vector(NULL)
  l2s<- as.vector(NULL)
  #写入数据
  for( i in 2:ncol(hisdata)){
    temp<- hisdata[intime+1,i]+addprice
    outprice<- c(outprice,temp)
    temp<- hisdata[61,i]
    standprice<- c(standprice,temp)
    temp<- hisdata[outtime,i]
    lm1s<- c(lm1s,temp)
    temp<- hisdata[outtime+1,i]
    l0s<- c(l0s,temp)
    temp<- hisdata[outtime+2,i]
    l1s<- c(l1s,temp)
    temp<- hisdata[outtime+3,i]
    l2s<- c(l2s,temp)
  }
  #Result
  result<- as.data.frame(NULL)
  result<- rbind(outprice-lm1s,outprice-l0s,outprice-l1s,outprice-l2s,outprice-standprice)
  colnames(result)<- colnames(hisdata[-1])
  rownames(result)<-c(outtime-1,outtime,outtime+1,outtime+2,"Diff")
  result
  a<- ifelse(result[-5,]<=300,1,0)
  for (i in 1:ncol(result)){
    for (m in 1:4){
      a[m,i]<- ifelse(result[5,i]<300 & result[5,i]>=0,a[m,i],0 )
    }
  }
  b<-melt(a,id=colnames(result))
  colnames(b)<- c("time","mon","value")
  b<- cbind(intime,addprice,b)
  return(b)
}

moni(45,700,56)

intimearrange<- c(45,48,50)
addpricearrange1<- c(700,800,900,1000)
addpricearrange2<- c(500,600,700,800)
addpricearrange3<- c(400,500,600,700)

re<- as.data.frame(NULL)
for (mm in 1:length(addpricearrange1)){
  temp<- moni(intimearrange[1],addpricearrange1[mm],56) 
  re<- rbind(re,temp)
}
re
result<- as.data.frame(NULL)
result<- rbind(result,re)

re<- as.data.frame(NULL)
for (mm in 1:length(addpricearrange2)){
  temp<- moni(intimearrange[2],addpricearrange2[mm],56) 
  re<- rbind(re,temp)
}
result<- rbind(result,re)

re<- as.data.frame(NULL)
for (mm in 1:length(addpricearrange1)){
  temp<- moni(intimearrange[3],addpricearrange3[mm],56) 
  re<- rbind(re,temp)
}
result<- rbind(result,re)
result
o<-cast(result,time~mon~addprice~intime,sum)

#表格
#提交时间对比
cast(result,time~mon,sum)

#intime对比
cast(result,intime~mon,sum)

#加价幅度对比
cast(result,intime+addprice~mon,sum)

#综合
cast(result,time~mon~addprice~intime,sum)

#图表
p<- ggplot(result,aes(time,mon))+geom_tile(aes(fill=value))+facet_wrap(~intime+addprice)+
scale_fill_gradient2(high="green", low="red") +
  labs(x="提交时间", y="年月") +
  theme_bw() +
  theme(
    axis.title.x=element_text(size=16),
    axis.title.y=element_text(size=16),
    axis.text.x=element_text(size=12, colour="grey50"),
    axis.text.y=element_text(size=12, colour="grey50"),
    legend.title=element_text(size=14),
    legend.text=element_text(size=12),
    legend.key.size = unit(0.8, "cm"))#需要载入grid包来调整legend的大小
p
#DT HTML交互
library(DT)
temp<- result
temp$intime<- as.integer(temp$intime)
temp$addprice<- as.integer(temp$addprice)
temp
datatable(temp,colnames=c("加价时间","加价幅度","提交时间","月份","是否中标"),filter = 'top', options = list(
  pageLength = 50, autoWidth = TRUE))
temp
#实时查询
temp<- moni(45,1100,56)
temp$value<- as.numeric(temp$value)
cast(temp,mon+time~intime,sum)


hisdata2<- read.csv(file = "C:/Users/chenshengkang/Desktop/hupai2.csv",header = FALSE,sep = ',')
colnames(hisdata2)<- c("date","price","add","s")
hisdata3<- hisdata2[3:4]
plot(hisdata3$s,hisdata3$add)
his_lm<- lm(add~ s+I(s^2),data=hisdata3)
summary(his_lm)
abline(his_lm)
library(car)
scatterplot(s~add,data=hisdata3)
lines(hisdata3$s,fitted(his_lm))

