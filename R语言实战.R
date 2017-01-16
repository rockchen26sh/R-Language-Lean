data(mtcars)
attach(mtcars)
par(mfrow=c(1,1))
plot(wt,mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
detach(mtcars)
pdf("mygraph1.pdf")
dev.off

#第三章
dose<- c(20,30,40,45,60)
drugA<- c(16,20,27,40,60)
drugB<- c(15,18,25,31,40)
plot(dose,drugA,type="b")


opar<- par(no.readonly=TRUE)
par(lty=2,pch=17) #lty:线型参数，pch：标记形状参数
par(opar)
par(lty=2)
plot(dose,drugA,type="b",lty=2,pch=17,cex=1,lwd=2,col=rgb(0.5,0.5,1),fg="#EEEEEE",bg="#EEEEEE")
#lty:线型参数，pch：标记形状参数 cex:标记大小 lwd：线粗 col：绘制图形颜色 fg:前景色 bg：背景色

par(font.lab=3,cex.lab=1.5,font.main=4,cex.main=2)
#font.lab: 坐标轴标签字体式样

#第四章
manager<- c(1,2,3,4,5)
date<- c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
country<- c("US","US","UK","UK","UK")
gender<- c("M","F","F","M","F")
age<- c(32,45,25,39,99)
q1<- c(5,3,3,3,2)
q2<- c(4,5,5,3,2)
q3<- c(5,2,5,4,1)
q4<- c(5,5,5,NA,2)
q5<- c(5,5,2,NA,1)
leadership<- data.frame(manager,date,country,gender,age,q1,q2,q3,q4,q5,stringsAsFactors = FALSE)
#对年龄进行分类
leadership$agecat[leadership$age>75]<- "Elder"
leadership$agecat[leadership$age>=55 & leadership$age<=75]<- "Middle Aged"
leadership$agecat[leadership$age<55]<- "Young"
#紧凑型编码：
leadership<- within(leadership,{
             agecat<- NA
             agecat[age>75] <- "Elder"
             agecat[age>=55 & age<= 75] <- "Middle Aged"
             agecat[age<55] <- "Young"
               })

#判断缺失值
is.na(leadership)
#重新编码某些值为缺失值
leadership$age[leadership$age==99]<- NA
#排除数据中的缺失值
newdata<- na.omit(leadership)
newdata

#日期值
mydates<- as.Date(c("2007-06-22","2004-02-13"))
mydates
strDates<- c("01/05/1965","08/16/1975")
strDates
dates<- as.Date(strDates,"%m%d%Y")

#制定格式的日期值
today<- Sys.Date()
format(today,format="%B %d %Y")
format(today,format="%A")

#计算日期时间间隔
today<- Sys.Date()
dob<- as.Date("1975-10-12")
difftime(today,dob,units="days")
#日期转换为日期变量
strDates<- as.character(today)
today

#类型转换
a<- c(1,2,3)
is.numeric(a)
is.vector(a)

a<- as.character(a)
is.character(a)

#数据排序
newdata<- leadership[order(leadership$age),]
attach(leadership)
newdata<-leadership[order(gender,age)]  #升序排列
newdata<-leadership[order(gender,-age)] #降序排列
newdata

#数据集合并
total<- merge(dataframeA,dataframeB,by="ID") #将dataframeA和dataframeB按照ID进行合并(inner join)
total<- merge(dataframeA,dataframeB,by=c("ID","Country")) #按照ID和country进行合并

total<- cbind(a,b) #a和b直接合并 添加列
total<- rbind(a,b) #添加行

#数据集取子集
newdata<- leadership[,c(6:10)] #选择第6到第10列数据

myvars<- c("q1","q2","q3","q4","q5")
newdata<- leadership[myvars] #根据列名取值

myvars<- paste("q",1:5, sep="")
newdata<- leadership[myvars]

#剔除变量
myvars<- names(leadership) %in% c("q3","q4")   #names生存列名向量 in逻辑判断符合q3 q4返回真
newdata<- leadership[!myvars]

newdata<- leadership[c(-8,-9)] #使用减号剔除对应列

newdata<- leadership$q3<- leadership$q4 <- NULL #Q3 Q4列赋值为空的方式删除列

#选择数据
newdata <- leadership[1:3,] #直接选择1到3列
newdata<- leadership[which(leadership$gender=="M"&leadership$age>30),] #条件选择 

attach(leadership)
newdata<- leadership[which(gender=='M' & age >30),]
detach(leadership)
newdata
leadership$date <- as.Date(leadership$date,"%m%d%y")
startdate<- as.Date("2009-01-01")
enddate<- as.Date("2009-10-31")
newdata<- leadership[which(leadership$date >= startdate& leadership$date<= enddate),]#筛选日期范围
#subset(数据集,条件,显示列)
newdata<- subset(leadership,age>= 35 | age<24,select=c(q1,q2,q3,q4))
newdata<- subset(leadership,gender=="M" & age> 25, select=gender:q4)

#随机抽样 sample(数据范围,抽取元素数量,是否允许重复)
mysample<- leadership[sample(1:nrow(leadship),3,replace=FALSE),]
#使用sql语句查询
library(sqldf)
newdf<- sqldf("select * from mtcars where carb=1 order by mpg",row.names=TRUE)
sqldf("select avg(mpg) as avg_mpg,avg(disp) as avg_disp, gear from mtcars where cyl in (4,6) group by gear")

#第五章
options(digits=2)#设置小数点位数2

stud<- x
z<- scale(stud[,2:4])  #分数标准化
score<- apply(z,1,mean) #算出综合分数
stud<- cbind(stud,score) 
y<-quantile(score,c(.8,.6,.4,.2)) #计算分界点
stud$评分[score>= y[1]] <- "A"    #根据条件分级
stud$评分[score< y[1] & score >= y[2]] <- "B"
stud$评分[score< y[2] & score >= y[3]] <- "C"
stud$评分[score< y[3] & score >= y[4]] <- "D"
stud$评分[score< y[4]] <- "F"
name<- strsplit(stud$姓名," ")   #拆分姓名
lastname<- sapply(name,"[",2)
firstname<- sapply(name, "[",1)
stud<- cbind(firstname,lastname,stud[,-1])
stud<- stud[order(firstname,lastname),]
stud

#转置函数
cars<- mtcars[1:5,1:4]
cars
t(cars)

#根据cyl和gear 以平均数的方式来折叠数据
options(digits=2)
attach(mtcars)
aggdata<- aggregate(mtcars,by=list(cyl,gear),FUN="mean",na.rm=TRUE)
aggdata
detach(mtcars)

library(reshape)


data(mtcars)
str(mtcars)
attach(mtcars)
aggregate(mtcars,by=list(cyl),FUN=mean)
detach(mtcars)

#融合与重铸
id<- c(1,1,2,2)
Time<- c(1,2,1,2)
X1<- c(5,3,6,2)
X2<- c(6,5,1,4)
mydata<- data.frame(id)
mydata<- cbind(mydata,Time,X1,X2)
mydata

#融合
md<- melt(mydata,id=c("id","Time"))
md
#执行整合统计
cast(md,id~varibale,sum)
cast(md,Time~variable,mean)
cast(md,id~Time,mean)

#不执行整合统计
cast(md,id+Time~variable)
cast(md,id+variable~Time)
cast(md,id~variable+time)

#第六章

#第七章
vars<- c("mpg","hp","wt")
head(mtcars[vars])

summary(mtcars[vars])
# 最小值、最大值、四分位数、数值型变量均值、因子向量逻辑向量的频数统计

mystats<- function(x,na.omit=FALSE) {
  if ( na.omit)
    x<- x[! is.na(x)]
  m<- mean(x)
  n<- length(x)
  s<- sd(x)
  skew<- sum((x-m)^3/s^3)/n
  kurt<- sum((x-m)^4/s^4)/n-3
  return(c(n=n,mean=m,stdev=s,skew=skew,kurtosis=kurt))
}
sapply(mtcars[vars],mystats)
#sapply(数据集，函数)

#分组计算描述性统计量
aggregate(mtcars[vars],by=list(am=mtcars$am),mean)
aggregate(mtcars[vars],by=list(am=mtcars$am),sd)

dstats<- function(x)(c(n=length(x),mean=mean(x),sd=sd(x)))
by(mtcars[vars],mtcars$am,summary)
library(doBy)
summaryBy(mpg+hp+wt~am,data=mtcars,FUN=dstats)

library(psych)
describe.by(mtcars[vars],mtcars$am)

library(reshape)
dstats<- function(x)(c(n=length(x),mean=mean(x),sd=sd(x)))
dfm<- melt(mtcars,measure.vars=c("mpg","hp","wt"),id.vars=c("am","cyl"))
cast(dfm,am+cyl+variable~., dstats)

#频数表和列联表
library(grid)
library(vcd)
head(Arthritis)
mytable <- with(Arthritis,table(Improved))
mytable
prop.table(mytable)
prop.table(mytable)*100

mytable<- xtabs(~ Treatment+Improved,data=Arthritis)
mytable
margin.table(mytable,1)
prop.table(mytable,1)*100
margin.table(mytable,2)
mytable
prop.table(mytable,2)*100
prop.table(mytable)
addmargins(mytable)
addmargins(prop.table(mytable,2))
addmargins(prop.table(mytable,2),1)

library(gmodels)
CrossTable(Arthritis$Treatment,Arthritis$Improved)

mytable<- xtabs(~ Treatment+Sex+Improved,data=Arthritis)
mytable

ftable(mytable)
margin.table(mytable,1)
margin.table(mytable,2)
margin.table(mytable,3)
margin.table(mytable,c(1,3))
margin.table(mytable,c(1,2))
addmargins(ftable(prop.table(mytable,c(1,2))),2)
ftable(addmargins(prop.table(mytable,c(2,1)),3))*100

#独立性检验
#卡方检验
library(vcd)
mytable<- xtabs(~Treatment+Improved,data=Arthritis)
mytable
chisq.test(mytable)

mytable<- xtabs(~Improved+Sex,data=Arthritis)
chisq.test(mytable)

#Fisher精确检验
mytable<- xtabs(~Treatment+Improved,data=Arthritis)
mytable
fisher.test(mytable)

#Cochran-Mantel-Haenszel检验
mytable<- xtabs(~Treatment+Improved+Sex,data=Arthritis)
mytable
mantelhaen.test(mytable)

#相关性度量
library(vcd)
mytable<- xtabs(~Treatment+Improved,data=Arthritis)
assocstats(mytable)

#表格转换为扁平格式
mytable<- xtabs(~Treatment+Improved+Sex,data=Arthritis)
y<- ftable(margin.table(mytable,c(1,2,3)))
y
table2flat<- function(y) {
  df<- as.data.frame(y)
  rows<- dim(df)[1]
  cols<- dim(df)[2]
  x<- NULL
  for( i in 1:rows){
    for(j in 1:df$Freq[i]){
      row<- df[i,c(1:(cols-1))]
      x<- rbind(x,row)
    }
  }
  row.names(x)<-c(1:dim(x)[1])
  return(x)
}
table2flat(y)

#另一种方法 使用melt融合后根据变量数量创建记录条数量
mytable<- xtabs(~Treatment+Improved+Sex,data=Arthritis)
y<- ftable(margin.table(mytable,c(1,2,3)))
y
library(reshape)
y<- melt(y,id=c(Treatment,Improved,Sex))
y
makeflattable<- function(y,rows) {
  df<- as.data.frame(y)
  z<- NULL
  for (i in 1: rows) {
    m<- df[i,4]
    for( n in 1:m) {
      z<- rbind(z,df[i,])
    }
  }
  z<- z[-4]
  colnames(z)<- c("Treatment","Improved","Sex")
  return(z)
}
makeflattable(y,nrow(y))

#相关性研究
options(dights= 3)
help(state.x77)
data(state.x77)
states<- state.x77[,1:6]
states
cov(states)
cor(states)
cor(states,method = "spearman")
x<- states[,c("Population","Income","Illiteracy" ,"HS Grad")]
y<- states[,c("Life Exp" ,"Murder")]
x
y
cor(x,y)
#偏相关研究
library(ggm)
#在控制了收入、文盲率和高中毕业率时
#人口和谋杀率的偏相关系数
pcor(c(1,5,2,3,6),cov(states))

#相关性的显著性检验
cor.test(states[,3],states[,5])
library(psych)
corr.test(states,use="complete")
#diamonds数据集相关性检测
dia_test<- sample(2,nrow(diamonds),replace=TRUE,drop(c(0.7,0.3)))
head(dia_test)
dia_test<- diamonds[dia_test==1,]
head(dia_test)
x<- dia_test[,c("x","y","z")]
y<- dia_test[,"carat"]
corr.test(dia_test,use="complete")
corr.test(x,y)  #X Y Z坐标与重量相关

#T检验
library(MASS)
t.test(Prob~ So, data=UScrime)


#拆分日期
require(dplyr)
your_Data%>%separate(time,into = c("Date","Time"),sep=" ")%>%
  filter(Date=="2011/4/18")
