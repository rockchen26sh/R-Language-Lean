library(quantmod)
#抓取数据
getSymbols("EDU",src="yahoo",from="2013-08-20",to="2013-09-02")
EDU

new.environment<- new.env() #新建对象夹
getSymbols("AAPL",env=new.environment,scr="yahoo",from="2013-10-01",to="2013-10-23")
AAPL
get("AAPL",envir=new.environment)


getSymbols("000300.ss",env=new.environment,scr="yahoo",from="2013-10-01",to="2013-10-23")
get("000300.ss",envir=new.environment)

getFX("HKD/USD",from="2013-10-20",env=new.environment)
get("HKDUSD",envir=new.environment)


getFinancials("AAPL")
viewFinancials(AAPL.f)
viewFinancials(AAPL.f,c("CF","IS","BS"),"Q")

getDividends("AAPL",from="2012-01-01",to="2013-10-25",env=new.environment,verbose=TRUE)

is.OHLC(EDU) #判断数据类型

has.OHLC(EDU,which=FALSE)
has.OHLC(EDU,which=TRUE) #返回OHLC对应列数
EDU

Op(EDU)  #提取数据(OP)

#计算函数
Delt(Op(EDU),type=("arithmetic")) #计算一个序列的一个阶段到另一个阶段的变化率或两个序列之间的变化率
Delt(Op(EDU),Cl(EDU))#计算开市价与收盘价之间的变化率
Delt(Op(EDU),type=("log"))#type为

first(EDU,5)
last(EDU,5)

Next(EDU,1)#所有观测值前进K各单位
EDU

to.weekly(EDU) #转化为周数据
to.monthly(EDU)#转化为月数据

#quantmod的ETL函数下载Apple,Microsoft,Oracle,Google四家公司全量股票行情数据
getSymbols("AAPL")
getSymbols("MSFT")
getSymbols("ORCL")
getSymbols("GOOG")
first(AAPL,5)
last(AAPL,5)
first(MSFT,5)
last(MSFT,5)
first(ORCL,5)
last(ORCL,5)
first(GOOG,5)
last(GOOG,5)

library(quantmod)#加载包
library(corrplot)
library(scales)
#1)求出Apple公司在2013.1-2013.10的股票总成交量使用
getSymbols("AAPL",SCR="yahoo",from="2013-01-01",to="2013-10-31") 
sum(Vo(AAPL))
#2)找出这些股票暴涨暴跌的时间点（例如开盘价或收盘价比前一天涨跌幅度超过2%）
#通过搜索引擎寻找是什么原因导致这些暴涨暴跌，观察（或用程序分析）数据，看就暴涨暴跌事件是否有可以利用的买卖规律
getSymbols("AAPL")
getSymbols("MSFT")
getSymbols("ORCL")
getSymbols("GOOG")
total_ad<- data.frame(Ad(AAPL),Ad(MSFT),Ad(ORCL),Ad(GOOG))
AAPL_delt<- Delt(Ad(AAPL),type="arithmetic")#使用考虑到有拆股除权的情况，故使用调整价格
summary(AAPL_delt)
AAPL_up<- as.data.frame(AAPL_delt[which(AAPL_delt>0.05)])#涨幅超过5%的数据
AAPL_down<- as.data.frame(AAPL_delt[which(AAPL_delt< -0.05)])#跌幅超过5%的数据
AAPL_down<- cbind(row.names(AAPL_down),AAPL_down,"AAPL","down")
AAPL_up<- cbind(row.names(AAPL_up),AAPL_up,"AAPL","up")
colnames(AAPL_down)<- c("date","data","Company","updown")
colnames(AAPL_up)<- c("date","data","Company","updown")

MSFT_delt<- Delt(Ad(MSFT),type="arithmetic")#使用考虑到有拆股除权的情况，故使用调整价格
summary(MSFT_delt)
MSFT_up<- as.data.frame(MSFT_delt[which(MSFT_delt>0.05)])#涨幅超过5%的数据
MSFT_down<- as.data.frame(MSFT_delt[which(MSFT_delt< -0.05)])#跌幅超过5%的数据
MSFT_down<- cbind(row.names(MSFT_down),MSFT_down,"MSFT","down")
MSFT_up<- cbind(row.names(MSFT_up),MSFT_up,"MSFT","up")
colnames(MSFT_down)<- c("date","data","Company","updown")
colnames(MSFT_up)<- c("date","data","Company","updown")

ORCL_delt<- Delt(Ad(ORCL),type="arithmetic")#使用考虑到有拆股除权的情况，故使用调整价格
summary(ORCL_delt)
ORCL_up<- as.data.frame(ORCL_delt[which(ORCL_delt>0.05)])#涨幅超过5%的数据
ORCL_down<- as.data.frame(ORCL_delt[which(ORCL_delt< -0.05)])#跌幅超过5%的数据
ORCL_down<- cbind(row.names(ORCL_down),ORCL_down,"ORCL","down")
ORCL_up<- cbind(row.names(ORCL_up),ORCL_up,"ORCL","up")
colnames(ORCL_down)<- c("date","data","Company","updown")
colnames(ORCL_up)<- c("date","data","Company","updown")

GOOG_delt<- Delt(Ad(GOOG),type="arithmetic")#使用考虑到有拆股除权的情况，故使用调整价格
summary(GOOG_delt)
GOOG_up<- as.data.frame(GOOG_delt[which(GOOG_delt>0.05)])#涨幅超过5%的数据
GOOG_down<- as.data.frame(GOOG_delt[which(GOOG_delt< -0.05)])#跌幅超过5%的数据
GOOG_down<- cbind(row.names(GOOG_down),GOOG_down,"GOOG","down")
GOOG_up<- cbind(row.names(GOOG_up),GOOG_up,"GOOG","up")
colnames(GOOG_down)<- c("date","data","Company","updown")
colnames(GOOG_up)<- c("date","data","Company","updown")



Total<- rbind(AAPL_down,AAPL_up,MSFT_down,MSFT_up,ORCL_down,ORCL_up,GOOG_down,GOOG_up)
Total$date<-as.Date(Total$date)
Total<- Total[order(Total$date),]
Total$data<- percent(Total$data)
library(ggplot2)
library(reshape2)
total_ad<- data.frame(Ad(AAPL),Ad(MSFT),Ad(ORCL),Ad(GOOG))
total_ad<- cbind(row.names(total_ad),total_ad)
total_ad$`row.names(total_ad)`<- as.Date(total_ad$`row.names(total_ad)`)
colnames(total_ad)<- c("date","AAPL.Ad","MSFT.Ad","ORCL.Ad","GOOG.Ad")

Ad_melt<- melt(total_ad,id="date")
ggplot(Ad_melt,aes(x=date,y=value))+geom_point()+facet_grid(variable~.)


DT::datatable(Total,filter = 'top')
#3)截取一段时间内这四家公司股价数据（注意分红派息除权对股价的影响），用R中的相关性分析判断股价之间的相关性，或者用R基础课程第八周所讲的MIC指标对其进行分析
getSymbols("AAPL",from="2014-01-01",to="2016-12-01")
getSymbols("MSFT",from="2014-01-01",to="2016-12-01")
getSymbols("ORCL",from="2014-01-01",to="2016-12-01")
getSymbols("GOOG",from="2014-01-01",to="2016-12-01")


total_ad<- data.frame(Ad(AAPL),Ad(MSFT),Ad(ORCL),Ad(GOOG))
cor(total_ad)

corrplot(cor(total_ad),order='AOE',addCoef.col = "grey")

