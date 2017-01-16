library(ggplot2)
#读取数据
gdata<- matrix(data=readLines("c:/lesson8.txt"),ncol=3,byrow=TRUE)
colnames(gdata)<- c("time","ip","pv")
gdata<- as.data.frame(gdata) #转换为数据框

#融合数据
gdata<-melt(gdata,id="time")
gdata$value<- as.numeric(gdata$value)

#日期格式变换
gdata$time <- sub(pattern="HKT",replacement="",gdata$time)
Sys.setlocale("LC_TIME", "C")
gdata$time <- as.Date(strptime(gdata$time, "%a %b %d %H:%M:%S %Y"))
DT::datatable(gdata,filter = 'top') #数据列表

#输出图片
qplot(time,value,data=gdata,colour=variable,geom=c("point","line"),main="IP/PV Graphics")+
  facet_grid(variable~.,scales = "free_y") #分面并将Y轴设置为按数值范围自由变换
