library("RODBC")
#连接数据库，调取数据
conn<- odbcConnect("CHEKU",uid="bossaccess",pwd="master-502")
layout_data<- sqlQuery(conn,"select id,update_time,vincode,series,status,price,seller_name,tickType1 FROM cheku_verification")
close(conn)

#整理出11月数据
library(dplyr)
library(lubridate)

#增加月份
layout_data<-cbind(layout_data,month(layout_data$update_time))

#判断状态
layout_data$status1[layout_data$status==0] <-"待核销"
layout_data$status1[layout_data$status==1] <-"待回访"
layout_data$status1[layout_data$status==2] <-"待审核"
layout_data$status1[layout_data$status==3] <-"待开票"
layout_data$status1[layout_data$status==4] <-"已核销"

#判断票种
layout_data$tickType2[layout_data$tickType1==1] <-"用户票"
layout_data$tickType2[layout_data$tickType1==2] <-"增票"

#筛选11月份数据，剔除无价格信息数据
#选择月份
layout_data1<- filter(layout_data,layout_data$`month(layout_data$update_time)`==12)
#选择状态
layout_data1<- filter(layout_data1,layout_data1$status1=="已核销")
#选择票种
layout_data1<- filter(layout_data1,layout_data1$tickType2=="增票")


#统计汇总
attach(layout_data1)
aggregate(price~seller_name,FUN="sum")
temp<- aggregate(price,by=list(seller_name,tickType2,price),FUN = sum)
colnames(temp)<- c("seller","票种","价格","合计")
detach(layout_data1)

#图表
library("ggplot2")
ggplot(layout_data1,aes(x=seller_name,y=price,fill=tickType2))+geom_col()+coord_flip()+geom_text(aes(label=price),position = position_stack(vjust = 0.5))

ggplot(temp,aes(x=seller,y=合计,fill=票种))+geom_col()+coord_flip()+geom_text(aes(label=合计),position=position_stack(vjust=0.5))
