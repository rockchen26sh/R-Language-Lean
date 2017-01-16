library(REmap)
library(RODBC)
library(reshape)
library(knitr)
library(grid)
library(vcd)
library(dplyr)
library(nycflights13)
library(tidyverse)
#连接数据库，调取数据
conn<- odbcConnect("CHEKU",uid="bossaccess",pwd="master-502")
layout_data<- sqlQuery(conn,
                       "select id,update_time,vincode,series,status,price,seller_name,tickType1,seller_area1,seller_area2  
                       FROM cheku_verification Where series='赛欧'")
close(conn)


datatotal<- cbind(layout_data,"Shanghai")
datatotal$seller_area2<- as.character(datatotal$seller_area2)
status_count<- summary(layout_data$seller_area1)
city_count<- summary(layout_data$seller_area2)
series_count<- summary(layout_data$series)
outlocation<- "Shanghai"
data1<- as.data.frame(cbind(outlocation,city_count)) 
a<-rownames(data1)
data1<- as.data.frame(cbind(outlocation,a,city_count)) 
data1
colnames(data1)<- c("outlocation","shippinglocation")
data2<- data1[,2]
data2<- as.character(data2)
data2<- get_geo_position(data2)
data3<- cbind(data2[-3],data1[3])
options(remap.js.web = T)

remapH(data3,maptype='china',
       theme=get_theme("Dark"),
       blurSize = 40,
       color=c('blue','cyan','lime','yellow','red'),
       minAlpha = 2,
       opacity=1)


a

#整理数据
count<- layout_data[c("seller_area1","seller_area2","series")]

data<- as.data.frame(summary(count$seller_area2))
data<- cbind(data,rownames(data))
temp<- as.character(data[,2])  
locationinfo<- get_geo_position(as.vector(data[,2]))
data1<- cbind(locationinfo[-3],data[,1])
data2<- cbind("上海市",temp)
datatotal<- cbind(data2$temp,data1)





options(remap.js.web = T)
lay1<- remapH(data1,maptype='china',
       theme=get_theme("Dark"),
       blurSize = 40,
       color=c('blue','cyan','lime','yellow','red'),
       minAlpha = 2,
       opacity=1)

data3<- datatotal[c(-2,-3)]
lay2<-remapC(data3,markPointData=data3[,2],title = "", subtitle = "", 
       theme =get_theme("Blue"))

count
count$seller_area2<- as.character(count$seller_area2)


data2 
data2<- as.data.frame(data2)
remap(data2, title = "", subtitle = "", 
      theme =get_theme("None", lineColor = "yellow",
                       backgroundColor = "#1b1b1b", titleColor = "#fff",
                       borderColor = "rgba(100,149,237,1)", regionColor = "#1b1b1b"))


remap(data2, title = "", subtitle = "", 
      theme =get_theme("Blue"))



knitrREmap(lay1)
