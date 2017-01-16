library(RODBC)
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)
library(reshape2)
library(reshape)

#读取数据
total_table <- read.csv('C:/Users/cck{28488747}/Desktop/系统补录/temp.csv',header = TRUE, sep = ',',stringsAsFactors =FALSE)

head(total_table)

#连接数据库,取列表
conn<- odbcConnect("CHEKU",uid="bossaccess",pwd="master-502")

series_list <- (sqlQuery(conn,"SELECT cheku_car_brand.id, cheku_car_brand.name, cheku_car_series.id, cheku_car_series.name, cheku_car_model.id, cheku_car_model.name, cheku_car_msrp.price
FROM (cheku_car_msrp INNER JOIN cheku_car_model ON cheku_car_msrp.model_id = cheku_car_model.id) INNER JOIN (cheku_car_brand INNER JOIN cheku_car_series ON cheku_car_brand.id = cheku_car_series.brand_id) ON cheku_car_model.series_id = cheku_car_series.id"))
close(conn)


# series_list
x1<- as.character(series_list$id)
x2<-as.character(series_list$name)  
x3<- as.character(series_list$id.1)
x4<- as.character(series_list$name.1)
x5<- as.character(series_list$id.2)
x6<- as.character(series_list$name.2)
x7<- as.character(series_list$price)
series<- cbind(brand_id = x1,品牌 = x2,   
          series_id = x3,车系 = x4,
          model_id = x5,车型 = x6,
          price= x7)



lookup<- data.frame(series)
lookup<- mutate(lookup,look = paste(lookup$品牌,lookup$车系,lookup$车型,sep=''))
total_table<- mutate(total_table,look = paste(total_table$品牌,total_table$车系,total_table$车型,sep=''))


library(plyr)
plyrl<- join(total_table,lookup,by = 'look')

head(plyrl)
