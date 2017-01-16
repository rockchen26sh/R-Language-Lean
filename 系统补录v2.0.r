library(RODBC)
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)
library(reshape2)
library(reshape)

#��ȡ����
total_table <- read.csv('C:/Users/cck{28488747}/Desktop/ϵͳ��¼/temp.csv',header = TRUE, sep = ',',stringsAsFactors =FALSE)

head(total_table)

#�������ݿ�,ȡ�б�
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
series<- cbind(brand_id = x1,Ʒ�� = x2,   
          series_id = x3,��ϵ = x4,
          model_id = x5,���� = x6,
          price= x7)



lookup<- data.frame(series)
lookup<- mutate(lookup,look = paste(lookup$Ʒ��,lookup$��ϵ,lookup$����,sep=''))
total_table<- mutate(total_table,look = paste(total_table$Ʒ��,total_table$��ϵ,total_table$����,sep=''))


library(plyr)
plyrl<- join(total_table,lookup,by = 'look')

head(plyrl)
