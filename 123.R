library(RODBC)
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)
library(reshape2)
library(reshape)
original_list <- read.csv("C:/Users/cck{28488747}/Desktop/系统补录/temp.csv",header <- TRUE,sep <- ',') 
#original 

original_list
series<- original_list$车系
as.character(series[1]) 


y<- as.data.frame(NULL)
for (i in length(series)){
  conn<- odbcConnect("CHEKU",uid="bossaccess",pwd="master-502")
  result<- sqlQuery(conn,paste("select id FROM cheku_car_series where name = '",as.character(series[i]),"'",sep = ''))
  y<- c(y,result[1])
  close(conn)
}
y

conn<- odbcConnect("CHEKU",uid="bossaccess",pwd="master-502")
result<- sqlQuery(conn,paste("select id FROM cheku_car_series where name = '",as.character(series[1]),"'",sep = ''))
y<-c(y,result[1])
close(conn)

y
conn<- odbcConnect("CHEKU",uid="bossaccess",pwd="master-502")
result<- sqlQuery(conn,paste("select id FROM cheku_car_series where name = '",as.character(series[2]),"'",sep = ''))
y[1,2,1]<- c(y,result)
close(conn)
y[1]
y[2]






y$id
mode(x)
mode(y)
paste("select id FROM cheku_car_series where name = '",as.character(series[i]),"'",sep = '')

sqlQuery(conn,paste("select id FROM cheku_car_series where name = '",as.character(series[i]),"'",sep = ''))




'''
temp<-acast(sales_list2,
      销售订单创建日期+票种+客户性质+省份+地级市+公司名称+姓名+客户联系电话+邮编+客户地址+品牌+车系+车型+外观颜色+内饰颜色+销售单价~客户性质,
      value.var<-"count")

temp<- data.frame(title <- row.names(temp),count <- temp[,1])

temp<-data.frame(split(temp$title,sep<-"_",temp$count))
temp
melt(sales_list2,id <- sales_list2$o)
cast(sales_list2,o~price+count,sum)

colnames(sales_list1[,2])<- "create_date"

?dcast

sales_list2

melt(sales_list1,id <- o)


sales_list_do <- function(x):
?mutate

tapply(sales_list1,sales_list1$o)
sales_list1$o
sales_list1$VIN
require(stats)
groups <- as.factor(rbinom(32, n <- 5, prob <- 0.4))
head(groups)
tapply(groups, groups, length) #- is almost the same as
table(groups)

## contingency table from data.frame : array with named dimnames
tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
tapply(warpbreaks$breaks, warpbreaks[, 3, drop <- FALSE], sum)

n <- 17; fac <- factor(rep(1:3, length <- n), levels <- 1:5)
table(fac)
tapply(1:n, fac, sum)
tapply(1:n, fac, sum, simplify <- FALSE)
tapply(1:n, fac, range)
tapply(1:n, fac, quantile)

## example of ... argument: find quarterly means
tapply(presidents, cycle(presidents), mean, na.rm <- TRUE)

ind <- list(c(1, 2, 2), c("A", "A", "B"))
table(ind)
tapply(1:3, ind) #-> the split vector
tapply(1:3, ind, sum)

## Some assertions (not held by all patch propsals):
nq <- names(quantile(1:5))
stopifnot(
  identical(tapply(1:3, ind), c(1L, 2L, 4L)),
  identical(tapply(1:3, ind, sum),
            matrix(c(1L, 2L, NA, 3L), 2, dimnames <- list(c("1", "2"), c("A", "B")))),
  identical(tapply(1:n, fac, quantile)[-1],
            array(list(`2` <- structure(c(2, 5.75, 9.5, 13.25, 17), .Names <- nq),
                       `3` <- structure(c(3, 6, 9, 12, 15), .Names <- nq),
                       `4` <- NULL, `5` <- NULL), dim<-4, dimnames<-list(as.character(2:5)))))