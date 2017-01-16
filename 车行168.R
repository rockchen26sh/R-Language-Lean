library(stringr)
a<-readLines("http://test.1111che.com/boss/system/chehang?brand=别克",encoding = 'UTF-8')
b<- str_split(a,"\\\\n")
b<-unlist(b)
price<- b[grep("<b>",b)]
info<- b[grep("<h3><a href=",b)]
info2<- b[grep("车源所在地",b)]
re_price<- substr(price,regexpr("<span class=\\\"pr\\\">",price)+28,regexpr("</b></span>",price)-2)
re_mode<- substr(info,regexpr("target",info)+18,regexpr("</a>",info)-1) 
re_msrp<- substr(info,regexpr("指导价",info)+4,regexpr("万",info)-1) 
re_location<- substr(info2,regexpr("车源所在地",info2)+6,regexpr("</cite><cite><b style",info2)-1) 
car_result<- as.data.frame(NULL)
car_result<- cbind(re_mode,re_msrp,re_price,re_location)
colnames(car_result)<- c("车型","msrp","价格","地区")
car_result
write.table(car_result,"1.csv",sep = ";")


