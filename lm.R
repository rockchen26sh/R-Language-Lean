carbon<- c(0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.20,0.21,0.23)
strong<- c(42,43.5,45,45.5,45,47.5,49,53,50,55,55,60)
c_s<- cbind(carbon,strong)
mode(c_s)
lm.sol<- lm(strong~ 1+ carbon)
summary(lm.sol)
qplot(x=carbon,y=strong)+geom_line()

library(ggplot2)

t1<- t2<- t3<- t4<-0

for (i in 1:length(carbon)){
  t1<- t1+ carbon[i]*carbon[i]
  t2<- t2+ carbon[i]
  t3<- t3+ carbon[i]*strong[i]
  t4<- t4+strong[i]
}
t1
t2
t3
t4
a= (t3*length(carbon)-t2*t4)/(t1*length(carbon)-t2*t2)
a
b= (t1*t4 - t2*t3)/(t1*length(carbon)-t2*t2)
b
lm.sol


plot(lm.sol)

anova(lm.sol)
lm.sol
coef(lm.sol)
residuals(lm.sol)
deviance(lm.sol)
print(lm.sol)
z<- data.frame(carbon=c(50,63,67,90))
predict(lm.sol,z,level=0.95,interval = "prediction")
lm.sol

f<- matrix(c(194.5, 20.79, 1.3179, 131.79,
      194.3, 20.79, 1.3179, 131.79,
      197.9, 22.40, 1.3502, 135.02,
      198.4, 22.67, 1.3555, 135.55,
      199.4, 23.15, 1.3646, 136.46,
      199.9, 23.35, 1.3683, 136.83,
      200.9, 23.89, 1.3782, 137.82,
      201.1, 23.99, 1.3800, 138.00,
      201.4, 24.02, 1.3806, 138.06,
      201.3, 24.01, 1.3805, 138.05,
      203.6, 25.14, 1.4004, 140.04,
      204.6, 26.57, 1.4244, 142.44,
      209.5, 28.49, 1.4547, 145.47,
      208.6, 27.76, 1.4434, 144.34,
      210.7, 29.04, 1.4630, 146.30,
      211.9, 29.88, 1.4754, 147.54,
      212.2, 30.06, 1.4780, 147.80),ncol=4,byrow=T,dimnames=list(1:17,c("F","h","log","log100"))

forbes<- as.data.frame(f[-12,])
lm.sol<- lm(F~1+log100,data=forbes)
lm.sol
plot(forbes$F,forbes$log100)
summary(lm.sol)


library(ggplot2)
diamonds
plot(diamonds)
par(mfrow=c(2,2))
lm.sol<- lm(price~carat,data=sdiamonds)
plot(diamonds$carat,diamonds$price)
abline(lm.sol)
summary(lm.sol)


residuals(lm.sol)

plot(lm.sol)
text(res[which(res>501)],labels="out",adj=1.2)
length(res[which(res>501)])
length(res[which(res< -789)])
nrow(sdiamonds)
sdiamonds<- sample(2,nrow(diamonds),replace=T,drop(c(0.7,0.3)))
head(sdiamonds)
sdiamonds<- diamonds[sdiamonds==2,]

library(dplyr)
sdiamonds<- mutate(sdiamonds,size=x*y*z)

cor(sdiamonds[c("price","size","depth")])

plot(sdiamonds[c("size","price")])
lm.sol<- lm(price~size,data=sdiamonds)

res<- residuals(lm.sol)
plot(res)

z<- data.frame()

summary(lm.sol)
?cor
sample_data<- sample(2,nrow(diamonds),replace=TRUE,drop(c(0.7,0.3)))
head(sample_data)
tail_data<- diamonds[sample_data==1,]

plot(sdiamonds)







#多元线性回归

blood<-data.frame(
  weight=c(76.0, 91.5, 85.5, 82.5, 79.0, 80.5, 74.5,
       79.0, 85.0, 76.5, 82.0, 95.0, 92.5),
  age=c(50, 20, 20, 30, 30, 50, 60, 50, 40, 55,
       40, 40, 20),
  blood= c(120, 141, 124, 126, 117, 125, 123, 125,
       132, 123, 132, 155, 147))


lm.sol<- lm(blood~.,data=blood)
summary(lm.sol)
res<-residuals(lm.sol)
quantile(res,probs=seq(0,1,0.1))
#预测
z<- data.frame(weight=c(75,70),age=c(33,40))
predict(lm.sol,z,interval = "prediction",level = 0.95)
#修正拟合模型
new.model<-update(old.model,new.model)
lm1<- lm(blood~weight,data=blood)
lm2<- update(lm1,~.+age,data=blood)#在原有模型中添加自变量
plot(blood$blood~blood$weight)
abline(lm(blood~weight,data=blood))

lm2.sol<- lm(blood~age+I(age^2),data = blood)
plot(blood$blood,blood$age)
abline(lm2.sol)
#例2牙膏数据


