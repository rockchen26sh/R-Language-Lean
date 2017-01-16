head(diamonds)
summary(diamonds)
sample_data<- sample(2,nrow(diamonds),replace=TRUE,drop(c(0.7,0.3)))
head(sample_data)
tail_data<- diamonds[sample_data==1,]
test_data<- diamonds[sample_data==2,]
summary(tail_data)
library(ggplot2)
ggplot(tail_data,aes(x=depth))+geom_histogram()
ggplot(tail_data,aes(x=color,y=depth))+geom_boxplot() #ggplot??ʽ??????ͼ

library(car)
par(mfrow=c(1,2))
hist(tail_data$depth,prob=T,xlab="")
lines(density(tail_data$depth))
qq.plot(tail_data$depth)

library(lattice)
bwplot(depth~color,data=tail_data)  #lattice??????ͼ

cor(tail_data[,-c(2,3,4)])
symnum(cor(tail_data[,-c(2,3,4)]))

lm_model<- lm(price~.,data=tail_data)
summary(lm_model)

step_lm_model<- step(lm_model)
summary(step_lm_model)

par(mfrow=c(2,2))
plot(step_lm_model)

library(rpart)
tail_data<- tail_data[1:7]
tree_model<- rpart(price~.,data=tail_data)
library(maptree)
draw.tree(tree_model)
par(mfrow=c(1,1))
