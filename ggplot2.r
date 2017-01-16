require(ggplot2)
data(diamonds)
set.seed(42)
small<- diamonds[sample(nrow(diamonds),1000),]
head(small)
summary(small)
p <- ggplot(data=small,mapping=aes(x=carat,y=price))
#aes()????????ӳ??XY??????
p+geom_point()
#geom_point()ɢ??ͼ
p <- ggplot(data=small,mapping=aes(x=carat,y=price,shape=cut))
#shape:??????ӳ?䵽??״
p <- ggplot(data=small,mapping=aes(x=carat,y=price,shape=cut,colour=color))
#colour:??????ӳ?䵽??ɫ

#ֱ??ͼ
p<- ggplot(data=small,aes(x=price))
p+geom_histogram()
p<- ggplot(small)
p+ geom_histogram(aes(x= price,fill=cut))
#fill:
p+ geom_histogram(aes(x= price,fill=cut),position="dodge")
#position:ֱ??ͼ????ѡ?? dodge??side-by-side fill:????ͼ

#??״ͼ
ggplot(small)+geom_bar(aes(x=clarity))
#geom_bar????״ͼ
ggplot()+geom_bar(aes(x=c(LETTERS[1:3]),y=1:3),stat = 'identity')
#geom_bar????ָ???߶Ȼ?ͼ

#?ܶȺ???ͼ
ggplot(small)+geom_density(aes(x=price,colour=cut))
#geom_density:?ܶ?ͼ colour????ӳ?䵽??????ɫ
ggplot(small)+geom_density(aes(x=price,fill=clarity))
#fill:????ӳ?䵽??????ɫ

#??ʽͼ
ggplot(small)+geom_boxplot(aes(x=cut,y=price,colour=color))
#geom_boxplot:??ʽͼ fill:????ӳ?䵽??????ɫ

#geom_abline
#geom_bar
#geom_blank
#geom_contour 
#geom_dotplot
#geom_errorbarh 
#geom_hex 
#geom_hline
#geom_line
#geom_map   
#geom_point 
#geom_polygon
#	geom_raster
#	geom_ribbon
#	geom_segment 
#geom_step 
#geom_tile
#geom_vline
#geom_area 
#geom_bin2d
#geom_boxplot  
#geom_crossbar
#geom_density2d 
#geom_errorbar
#geom_freqpoly 
#geom_histogram
#geom_jitter   
#geom_linerange
#geom_path 
#geom_pointrange
#geom_quantile 
#geom_rect
#geom_rug 
#geom_smooth
#geom_text  
#geom_violin


#????
ggplot(small)+geom_point(aes(x=carat,y=price,shape=cut,colour=color))+scale_y_log10()+scale_colour_manual(values=rainbow(7))
# y?????߽??ж????任????ɫ???òʺ?ɫ??ȡ7ɫ??
ggplot(small,aes(x=carat,y=price))+geom_point()+scale_y_log10()+stat_smooth()
# stat_smooth???ع??ߣ? ӳ?????ݷ???ggplot??

#stat_abline       stat_contour      stat_identity     stat_summary
#stat_bin          stat_density      stat_qq           stat_summary2d
#stat_bin2d        stat_density2d    stat_quantile     stat_summary_hex
#stat_bindot       stat_ecdf         stat_smooth       stat_unique
#stat_binhex       stat_function     stat_spoke        stat_vline
#stat_boxplot      stat_hline        stat_sum          stat_ydensity



#????ϵͳ
ggplot(small)+geom_bar(aes(x=cut,fill=cut))+coord_flip()
#coord_flip ?????ᷭת
ggplot(small)+geom_bar(aes(x=factor(1),fill=cut))+coord_polar(theta="y")
#??????ת???ɼ?????
ggplot(small)+geom_bar(aes(x=factor(1),fill=cut))+coord_polar()


#ͼ??Ӧ??

#????
ggplot(small, aes(x=carat, y=price))+geom_point(aes(colour=cut))+scale_y_log10() +facet_wrap(~cut)+stat_smooth()
#facet_wrap: ????cut??ͬ?ֲ???ͼ

#????
require(ggthemes)

ggplot(small, aes(x=carat, y=price))+geom_point(aes(colour=cut))+scale_y_log10() +facet_wrap(~cut)+stat_smooth()+theme_solarized()+ggtitle("��Ч")+xlab("??��")+ylab("?۸?")
# ggtitle ???ӱ???  xlab()??X?????? ylab()??Y?????? theme_solarized??ָ??ͼ?????? ??Ҫggthemes??

#??ά?ܶ?ͼ
ggplot(diamonds, aes(carat, price))+ stat_density2d(aes(fill = ..level..), geom="polygon")+ scale_fill_continuous(high='yellow',low='orange')

#????ɢ??ͼ

#密度图
WHO<-read.csv("c:/WHO.csv", header = TRUE)
require(plyr)
#按总人口数排列数据
WHO<-arrange(WHO, desc(D))
#将数据的名字转换为因子，并固定已拍好的country，
#同理可以按照聚类的结果进行排列
WHO<- transform(WHO, Country = factor(Country, levels = unique(Country)))

require(reshape2)
require(ggplot2)
require(scales)
require(grid)
#melt数据
m.WHO <- melt(WHO)
#标准化，每排数据映射到按最小值和最大值映射到(0,1)区间
m.WHO <- ddply(m.WHO, .(variable), transform, rescale = rescale(value))
#标准化并正态化数据
s.WHO <- ddply(m.WHO, .(variable), transform, rescale = scale(value))
require(ggplot2)
p<-ggplot(s.WHO, aes(variable, Country)) +
  #用tile来进行绘热力图
  geom_tile(aes(fill=rescale)) +
  scale_fill_gradient2(mid="black", high="red", low="green", name = "Intensity") +
  labs(x="Country", y="Index", face = "bold") +
  theme_bw() +
  theme(
    axis.title.x=element_text(size=16),
    axis.title.y=element_text(size=16),
    axis.text.x=element_text(size=12, colour="grey50"),
    axis.text.y=element_text(size=12, colour="grey50"),
    legend.title=element_text(size=14),
    legend.text=element_text(size=12),
    legend.key.size = unit(0.8, "cm"))#需要载入grid包来调整legend的大小
p






