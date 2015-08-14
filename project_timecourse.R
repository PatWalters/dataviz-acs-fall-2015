library(beeswarm)
library(ggplot2)
library(ggthemes)

#Setup my preferences for fonts
pwfont = theme(axis.title.x = element_blank()) + theme(axis.text = element_text(size=16)) + theme(axis.title.y=element_text(size=16)) + theme(legend.title = element_text(size=16)) + theme(legend.text = element_text(size=16)) + theme(strip.text.x = element_text(size = 16))

# read the data 
d = read.csv("CACO_sample.csv")
# make sure the data field is the appropriate data type
d$Date = as.Date(d$Date,"%Y-%m-%d")
# bin the LogP Data
d$LogPBin = cut(d$CLogP,breaks=c(-100,3,100),labels=c("<=3",">3"))

#Make a scatterplot of CACO-2 A-B vs time
scatterPlot = ggplot(d,aes(x=Date,y=Value,col=LogPBin))+geom_point(size=5) + ylab("CACO-2 A-B (1hr)") + scale_color_manual(name="CLogP",values=c("<=3"="blue",">3"="red")) +  pwfont
print(scatterPlot)

#plot a scatterplot of CACO-2 A-B vs CLogP faceted by quarter as a single row
facetGrid = ggplot(d,aes(x=CLogP,y=Value)) + geom_point(size=3) + ylab("Caco-2 A-B (1hr)") + facet_grid(~Label) + pwfont
print(facetGrid)

#plot a scatterplot of CACO-2 A-B vs CLogP faceted by quarter as a multi-row grid
facetWrap = ggplot(d,aes(x=CLogP,y=Value)) + geom_point(size=3) + ylab("Caco-2 A-B (1hr)") + facet_wrap(~Label) + pwfont 
print(facetWrap)

#setup the points for the beeswarm plot
bs = beeswarm(d$Value~d$QY,do.plot=FALSE)
# transfer the points for the beeswarm plot to dataframe d
d$x = bs$x
d$y = bs$y

# simple boxplot
simpleBoxplot = ggplot(d,aes(x=Label,y=Value)) + stat_boxplot(geom ='errorbar') + geom_boxplot(notch=FALSE,outlier.size=3) + theme(strip.text.x = element_text(size = 16)) + pwfont + ylab("Caco-2 A-B (1hr)") + xlab("CLogP")
print(simpleBoxplot)

#beeswarm plot, not that the trick here is to do geom_tufteboxplot with boxwidth=0 and fatten = 0
beeswarmPlot = ggplot(d,aes(x=Label,y=Value)) + geom_tufteboxplot(median.type="box",color="white",fill="white",boxwidth=0,fatten=0) + theme(strip.text.x = element_text(size = 16)) + pwfont + ylab("Caco-2 A-B (1hr)") + xlab("CLogP") + geom_point(aes(x=x,y=y,col=LogPBin,alpha=1),size=4) + scale_color_manual(name="CLogP",values=c("<=3"="blue",">3"="red"))+guides(alpha=FALSE)
print(beeswarmPlot)

#beeswarm plot + boxplot
beeswarmBoxplot = ggplot(d,aes(x=Label,y=Value)) + stat_boxplot(geom="errorbar")+ geom_boxplot(outlier.size=0) + theme(strip.text.x = element_text(size = 16)) + pwfont + ylab("Caco-2 A-B (1hr)") + xlab("CLogP") + geom_point(aes(x=x,y=y,col=LogPBin,alpha=1),size=4) + scale_color_manual(name="CLogP",values=c("<=3"="blue",">3"="red"))+guides(alpha=FALSE) 
print(beeswarmBoxplot)

#beeswarm plot + Tufte boxplot
beeswarmTufte = ggplot(d,aes(x=Label,y=Value)) + geom_tufteboxplot(median.type="box",color="white",fill="white") + theme(strip.text.x = element_text(size = 16)) + pwfont + ylab("Caco-2 A-B (1hr)") + xlab("CLogP") + geom_point(aes(x=x,y=y,col=LogPBin,alpha=1),size=4) + scale_color_manual(name="CLogP",values=c("<=3"="blue",">3"="red"))+guides(alpha=FALSE)
print(beeswarmTufte)



