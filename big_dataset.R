library(ggplot2)
library(hexbin)

#Setup my preferences for fonts
pwfont = theme(axis.title.x = element_blank()) + theme(axis.text = element_text(size=16)) + theme(axis.title.y=element_text(size=16)) + theme(legend.title = element_text(size=16)) + theme(legend.text = element_text(size=16)) + theme(strip.text.x = element_text(size = 16))

#read the data
s = read.csv("big_dataset.csv",header=TRUE)
# remove rows with missing values
s = s[complete.cases(s),]

#a messay scatterplot
p = ggplot(s,aes(x=MW,y=XLogP))+geom_point() + pwfont + xlab("Molecular Weight")
print(p)

#a messy scatterplot with color
p = ggplot(s,aes(x=MW,y=XLogP,col=target_class))+geom_point() + pwfont + xlab("Molecular Weight") + scale_color_manual(values = bp,name="Target Class", breaks=c("gpcr","kinase","ppi"),labels=c("GPCR","Kinase","PPI"))
print(p)

#Hexagon binning to show data density
p = ggplot(s,aes(x=MW,y=XLogP))+geom_hex() + pwfont + xlab("Molecular Weight")
print(p)

#Comparing a pie chart and a bar chart
par(mfrow=c(1,2))
pie(summary(s$target_class),labels =c("GPCR","Kinase","PPI"),col=brewer.pal(3,"Set1"))
barplot(summary(s$target_class),names.arg=c("GPCR","Kinase","PPI"),col=brewer.pal(3,"Set1"))

# Hexagon binning with pie charts
par(mfrow=c(1,1))
plot(s$MW,s$XLogP,type="n",xlab="Molecular Weight",ylab="XLogP")
hexbinpie(s$MW,s$XLogP,as.factor(s$target_class),pal=brewer.pal(3,"Set1"))
legend("topleft",legend=c("Kinase","GPCR","PPI"),col=brewer.pal(3,"Set1"),pch=15,cex=1.5)


