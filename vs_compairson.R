library("gplots")

# take the matrix that comes out of pairwise.t.test (bottom triangle) and make it symmetric
# there is probably a more elegant way to do this
symmetrizeMatrix <- function(matIn) {
	nRow = nrow(matIn)
	dummy1 = rep(0.0,nRow)
	mat1 = matIn
	mat1[is.na(mat1)] = 0
	mat1 = rbind(dummy1,mat1)
	dummy2 = rep(0.0,nRow+1)
	mat1 = cbind(mat1,dummy2)
	dimnames(mat1)[1][1] = 	dimnames(mat1)[2][1]
	dimnames(mat1)[[1]][nRow+1] = LETTERS[nRow+1]
	dimnames(mat1)[[2]][nRow+1] = LETTERS[nRow+1]
	mat1 = mat1 + t(mat1)
	diag(mat1) = 1.1
	mat1
}

# Plot p-values as a heatmap
plotHeatmap <- function(matIn) {
	noteVals = formatC(matIn,digits = 2,format="f")
	heatmap.2(matIn,dendrogram='none',Rowv = FALSE, Colv = FALSE, trace='none',key=FALSE,cellnote = noteVals,notecol="black",breaks=c(-100,0.05,0.1,1.01,100),col=c("green","yellow","red","white"),lwid=c(0.2,0.8))
}

pwfont = theme(axis.text = element_text(size=16)) + theme(axis.title=element_text(size=16)) + theme(legend.title = element_text(size=16)) + theme(legend.text = element_text(size=16)) + theme(strip.text.x = element_text(size = 16))

s = read.csv("vs_example.csv",header=TRUE)
ss = stack(s)

#Get the mean and vairance
a = ddply(ss,c("ind"),function(x)c(median(x[["values"]]),var(x[["values"]])))
names(a) = c("ind","mean","var")

#Make the bar graph
ggplot(a,aes(x=ind,y=mean)) + geom_bar(stat="identity",fill="lightblue") +ylab("BEDROC") + xlab("Method") + pwfont + theme(axis.title.x=element_text(size=16)) + ylim(c(0,0.6))

#Make the bar graph with error bars
ggplot(a,aes(x=ind,y=mean)) + geom_bar(stat="identity",fill="lightblue") + geom_errorbar(aes(ymin=a$mean-a$var/2,ymax=a$mean+a$var/2))+ylab("BEDROC") + xlab("Method") + pwfont + theme(axis.title.x=element_text(size=16)) + ylim(c(0,0.6))

#Boxplot
 ggplot(ss,aes(x=ind,y=values))+stat_boxplot(geom="errorbar")+geom_boxplot()+xlab("")+ylab("BEDROC") + pwfont

 #Notched boxplot
 ggplot(ss,aes(x=ind,y=values))+stat_boxplot(geom="errorbar")+geom_boxplot(notch=TRUE)+xlab("")+ylab("BEDROC") + pwfont


pWrong = pairwise.t.test(ss$values,ss$ind,method="none",p.adjust.method="none",paired=TRUE)
zWrong = symmetrizeMatrix(pWrong$p.value)
plotHeatmap(zWrong)

pCorrect = pairwise.t.test(ss$values,ss$ind,method="none",paired=TRUE)
zCorrect = symmetrizeMatrix(pCorrect$p.value)
plotHeatmap(zCorrect)

boxColors = cut(zCorrect["H",],c(-100,0.05,0.1,1.01,100),labels=c("green","yellow","red","white"))
ggplot(ss,aes(x=ind,y=values))+stat_boxplot(geom="errorbar")+geom_boxplot(fill=boxColors,notch=TRUE)+xlab("")+ylab("BEDROC") + pwfont


