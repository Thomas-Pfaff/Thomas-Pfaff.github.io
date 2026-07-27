######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 15: Nonparametric Statistical Tests
######################################################################


## Note: Seed is set so results will no vary.


set.seed(41)
x1=runif(20,0,10)
x2=runif(20,0,10)
x3=runif(20,0,5)
x1


Data=list(x1,x2,x3)
par(mar=c(4,3,3,3))
boxplot(Data,at=c(1,2,3),names=c("U(0,10)","U(0,10)","U(0,5)"),lwd=2,
	border=c("blue","red","purple"))
stripchart(Data,vertical=TRUE,method="jitter",jitter=0.05,add=TRUE,
	pch=c(1,16,18),col=c("black","gray40","gray20"))
mtext("Randomly Generated Uniform Data",side=1,line=3)


##### Wilcoxon Signed Rank Test for a Single Population #####


result=wilcox.test(x1,mu=5,alternative="greater",conf.int=TRUE,conf.level=0.90)
result


names(result)


##### Wilcoxon Rank Sum Test for Independent Groups #####


wilcox.test(x1,x2,conf.int=TRUE)


Data.Stacked.1=data.frame("data"=c(x1,x2),"factor"=c(rep("a",times=length(x1)),
	rep("b",times=length(x2))))
Data.Stacked.1

##### Wilcoxon Signed Rank Test for Dependent Data #####


wilcox.test(x1,x3,paired=TRUE,conf.int=TRUE)


##### Spearman's Rank Correlation Coefficient #####


cor.test(x1,x2,method="spearman")


##### Kruskal-Wallis One-Way Analysis of Variance #####


Data.Stacked.2=data.frame("data"=c(x1,x2,x3),"factor"=c(rep("a",
	times=length(x1)),rep("b",times=length(x2)),rep("c",times=length(x3))))
Data.Stacked.2


kruskal.test(Data.Stacked.2$data~Data.Stacked.2$factor)

