######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 3: Graphing
######################################################################

x0=c(1,1,3,3,1)
y0=c(1,3,3,1,1)
plot(x0,y0,type="o",pch=12,cex=3,lwd=5,lty=2,xlim=c(0,5),ylim=c(0,5),
	xlab="x-axis",ylab="y-axis",col="blue",main="An Example")
x1=c(2,2,4,4)
y1=c(2,4,4,2)
x2=c(2,4,4,2)
y2=c(4,4,2,2)
segments(x1,y1,x2,y2,lwd=10,col="darkorchid")
curve(2+sqrt(4-(x-2)^2),2,4,col="red",lwd=10,add=TRUE)
p=par("usr")
text(p[1], p[3],"BL",adj=c(0,0),col="darkorchid")
text(p[2], p[4],"RT",adj=c(1,1),col="darkorchid",font=2)
text(mean(p[1:2]),mean(p[3:4]),expression(infinity),cex=3,adj=0.5)

#####

x0=c(1,1,3,3,1)
y0=c(1,3,3,1,1)
par(mar=c(4,4,3,3))
plot(x0,y0,type="o",pch=12,cex=3,lwd=5,lty=2,xlim=c(0,5),ylim=c(0,5),
	xlab="x-axis",ylab="y-axis",col="blue",main="An Example")
mtext("Line 0",side=1,line=0,adj=1,col="red")
mtext("Line 1",side=1,line=1,adj=0,col="red")
mtext("Line 2",side=1,line=2,adj=1,col="red")
mtext("Line 3",side=1,line=3,adj=0,col="red")
box("figure", col="red")
p=par("usr")
text(p[2]+.15,mean(p[3:4]),"The Right Side",srt=-90,xpd=NA,adj=0.5)
mtext("TRS",side=4,line=2,adj=0.5,las=2)

#####

x0=c(1,1,3,3,1)
y0=c(1,3,3,1,1)
par(mar=c(4,4,3,3))
par(oma=c(3,3,3,3))
plot(x0,y0,type="o",pch=12,cex=3,lwd=5,lty=2,xlim=c(0,5),ylim=c(0,5),
	xlab="x-axis",ylab="y-axis",col="blue",main="An Example")
box("figure",col="red")
box("outer",lwd=5,col="blue")
mtext("Line A",side=3,line=0,adj=0,col="blue",outer=TRUE)
mtext("Line B",side=3,line=1,adj=0.5,col="blue",outer=TRUE)
mtext("Line C",side=3,line=2,adj=1,col="blue",outer=TRUE)

##### Graphing Functions #####

f=function(x){sin(x)}
g=function(x){x-x^3/factorial(3)+x^5/factorial(5)}
plot(f,col="blue",xlim=c(-2*pi,2*pi),ylim=c(-1,1),panel.first=grid(),
	xlab="x-axis",ylab="y-axis", main="Function Plot Example")
plot(g,xlim=c(-3.5,3.5),col="red",add=TRUE)

#####

f=function(x){sin(x)}
g=function(x){x-x^3/factorial(3)+x^5/factorial(5)}
par(mar=c(3,3,3,3)+0.2)
plot(f,lwd=2,lty=1,col="blue",xlim=c(-2*pi,2*pi),ylim=c(-1,1),xaxt="n",
	yaxt="n",frame.plot=FALSE,xlab="",ylab="",main="Function Plot Example",
	panel.first=grid(NA,NULL,lty=6,col="gray40"))
plot(g,lwd=2,lty=2,col="red",xlim=c(-3.5,3.5),add=TRUE)
v1=c(-2*pi,-3*pi/2,-pi,-pi/2, 0,pi/2,pi,3*pi/2,2*pi)
v2=c(expression(-2*pi),expression(-3*pi/2),expression(-pi),expression(-pi/2),"",
	expression(pi/2),expression(pi),expression(3*pi/2),expression(2*pi))
axis(side=1,pos=0,lwd=2,labels=v2,at=v1,cex.axis=1)
axis(side=2,pos=0,lwd=2,c(-1,-0.5,0.5,1),cex.axis=1.5,font=2,las=0)
v3=c(-2*pi,-3*pi/2,-pi,-pi/2, pi/2,pi,3*pi/2,2*pi)
abline(v=v3,lty=6,col="gray40")
mtext("x-axis",side=4,line=1,adj=0.5,las=2)
mtext("y-axis",side=1,line=0,adj=0.5,las=0)
text(3,0.25,"f(x)=sin(x)",adj=0,col="blue")
text(4.75,0.85,expression(g(x)==frac(x^5,120)-frac(x^3,6)+x),adj=0.5,col="red")

##### Scatter Plots #####

# This read.table() code isn't in the book, but the example assumes it has been done.
# Note: The Arctic-Ice-Data-R.csv should be downloaded and placed in a folder.
# Change the directory so that it points to the folder with the file.

Ice=read.table("Arctic-Ice-Data-R.csv",header=TRUE,sep=",")

par(bg="#fdf7ef")
plot(Ice$Years.after.1970,Ice$March.Extent.in.MSK,type="p",cex=1.5,pch=10,
	xlim=c(5,48),ylim=c(0,20),xlab="Years After 1970",ylab="",col.lab="purple",
	cex.lab=1.5)
points(Ice$Years.after.1970,Ice$September.Extent.in.MSK,type="p",cex=1.25,pch=7,
	col="red")
title(main="Arctic Ice Extent for\nMarch and September",
	ylab="Million Square Kilometers",col.lab="blue",cex.lab=0.75)
grid (NULL,NULL,lty=6,col="gray")
lvn=c("March","September")
legend("topright",lvn,pch=c(10,7),pt.cex=c(1.5, 1.25),col=c("black","red"),
y.intersp=1.25)
text(42,3.63,"Record Low\n3.63msk",pos=1,cex=0.75)

##### Dot, Pie, and Bar Charts #####

FBM=c("Jun","Nov","Mar","Mar","Aug","Aug","Sep","Sep","Sep","Jul","Aug","Nov",
	"Sep","Jan","Mar","Jan","Dec","Mar","May","Mar","Aug","Apr","Jul","Nov",
	"May","Dec","Mar","Oct","Jan","Sep","May","Feb","Mar","Sep","Dec","May",
	"Aug","Feb","Aug","Sep","Apr","Nov","Mar")
MBM=c(8,2,2,1,1,1,6,3,8,12,5,11,6,1,10,4,7,3,3,6,9,5,4,9,11,12,7,4,3,7,1,8,4,7,
	5,8,6,2,2,4,9,4,4,6,6,2,1,11,4,4,2,9,7,6,1,4,12,7,4,11,10,6,8,11,6)


#####

MBM=c(8,2,2,1,1,1,6,3,8,12,5,11,6,1,10,4,7,3,3,6,9,5,4,9,11,12,7,4,3,7,1,8,4,7,
	5,8,6,2,2,4,9,4,4,6,6,2,1,11,4,4,2,9,7,6,1,4,12,7,4,11,10,6,8,11,6)
Month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
par(oma=c(1,1,1,1))
par(mar=c(4,1,1,1))
stripchart(MBM, method="stack",at=0.05,offset=1.5,pch=19,
	main="Dotplot of Male Birth Months",xlab="Month",ylab="",col="red",axes=FALSE)
axis(side=1,at=1:12,labels=Month)

#####

FBM=c("Jun","Nov","Mar","Mar","Aug","Aug","Sep","Sep","Sep","Jul","Aug","Nov",
	"Sep","Jan","Mar","Jan","Dec","Mar","May","Mar","Aug","Apr","Jul","Nov",
	"May","Dec","Mar","Oct","Jan","Sep","May","Feb","Mar","Sep","Dec","May",
	"Aug","Feb","Aug","Sep","Apr","Nov","Mar")
FBMt=table(FBM)
pie(FBMt)

Ftable=as.data.frame(FBMt)
FMonth=as.character(Ftable[,1])
Fcounts=as.numeric(Ftable[,2])
Fperct=round(Fcounts/sum(Fcounts)*100)
Flabels=paste(FMonth, Fperct)
Flabels=paste(Flabels,"%", sep="")

MBMt=table(MBM)
Mtable=as.data.frame(MBMt)
Month=c("Jan","Feb","Mar","Apr","May","Jun","Jul", "Aug","Sep","Oct","Nov","Dec")
Mcounts=as.numeric(Mtable[,2])
Mperct=round(Mcounts/sum(Mcounts)*100)
Mlabels=paste(Month, Mperct)
Mlabels=paste(Mlabels,"%", sep="")

layout(matrix(c(1,2),1,2,byrow=TRUE))
pie(FBMt,main="Sample of Female Birth Months",labels=Flabels,cex=0.75,col=rainbow(12))
pie(Mcounts,labels=Mlabels,col=gray(c(1:12)/12))
title("Sample of Male Birth Months",line=-2)

#####

## Book error: The first term of pie3D() shoudl be FBMt not FBMCounts
## Orientation of the pie chart will be different from the book.

library(plotrix)
pie3D(FBMt,main="Sample of Female Birth Months",labels=Flabels,
	labelcex=0.75,explode=0.1,col=rainbow(12))

#####

FBM=c("Jun","Nov","Mar","Mar","Aug","Aug","Sep","Sep","Sep","Jul","Aug","Nov",
	"Sep","Jan","Mar","Jan","Dec","Mar","May","Mar","Aug","Apr","Jul","Nov",
	"May","Dec","Mar","Oct","Jan","Sep","May","Feb","Mar","Sep","Dec","May",
	"Aug","Feb","Aug","Sep","Apr","Nov","Mar")
Month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
FBM=replace(FBM,FBM==Month[2],2)
FBM

for(i in c(1:12)){FBM=replace(FBM,FBM==Month[i],i)}
FBM=as.numeric(FBM)
FBMt=table(FBM)
MBMt=table(MBM)

FMtable=rbind(FBMt, MBMt)
Month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
colnames(FMtable)=Month
rownames(FMtable)=c("Female", "Male")
FMtable

barplot(FMtable,main="Sample of Birth Months",xlab="Month",col=c("red","blue"),
	legend.text=TRUE,beside=TRUE)

barplot(FMtable, main="Sample of Birth Months",xlab="Month",col=c("red","blue"),
	legend.text=TRUE,args.legend=list(x="topleft"))


##### A Look at For Loops #####

Month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
FBMCounts=vector(mode="numeric",length=12)
for (i in c(1:12)){FBMCounts[i]=length(which(FBM==Month[i]))}

MBMCounts = vector(mode="numeric", length=12)
for (i in c(1:12)){MBMCounts[i]=length(which(MBM==i))}

##### Boxplot with a Stripchart #####

TenNormFive=rnorm(250,mean=10,sd=5)
TwentyNormTen=rnorm(250,mean=20,sd=10)
Data=list(TenNormFive,TwentyNormTen)
par(mar=c(8,5,3,3))
boxplot(Data,pch=20,notch=TRUE,las=2,at=c(1,2),names=c("Norm(10,5)","Norm(20,10)"),
	col=c("blue","red"),main="Boxplot Example",ylab="Value")
stripchart(Data,vertical=TRUE,method="jitter",jitter=0.1,add=TRUE,pch=1,
	col=c("gray50","gray50"))
mtext("Randomly Generated Normal Data",side=1,line=6)

##### Histogram #####

WeibullData=rweibull(1000, shape=10, scale = 1)
hist(WeibullData,breaks=15,col="red",prob=TRUE,
	main="1000 Random Weibull Distribution Values",xlab="")
m=mean(WeibullData)
s=sqrt(var(WeibullData))
curve(dnorm(x,m,s),col="blue",lwd=2,add=TRUE)













