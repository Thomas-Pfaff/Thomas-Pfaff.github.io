######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 14: Linear Regression
######################################################################


Ice=read.table("Arctic-Ice-Data-R.csv",header=TRUE,sep=",")


linear.model.M=lm(Ice$March.Extent.in.MSK ~Ice$Years.after.1970)
summary(linear.model.M)

linear.model.S=lm(Ice$September.Extent.in.MSK ~Ice$Years.after.1970)


par(bg = "#fdf7ef")
plot(Ice$Years.after.1970,Ice$March.Extent.in.MSK,type="p",cex=1.5,
	pch=10,xlim=c(5,48),ylim=c(0,20),xlab="Years After 1970",ylab="",
	col.lab="purple",cex.lab=1.5)
points(Ice$Years.after.1970,Ice$September.Extent.in.MSK,type="p",cex=1.25,pch=7,col="red")
title(main="Arctic Ice Extent for\nMarch and September",ylab="Million Square Kilometers",
	col.lab="blue",cex.lab=0.75)
grid(NULL,NULL,lty=6,col="gray")
v3=c("March","September")
legend("topright",v3,pch=c(10,7),pt.cex=c(1.5,1.25),col=c("black","red"),y.intersp=1.25)
text(42,3.63,"Record Low\n3.63msk",pos=1,cex=0.75)
abline(linear.model.M, lwd=3)
abline(linear.model.S, lwd=3, col="red")

linear.model.M$coefficients[[1]]


March.Line=function(x){linear.model.M$coefficients[[2]]*x+linear.model.M$coefficients[[1]]}
March.Line(30)


confint(linear.model.M,"Ice$Years.after.1970",level=0.95)


x=Ice$Years.after.1970
linear.model.M2=lm(Ice$March.Extent.in.MSK ~ x)
new=data.frame(x=c(41,48))
predict(linear.model.M2,new,interval="confidence",level=0.90)


cbind(new,predict(linear.model.M2,new,interval="prediction",level=0.90))


par(mfrow=c(2,2))
plot(linear.model.S)


par(mfrow=c(2,1))
plot(linear.model.M$residuals,pch=16,col="blue",ylab="March Residuals")
abline(h=0)
plot(Ice$Years.after.1970,linear.model.S$residuals,pch=16,col="red",ylab="Sept Residuals")
abline(h=0)


par(mfrow=c(1,2))
hist(linear.model.M$residuals,ylim=c(0,1.6),prob=TRUE,col="lightblue", xlab="March")
mM=mean(linear.model.M$residuals)
sM=sqrt(var(linear.model.M$residuals))
curve(dnorm(x,mM,sM),col="blue",lwd=2,add=TRUE)
hist(linear.model.S$residuals,prob=TRUE,xlab="September",main="September Residuals")
mS=mean(linear.model.S$residuals)
sS=sqrt(var(linear.model.S$residuals))
curve(dnorm(x,mS,sS),col="red",lwd=2,add=TRUE)


##### Multiple Linear Regression #####


mult.linear.model=lm(Ice$September.Extent.in.MSK ~ 
	Ice$Years.after.1970+Ice$March.Extent.in.MSK + Ice$June.Extent.in.MSK)
summary(mult.linear.model)


x1=Ice$Years.after.1970
x2=Ice$March.Extent.in.MSK
x3=Ice$June.Extent.in.MSK
mult.linear.model2=lm(Ice$September.Extent.in.MSK~ x1+x2+x3)
newMult = data.frame(x1=c(41,46),x2=c(14.55,14.40),x3=c(10.75,10.41))
cbind(newMult,predict(mult.linear.model2,newMult,interval="confidence", level = 0.90))
