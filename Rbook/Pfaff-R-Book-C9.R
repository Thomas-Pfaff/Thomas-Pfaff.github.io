######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 9: Curve Fitting
######################################################################


##### Exponential Fit #####

Years.after.2000=4:16
Solar.usa=c(119,198,303,463,761,1190,2040,3961,7330,12106,18317,25600,40436)
plot(Years.after.2000,Solar.usa,pch=16,cex=1.25,xlim=c(4,16),
	xlab="Years after 2000", ylab="Megawatts")
title(main="Cumulative Installed Photovoltaic\nPower for the U.S.")
grid(NULL,NULL,lty=6,col="gray")
Solar.usa.fit=lm(log(Solar.usa)~ Years.after.2000)
Exp.Solar.usa=function(x){exp(coef(Solar.usa.fit)[[1]])*exp(coef(Solar.usa.fit)[[2]]*x)}
curve(Exp.Solar.usa,4,16,lwd=2,add=TRUE)


summary(Solar.usa.fit)


names(Solar.usa.fit)


Solar.usa.fit$coefficients


Solar.usa.nls=nls(Solar.usa~P*exp(r*Years.after.2000),start=list(P=1,r=1))
Exp.Solar.usa.nls=function(x){coef(Solar.usa.nls)[[1]]*exp(coef(Solar.usa.nls)[[2]]*x)}
curve(Exp.Solar.usa.nls,4,16,add=TRUE,col="red",lwd=2)


##### Polynomial Fit #####


Month=1:12
Arctic.Ice.2017=c(13.17,14.11,14.27,13.76,12.62,10.72,7.9,5.47,4.8,6.71,9.46,11.75)
plot(Month,Arctic.Ice.2017,pch=16,cex=1.25,ylim=c(0,15),xlab="Month",
	ylab="Million Square Kilometers")
title(main="2017 Arctic Ice Extent")
grid (NULL,NULL,lty=6,col="gray")
Ice.fit=lm(Arctic.Ice.2017~poly(Month,6,raw=TRUE))
Coef=Ice.fit$coefficients
Poly.Ice=function(x){Coef[[1]]+Coef[[2]]*x+Coef[[3]]*x^2+Coef[[4]]*x^3+Coef[[5]]*x^4+
	Coef[[6]]*x^5+Coef[[7]]*x^6}
curve(Poly.Ice,1,12,lwd=2,add=TRUE)


##### Log Fit #####


x=3:100
y=log(x) + rnorm(length(x),0,0.25)
plot(x,y)
Log.fit=lm(y~log(x))
Log.fit.function=function(x){coef(Log.fit)[2]*log(x)+coef(Log.fit)[1]}
curve(Log.fit.function,0,100,lwd=2,add=TRUE)


plot(x,y,log="x")


##### Logistic Fit #####


Years.After.1990=2:26
Solar.Ger=c(2.9,4.3,5.6,6.7,10.3,16.5,21.9,30.2,103.4,222.5,343.6,496.0,
	1165.4,2100.6,2950.4,4230.1,6193.1,10538.1,17956.4,25441.6,33045.6,
	36349.9,38249.9,39710.0,41186.0)
plot(Years.After.1990,Solar.Ger,pch=10,cex=1.25,xlab="Years after 1990",ylab="Megawatts")
title(main="Cumulative Installed Photovoltaic\nPower for Germany")
grid(NULL,NULL,lty=6,col="gray")
Ger.logistic=nls(Solar.Ger~SSlogis(Years.After.1990,Asym,xmid,scal))
Ger.logistic.function=function(x){coef(Ger.logistic)[[1]]/(1+exp((coef(Ger.logistic)
	[[2]]-x)/coef(Ger.logistic)[[3]]))}
curve(Ger.logistic.function,2, 26,lwd=2,add=TRUE)


##### Power Fit #####

## Book error: corrected ) in paste for title

x=1:50
error=0.05
par(mfrow=c(2,2))
for (r in c(-.2, 2.5, 0.5, -1.5)){
 c=sample(5:15,1)
 y=x^r+rnorm(length(x),0,error)+c
 Power.fit=nls(y ~ I(x^power)+c,start=list(power=1,c=10))
 f=function(x){x^coef(Power.fit)[[1]]+coef(Power.fit)[[2]]}
 title=paste("Data: x^(",r,")+N(0,", error,")+",c)
 xlabel=paste("nls() estimates: r= ",round(coef(Power.fit)[[1]],2),", constant=",
	round(coef(Power.fit)[[2]],2),sep="")
 plot(x,y,main=title,xlab=xlabel)
 curve(f,1,50,add=TRUE)}
