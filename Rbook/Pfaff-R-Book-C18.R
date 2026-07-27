######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 18:  Differential Equations
######################################################################


##### Newton's Law of Cooling #####


library(deSolve)
Cooling.DE=function(t,y,parameters){
 k=parameters
 dy.dt= -k*(y-70)
 list(dy.dt)}
y.0=212
t=seq(0,60,0.01)
parameters=c(k=0.10)
Cooling.DE.Results=ode(y=y.0,times=t,func=Cooling.DE,parms=parameters)
head(Cooling.DE.Results,n=10)


plot(Cooling.DE.Results,lwd=2,main="",xlab="Minutes",
	ylab=expression(paste("Temperature in", ~~degree,"F")))

## Note: I've added this line. There seems to be inconsistencies on column names 
## of the output. This assures that the columns are labelled time and y.
## You may not need this extra line of code.

colnames(Cooling.DE.Results)=c("time", "y")


Cooling.DE.Results.df=as.data.frame(Cooling.DE.Results)
Cooling.DE.Results.df[Cooling.DE.Results.df$y>135.5 &Cooling.DE.Results.df$y<136.5, ]


library(phaseR)
k=0.1
Cooling.Flow=flowField(Cooling.DE,xlim=c(0,60),ylim=c(0,150),
	main="Cooling Flow Field",xlab="Minutes",
	ylab=expression(paste("Temperature in", ~~degree,"F")),
	parameters=k,points=15,col="black",system="one.dim",add=FALSE)
Cooling.Traj=trajectory(Cooling.DE,y0=c(20,150),tlim=c(0,60),lwd=2,
	col="red",parameters=k,system="one.dim")
Cooling.Null=nullclines(Cooling.DE,xlim=c(0,60),ylim=c(0,200),lwd=2,
	parameters=k,system="one.dim")


##### The Logistic Equation #####

## Reminder: deSolve and phaseR need to be loaded for the following code.

Logistic.DE=function(t, y, parameters){
 k=parameters[1]
 r=parameters[2]
 dy.dt= r*y*(1-y/k)
 list(dy.dt) }
t=seq(0,100,0.01)
y.0=25
parameters=c(k=100,r=0.1)
Logistic.DE.results=ode(y=y.0,times=t,func=Logistic.DE,parms=parameters)
plot(Logistic.DE.results,ylim=c(0,150),lwd=2,main="Logistic DE",xlab="Days",
	ylab="Population")
abline(h=100,lty=2,lwd=2)


library(phaseR)
plot(Logistic.DE.results,type="n",ylim=c(0,150),xaxs="i",yaxs="i",
	lwd=2,main="Logistic DE",xlab="Days",ylab="Population")
grid(NULL,NULL,col="black")
abline(h=100,lty=2,lwd=2)
colors=rainbow(6,start=3/6,end=6/6)
j=0
for (i in c(5,10,20,40,80,160)){
 j=j+1
 Logistic.DE.results=ode(y=i,times=t,func=Logistic.DE,parms=parameters)
 lines(Logistic.DE.results,lwd=2,col=colors[j])
 text(80,50-4*j,paste("N(0)=",i,sep=""),cex=0.75,font=2,col=colors[j],adj=0) }


k=100
r=0.1
Logistic.Flow=flowField(Logistic.DE,xlim=c(0,100),ylim=c(0,150),
	main="Logistic DE",xlab="Days",ylab="Population",parameters=c(k,r),
	points=15,system="one.dim",col="black",add=FALSE)
Logistic.Traj=trajectory(Logistic.DE,y0=c(25,75,150),tlim=c(0,100),lwd=2,
	col="red",parameters=c(k,r),system="one.dim")
Logistic.Null=nullclines(Logistic.DE,xlim=c(0,100),ylim=c(0,150),parameters=c(k,r),
	lwd=2,system="one.dim")


Logistic.Eq=findEquilibrium(Logistic.DE,y0=90,parameters=c(k,r),system="one.dim")


names(Logistic.Eq)


Logistic.Stability=stability(Logistic.DE,ystar=100,parameters=c(k,r),system="one.dim")


##### Predator-Prey Model #####

## Reminder: deSolve and phaseR need to be loaded for the following code.

PredPrey.DE=function(t,y,parameters) {
 a=parameters[1]
 b=parameters[2]
 c=parameters[3]
 d=parameters[4]
 dy = numeric(2)
 dy[1]=a*y[1]-b*y[1]*y[2]
 dy[2]=c*y[1]*y[2]-d*y[2]
 return(list(dy))
 }
y.0=c(y1=2,y2=2)
t=seq(0,100,0.01)
parameters=c(a=20,b=5,c=2,d=10)
PredPrey.DE.result=ode(y=y.0,times=t,func=PredPrey.DE,parms=parameters)
head(PredPrey.DE.result,n=10)


plot(PredPrey.DE.result[,1],PredPrey.DE.result[,2],type="l",xaxs="i",
	yaxs="i",xlim=c(0,5),ylim=c(0,15),lwd=2,main="Predator Prey Model",
	xlab="Time",ylab="Population")
grid(NULL,NULL,col="black")
lines(PredPrey.DE.result[,1],PredPrey.DE.result[,3],lwd=2,col="red")


a=20
b=5
c=2
d=10
PredPrey.Flow=flowField(PredPrey.DE,xlim=c(0,15),ylim=c(0,10),
	main="Predator Prey Model",xlab="Time",ylab="Population",
	parameters=c(a,b,c,d),points=15,system="two.dim",col="black",add=FALSE)
y.ini=matrix(c(4,4,3,9,9,6),ncol=2,byrow=TRUE)
PredPrey.Traj=trajectory(PredPrey.DE,y0=y.ini,tlim=c(0,1),lwd=2,col="red",
parameters=c(a,b,c,d),system="two.dim",cex=2)


PredPrey.Eq=findEquilibrium(PredPrey.DE,y0=c(4,3),parameters=c(a,b,c,d),
	system="two.dim",plot.it=TRUE)


names(PredPrey.Eq)


PredPrey.Stability=stability(PredPrey.DE,ystar=c(5,4),parameters=c(a,b,c,d),
	system ="two.dim")

