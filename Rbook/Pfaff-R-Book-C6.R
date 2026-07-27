######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 6: Calculating Derivatives
######################################################################

library(numDeriv)
f=function(x){x^2}
g=function(x){exp(x)}
grad(f,2)


grad(g,1)


grad(cos, pi/2)


options(digits=20)
grad(cos, pi/2)


##### Symbolic Differentiation #####

library(Deriv)
f=function(x){7*x^2+11*x+42}
fp=Deriv(f)
fp


fpp=Deriv(fp)
fpp


g=function(x){sin(a*x)}
Deriv(g,"x")


gp9=Deriv(g,"x", nderiv=9)
gp9


h=function(x){exp(x^2)*cos(5*x)}
hp=Deriv(h)
hp


options(digits=20)
hp(5)


k=function(x){x*log(x)/(x^2+d)}
kp=Deriv(k)
kp


m=function(x,y){x*exp(y^2)+ y*log(x) + 3*x}
m.xp=Deriv(m,"x")
m.xp


Deriv(m.xp,"y")


f=function(x){sin(x)}
f.np=function(n){Deriv(f,nderiv=n)}
f.np(5)


f.np(5)(pi)


##### Finding Maximum, Minimum, and Inflection Points #####

# NOTE: You don't need to and shouldn't keep running library(Deriv).
# Once per session is all that is needed.

library(Deriv)
library(rootSolve)
f=function(x){(x^2-1)*exp(-x)}
fp=Deriv(f)
fpp=Deriv(fp)
First.D.roots=uniroot.all(fp, c(-10,10))
First.D.roots


Sec.D.roots=uniroot.all(fpp, c(-10,10))
Sec.D.roots


par(mar=c(4,5,3,3))
plot(f,xlim=c(-1,4),ylab=expression((x^2-1)*e^-x))
xValues=c(First.D.roots, Sec.D.roots)
points(xValues,f(xValues),col="red",cex=1.25,pch=16)


##### Graphing a Function and Its Derivative #####

# NOTE: You don't need to and shouldn't keep running library(Deriv).
# Once per session is all that is needed.

library(Deriv)
f=function(x){x+x*sin(4*x)-1}
fp=Deriv(f)
par(mar=c(4,5,3,5))
plot(f,xlim=c(0,4.5),ylim=c(-2,8),lwd=2,col="blue",xlab="",yaxs="i",
	xaxs="i",ylab=expression(f(x)==x+x*sin(4*x)-1),cex.lab=1.25,
	col.lab="blue")
axis(2,col="blue")
abline(h=0,col="blue")
par(new=T)
plot(fp,xlim=c(0,4.5),ylim=c(-15,15),col="red",lty=2,xlab="",ylab="",
	lwd=2,yaxs="i",xaxs="i",axes=F)
axis(4,col="red")
abline(h=0,lty=2,col="red")
p=par("usr")
text(p[2]+.7,mean(p[3:4]),expression(f^{"'"}*(x)),srt=-90,xpd=NA,adj=0.5,
	cex=1.25,col="red")


##### Graphing a Function with Tangent Lines #####

# NOTE: You don't need to and shouldn't keep running library(Deriv).
# Once per session is all that is needed.

library(Deriv)
f=function(x){x^2}
fp=Deriv(f)
f.tl=function(x,a){fp(a)*(x-a)+f(a)}
palette=colorRampPalette(c("steelblue1","royalblue4"))
colors=palette(5)
plot(f,xlim=c(-10,10),ylab=expression(x^2))
for(i in -2:2){curve(f.tl(x,3*i),lwd=2,col=colors[i+3],lty=i+3,add=TRUE)}


##### Shading the Normal Density Curve Outside the Inflection Points #####

# NOTE: You don't need to and shouldn't keep running library(Deriv) or librayr(rootSolve)..
# Once per session is all that is needed.

library(Deriv)
library(rootSolve)
Norm.u.s=function(x,u,s){ 1/sqrt(2*pi*s^2)*exp(-(x-u)^2/(2*s^2))}
Norm.u.s.pp=Deriv(Norm.u.s,"x",nderiv=2)
NormInflFix.u.s=function(x){Norm.u.s.pp(x,0,2)}
roots=uniroot.all(NormInflFix.u.s,c(-5,5))
roots


curve(Norm.u.s(x,0,2),-7,7,ylab="N(0,2) Density")
points(roots,Norm.u.s(roots,0,2),col="red",cex=1.25,pch=16)
xlist=seq(-7,-2,0.01)
polygon(c(xlist,rev(xlist)),c(Norm.u.s(xlist,0,2),0*rev(xlist)),
	col="gray85",border="black")
polygon(c(-xlist,rev(-xlist)),c(Norm.u.s(xlist,0,2),0*rev(xlist)),
	col="gray85",border="black")
