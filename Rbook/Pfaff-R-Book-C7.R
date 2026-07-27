######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 7: Riemann Sums and Integration
######################################################################

f=function(x){(5+exp(x)+(2.5)^x*sin(2*pi*x))/3000}
dx=(11-5)/19
mid=seq(5+dx/2,11,dx)
sum(f(mid)*dx)


##### Riemann Boxes #####


f=function(x){5+x^2}
par(mar=c(4,5,2,2))
curve(f,0,4,ylim=c(0,25),yaxs="i",xaxs="i",ylab=expression(f(x)==5+x^2))
segments(0,0,0,f(0),lwd=2)
segments(0,f(0),1,f(0),lwd=2)
segments(1,f(0),1,0,lwd=2)
segments(1,0,1,f(1),lwd=2)
segments(1,f(1),2,f(1),lwd=2)
segments(2,f(1),2,0,lwd=2)
segments(2,0,2,f(2),lwd=2)
segments(2,f(2),3,f(2),lwd=2)
segments(3,f(2),3,0,lwd=2)
segments(3,0,3,f(3),lwd=2)
segments(3,f(3),4,f(3),lwd=2)
segments(4,f(3),4,0,lwd=2)


f=function(x){5+x^2}
a=0
b=4
n=11
dx= (b-a)/n
mid=seq(a+dx/2, b, dx)
par(mar=c(4,5,2,2))
curve(f,a,b,ylim=c(0,25),lwd=2,yaxs="i",xaxs="i",ylab=expression(f(x)==5+x^2))
for(i in 1:n){
 mid.box.x=c(mid[i]-dx/2,mid[i]-dx/2,mid[i]+dx/2,mid[i]+dx/2)
 mid.box.y=c(0, f(mid[i]), f(mid[i]), 0)
 polygon(mid.box.x,mid.box.y,col="gray85",border="black") }
curve(f,a,b,lty=3,lwd=2,add=TRUE)
points(mid,0*mid,col="red",pch=16)
p=par("usr")
text(p[1], p[4],paste("Sum=",sum(f(mid)*dx)),adj=c(0,1))


MidBox=function(a,b,n,f){
 dx=(b-a)/n
 mid=seq(a+dx/2, b, dx)
 par(mar=c(4,4,2,2))
 curve(f,a,b,lwd=2,ylab="function")
 for(i in 1:n){
 polygon(c(mid[i]-dx/2,mid[i]-dx/2,mid[i]+dx/2,mid[i]+dx/2),c(0,f(mid[i]),
	f(mid[i]),0),col="gray85",border="black") }
 curve(f,a,b,lty=3,lwd=2,add=TRUE)
 p=par("usr")
 text(p[1]+.1, p[4]-.2,paste("Sum=",sum(f(mid)*dx)),adj=c(0,1)) }
h=function(x){(5+exp(x)+(2.5)^x*sin(2*pi*x))/3000}
MidBox(5,11,20,h)


##### Numerical Integration #####

f=function(x){x^2}
integrate(f,0,2)


integrate(sin,0, 2*pi)


g=function(x){exp(-x)}
integrate(g,0,1)


integrate(g,0,Inf)


integrate(g,-Inf,Inf)


g=function(x){exp(-x)}
integrate(g,0,20000)


##### Numerical Integration of Iterated Integrals #####


y=0:10
sapply(y,function(y){y^2})


y=0:10/10
sapply(y,function(y){integrate(function(x){x*y},0,2)$value})


integrate(function(z){sapply(z, function(y) {
 integrate(function(x){x*y},0,2)$value
  })},0,1)


integrate(function(z){sapply(z,function(y){
 integrate(function(x){4+x-y},1,2)$value
  })},1,3)


integrate(function(z){sapply(z,function(y){
 integrate(function(x){x*exp(y)*sin(x)},0,pi/2)$value
 })},-1, 1)


##### Area Between Two Curves #####

library(rootSolve)
f=function(x){(10*x^2-1)*exp(-x)}
g=function(x){x^3}
h=function(x){f(x)-g(x)}
roots=uniroot.all(h,c(-1,2))
roots


integrate(h,roots[1],roots[2])
integrate(h,roots[2],roots[3])
integrate(h,roots[2],roots[3])$value-integrate(h,roots[1],roots[2])$value


par(mar=c(4,4,3,3))
plot(f,xlim=c(-.5,2),lwd=2,xlab="",ylab="")
curve(g,-.5,3,lwd=2,col="red",add=TRUE)
legend("topleft",c(expression(f(x)==(10*x^2-1)*e^-x),expression(g(x)==x^3)),
	lty=c(1,1),lwd=c(2,2),col=c("black","red"))
xlist1=seq(roots[1],roots[2],0.01)
xlist2=seq(roots[2],roots[3],0.01)
polygon(c(xlist1,rev(xlist1)),c(g(xlist1),f(rev(xlist1))),col="gray65" )
polygon(c(xlist2,rev(xlist2)),c(f(xlist2),g(rev(xlist2))),col="gray85")
curve(g,-.5,2,lwd=3,col="red",add=TRUE)
curve(f,-.5,2,lwd=3,col="black",add=TRUE)
text(0,-0.25,paste("Area=",-round(integrate(h,roots[1],roots[2])$value,2)))
text(1,2,paste("Area=",round(integrate(h,roots[2],roots[3])$value,2)))


##### Graphing an Antiderivative #####


f=function(x){cos(x)}
g=function(x){integrate(f,0,x)$value}
h=Vectorize(g)
par(mar=c(4,4,3,3))
plot(f ,xlim=c(-2*pi,2*pi),ylim=c(-1,1.25),lwd=2,xlab="",ylab="")
curve(h(x),-2*pi ,2*pi,lwd=2,col="red",add=TRUE)
abline(h=0,lwd=1)
legend("topleft",lty=c(1,1),lwd=c(2,2),c(expression(f(x)==cos(x)),
	paste("Integral of",expression(cos(x)))),col=c("black","red"))