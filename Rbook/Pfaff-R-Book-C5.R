######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 5: Sequences, Series, and Limits
######################################################################

options(scipen=9)
n=39:42
n=n^3+5
n=1/n
n


a=1:9
b=1:3
a+b
a^b

##### Sequences and Series #####

nr=100:1/100
nl=-100:-1/100
f=function(n){1/n}
par(mar=c(4,6,4,4))
plot(nr,f(nr),col="red",xlim=c(-1,1),ylim=c(-100,100),xlab="Value of n",
	ylab=expression(frac(1,n)))
points(nl,f(nl),col="blue")
abline(v=0,lty=2,col="gray40")


l=1
u=1000
n1=l:u
s1=1/n1
n2=(l:u)^2
s2=1/n2
miny=min(min(s1),min(s2))
maxy=max(max(s1),max(s2))
plot(n1,s1,pch=1,xlim=c(l,u),ylim=c(miny,maxy),xlab="n",ylab="Sequence Values")
points(n1,s2,pch=17,col="red")
legend("topright",c(expression(frac(1,n)),expression(frac(1,n^2))),
	pch=c(1,17),pt.cex=c(1.5,1.25),col=c("black","red"),y.intersp=1.25)
abline(h=0,lwd=2)


l2=900
u2=1000
miny2=min(min(s1[c(l2:u2)]),min(s2[c(l2:u2)]))
maxy2=max(max(s1[c(l2:u2)]),min(s2[c(l2:u2)]))
options(scipen=9)
plot(n1[c(l2:u2)],s1[c(l2:u2)],pch=1,xlim=c(l2,u2),ylim=c(miny2,maxy2),
	xlab="n",ylab="Sequence Values")
points(n1[c(l2:u2)],s2[c(l2:u2)],pch=17,col="red")
legend(990,0.0009,c(expression(frac(1,n)),expression(frac(1,n^2))),
	pch=c(1,17),pt.cex=c(1.5,1.25),col=c("black","red"),y.intersp=1.25)
abline(h=0,lwd=2)


l2=900
u2=1000
ps1=cumsum(s1)
ps2=cumsum(s2)
miny3=min(min(ps1[c(l2:u2)]),min(ps2[c(l2:u2)]))
maxy3=max(max(ps1[c(l2:u2)]),min(ps2[c(l2:u2)]))
plot(n1[c(l2:u2)],ps1[c(l2:u2)],pch=1,xlim=c(l2,u2),ylim=c(miny3,maxy3),
	xlab="n",ylab="Sequence of Partial Sums Values")
points(n1[c(l2:u2)],ps2[c(l2:u2)],pch=17,col="red")
legend(990,7,c(expression(frac(1,n)),expression(frac(1,n^2))),pch=c(1,17),
	pt.cex=c(1.5,1.25),col=c("black","red"),y.intersp=1.25)

##### The Derivative as a Limit #####

hp=100:1/100
hm=-100:-1/100
D=function(h){((1+h)^2-1)/h}
par(mar=c(4,6,4,4))
plot(hp,D(hp),col="red",xlim=c(-1,1),ylim=c(0,3),xlab="Value of h",
	ylab=expression(frac((1+h)^2-1,h)))
points(hm,D(hm),col="blue")


hp=1000:1/1000
hm=-1000:-1/1000
D=function(h){((1+h)^3-1)/h}
par(mar=c(4,6,4,4))
plot(hp,D(hp),col="red",xlim=c(-1,1),ylim=c(0,7),xlab="Value of h",
	ylab=expression(frac((1+h)^3-1,h)))
points(hm,D(hm),col="blue")


hp=50:1/100
hm=-50:-1/100
D=function(a,h){(exp(a+h)-exp(a))/h}
par(mar=c(4,6,4,4))
plot(hp,D(2,hp),col="red",xlim=c(-0.5,0.5),ylim=c(0,13),xlab="Value of h",
	ylab=expression(frac(e^(2+h)-e^(2),h)))
points(hm,D(2,hm),col="blue")
points(0,exp(2),pch=18,cex=1.75)


xr=1+ 20:1/20
xl=1- 20:1/20
f=function(x){(x^2-1)/(x-1)}
par(mar=c(4,6,4,4))
plot(xr,f(xr),col="red",xlim=c(0,2),ylim=c(0,3),xlab="Value of x",
	ylab=expression(frac(x^2-1,x-1)))
points(xl,f(xl),col="blue")
abline(v=1,lty=2,col="gray40")

##### Recursive Sequences #####

n=20
FibSeq=numeric(n)
FibSeq[1]=1
FibSeq[2]=1
for (i in 3:n) {FibSeq[i]=FibSeq[i-1]+FibSeq[i-2]}
FibSeq


list=1:n
FibTable=rbind(list,FibSeq)
FibTable


FibTable2=matrix(FibSeq,n,1)
FibTable2


FibFun=function(n) {
 if (n==1 | n == 2) return (1)
 else return (FibFun(n-1)+FibFun(n-2))
 }
FibFun(30)


seq(0,3, length.out=5)
