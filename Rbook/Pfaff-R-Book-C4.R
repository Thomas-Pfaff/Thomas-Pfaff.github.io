######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 4: Polynomials
######################################################################

library(PolynomF)
p1=poly.from.zeros(-2:4)
p1

p2=poly.from.zeros(c(-1,1))
p2

p3=polynom(c(1,2,0,4))
p3

x=polynom()
p4=5*(x-4)^3 + 10*x^2 - 5
p4

# result will vary due to randomness
rand.int=sample(0:10,5,replace=TRUE)
p5=poly.from.zeros(rand.int)
p5

# result will vary due to randomness
p6=polynom(rand.int)
p6

# result will vary due to randomness
solve(p6)

# result will vary due to randomness
solve(p6,9)

# result will vary due to randomness
plot(p6)

p4^2

round(p1/3,2)

# result will vary due to randomness
p6/p2

# result will vary due to randomness
p6%/%p2

# result will vary due to randomness
p6%%p2

# result will vary due to randomness
p6%/%p2*p2+p6%%p2==p6

# result will vary due to randomness
coef(p5)

coef((x-2)^20)[1]

coef((x-2)^20)[21]

p7 = polynom(c(0,0,1))
p8 = change.origin(p7,2)
plot(polylist(p7,p8),xlim=c(-4,2))

##### The LCM and GCD of Polynomials #####

p1=poly.from.zeros(c(1,2,3))
p2=poly.from.zeros(c(3,4,5))
LCM(p1,p2)

solve(LCM(p1,p2))

GCD(p1,p2)

# result will vary due to randomness 
#(takes a number of trials to get something other than 1)
rand.int.5=sample(-10:10,5,replace=TRUE)
rand.int.7=sample(-10:10,7,replace=TRUE)
rand.int.10=sample(-10:10,10,replace=TRUE)
p5=poly.from.zeros(rand.int.5)
p7=poly.from.zeros(rand.int.7)
p10=poly.from.zeros(rand.int.10)
GCD(p5,p7,p10)

# result will vary due to randomness 
LCM(p5,p7,p10)

##### Illustrating Roots of a Degree-Three Polynomial #####

par(mar=c(3,3,2,2))
plot(0,0,type="n",xlim=c(-5,5),ylim=c(0,50),ylab="",xlab="")
abline(v=0,lwd=2)
abline(h=seq(1,50,1),col="gray70",lty=2)
for(i in 1:50){
 rand.int=sample(-100:100,4,replace=TRUE)
 p=polynom(rand.int)
 roots=solve(p)
 for(j in 1:3){
 if (Im(roots[j])==0){
 points(Re(roots[j]),i,pch=16,col="blue")}
 else {points(Im(roots[j]),i,pch=15,col="red")
 points(Re(roots[j]),i,pch=17,col="black")
 }}}
legend("topleft",c("Imaginary Part of Root","Real Part of Root","Real Root"),
	pch=c(15,17,16),col=c("red","black","blue"),cex=0.9,bty="n")

##### Creating Pascal's Triangle with Polynomial Coefficients #####

# Book error: Missing { to begin for loop on i.
n=12
par(mar=c(0,0,0,0))
x=polynom()
plot(0,0,type="n",xlim=c(-n,n),ylim=c(-n,0),axes=FALSE,ylab="",xlab="")
for(i in 0:n){ p=(1+x)^i
 for(j in 0:i){
 if (coef(p)[j+1]%%2==1){color="black"} else {color= "red"}
text(2*j-i,-i,label=coef(p)[j+1],col=color)
 }}

##### Calculus with Polynomials #####

p=-1*poly.from.zeros(c(-2,1,3,6))
P=integral(p)+25
plot(P)

integral(p,limits=c(-2,6))

dp=deriv(p)
solve(dp)

summary(P)

poly.info=summary(P)
names(poly.info)

poly.info$zeros

Re(poly.info$zeros)

Im(poly.info$zeros[3])

plot(polylist(p,P,dp),ylab="Plots of P(x),P'(x),and P''(x)",lwd=2)
abline(v=0,h=0,lwd=2)

##### Taylor Polynomial of Sin(x) #####

library(Deriv)
power=50
coef=numeric(power)
for (i in 1:power){
 d.sin=Deriv(sin,nderiv=i)
 coef[i]=d.sin(0)/factorial(i)}
Taylor.poly.sin=polynom(c(sin(0),coef))
plot(Taylor.poly.sin, ylim=c(-1.5,1.5),lwd=3,
	ylab=paste("Taylor Polynomial for sin(x) with n=",power))
curve(sin,add=TRUE,lty=2,lwd=2,col="red")
legend("topright",c("sin(x)","Taylor Polynomial"),col=c("red","black"),
	lty=c(2,1),lwd=c(2,3),bg="white")

##### Legendre Polynomials #####

x=polynom()
P=polylist(x, 1/2*(3*x^2-1))
for(n in 2:14){
 P[[n+1]]=((2*n+1)*x*P[[n]]-n*P[[n-1]])/(n+1)}
P[5]

for (i in 1:15){print(P[[i]](-x)==(-1)^i*P[[i]](x))}

for(i in 1:15){
 p=deriv(P[[i]])
 print(paste("Is ", p(1),"=", i*(i+1)/2,"?", sep=""))}


