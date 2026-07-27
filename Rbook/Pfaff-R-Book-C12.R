######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 12: The T-Test
######################################################################


##### T Test and Intervals for One and Two Means #####


data(mtcars)
str(mtcars)


shapiro.test(mtcars$mpg)


qqnorm(mtcars$mpg)
qqline(mtcars$mpg,lwd=2)


result=t.test(mtcars$mpg,mu=13.6,alternative="greater")
result


names(result)


result$estimate


result$conf.int


result$conf.int[1]


result$conf.int[2]


cars.4cyl=mtcars[mtcars$cyl==4,]
head(cars.4cyl, n=3)


cars.not.4cyl=mtcars[mtcars$cyl!=4,]
head(cars.not.4cyl,n=3)


result.2=t.test(cars.4cyl$mpg,cars.not.4cyl$mpg,alternative="two.sided",
	conf.level=0.90)
result.2


x.bar.4=mean(cars.4cyl$mpg)
sd.4=sd(cars.4cyl$mpg)
n.4=length(cars.4cyl$mpg)
x.bar.not.4=mean(cars.not.4cyl$mpg)
sd.not.4=sd(cars.not.4cyl$mpg)
n.not.4=length(cars.not.4cyl$mpg)


library(BSDA)
result.sum.2=tsum.test(x.bar.4,sd.4,n.4,x.bar.not.4,sd.not.4,n.not.4)
result.sum.2


##### Illustrating the Meaning of a Confidence Interval Simulation #####

## Results will vary due to randomness. ##

misses=0
sample.size=200
sim=10000
c.level=0.90
for(i in 1:sim){
 data=rnorm(sample.size,10,2)
 result=t.test(data,mu=10,conf.level=c.level)
 if(10 < result$conf.int[1] || 10 > result$conf.int[2]){misses=misses+1} }
paste(misses/sim*100,"%","of the Intervals Missed the True Mean")


misses=0
samp.mean.est=0
sim=100
c.level=0.90
plot(0,0,type="n",xlim=c(9.5,10.5),ylim=c(0,1.07*sim),axes=FALSE,ylab="",xlab="")
axis(1,at=seq(9,11,0.5),label=seq(9,11,0.5))
abline(v=10,lwd=2,col="red")
for(i in 1:sim){data=rnorm(200,10,2)
 result=t.test(data, mu=10, conf.level=c.level)
 if( 10<result$conf.int[1] | 10>result$conf.int[2]){
 misses=misses+1
 segments(result$conf.int[1],i,result$conf.int[2],i,lwd=2,col="green")}
 else {segments(result$conf.int[1],i,result$conf.int[2],i,lwd=2)}
 samp.mean.est=samp.mean.est+result$estimate}
mtext(paste( misses/sim*100,"%","of the Intervals Missed the True Mean"),side=1,line=3)
abline(v=samp.mean.est/sim, lwd=2,col="blue")
legend("topright",c("True Mean","Average of Sample Means"),lwd=c(2,2),
	col=c("red","blue"),bty="n",cex=0.65)