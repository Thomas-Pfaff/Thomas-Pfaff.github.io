######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 11: A Central Limit Theorem Simulation
######################################################################


##### A Central Limit Theorem Simulation #####

## Results will vary due to randomness. ##

par(mfrow=c(2,2))
trials=1000000
for(i in c(1,10,50,100)){
 simulation=replicate(trials,mean(rexp(i,0.25)))
 x.bar=mean(simulation)
 s=sd(simulation)
 xlabel=paste("Mean from a Sample of Size", i, "\nSimulation with",
	format(trials, big.mark=",", scientific=FALSE) , "Trials")
 title=paste("Mean of Sample Means is",round(x.bar,2),
	"\n Sd of Sample Means is",round(s,2))
 hist(simulation,freq=FALSE,xlab=xlabel,main=title)
 }


par(mfrow=c(2,2))
trials=1000000
for(i in c(1,10,50,100)){
 simulation=replicate(trials,mean(runif(i,0,10)))
 x.bar=mean(simulation)
 s=sd(simulation)
 xlabel=paste("Mean from a Sample of Size", i,"\nSimulation with",
	trials, "Trials")
 title=paste("Mean of Sample Means is",round(x.bar,2),
	"\n Sd of Sample Means is",round(s,2))
 hist(simulation,freq=FALSE,breaks=seq(0,10,by=0.25),xlab=xlabel,main=title)
 }


##### Z Test and Interval for One Mean #####

## A seed was set here, so all results will be the same. ##

library(BSDA)
set.seed(42)
sample.1=rnorm(100,0,1)
result=z.test(sample.1,alternative="two.sided",mu=0,sigma.x=1)
result


names(result)


result$p.value


result$conf.int


result$conf.int[1]


result$conf.int[2]


mu.1=mean(sample.1)
n.1=length(sample.1)
library(BSDA)
result.sum.1=zsum.test(mean.x=mu.1,sigma.x=1,n.x=n.1,mu=0,
	alternative="two.sided",conf.level=0.90)
result.sum.1

##### Z Test and Interval for Two Means #####

## Book error: Stray \ in library(BSDA) removed.

sample.2=rnorm(125,0,1)
library(BSDA)
result.2=z.test(sample.1,sample.2,mu=0,alternative="greater",
	sigma.x=1,sigma.y=1,conf.level=0.90)
result.2


mu.1=mean(sample.1)
mu.2=mean(sample.2)
length.1=length(sample.1)
length.2=length(sample.2)
library(BSDA)
result.sum.2=zsum.test(mean.x=mu.1,sigma.x=1,n.x=length.1,mean.y=mu.2,
	sigma.y=1,n.y=length.2,alternative="two.sided",mu=0,conf.level=0.95)
result.sum.2
