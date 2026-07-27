######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 13: Testing Proportions
######################################################################


##### Tests and Intervals for One and Two Proportions #####

## Results will vary due to randomness. ##

heads=rbinom(1,size=123,prob=0.5)
heads


result.z=prop.test(heads,123,p=0.55,alternative="two.sided")
result.z


result.exact=binom.test(heads,123,p=0.55,alternative="two.sided",conf.level=0.95)
result.exact


names(result.z)


result.z$p.value


result.z$conf.int


result.z$conf.int[1]


result.z$conf.int[2]


heads.2= rbinom(1,size=45,prob=0.5)
result.z.2=prop.test(c(heads,heads.2),c(123,45),alternative="two.sided",conf.level=0.90)
result.z.2

## Book error: test.z.2 is incorrect. Should be result.z.2.

result.z.2$estimate[2]*45


##### Illustrating the Meaning of $\alpha$ Simulation #####


sim=100
pop.size=123
pop.p=0.5
p.0=0.5
alpha=0.05
reject=0
plot(0,0,type="n",xlim=c(0,sim),ylim=c(0,1),xlab="Trial Number",ylab="P-value",)
abline(h=alpha,col="red")
for(i in 1:sim){
  sample=rbinom(1,size=pop.size,prob=pop.p)
  result=prop.test(sample,pop.size,p=p.0, alternative="two.sided",conf.level=(1-alpha))
  points(i,result$p.value,pch=16)
  if( result$p.value < alpha){reject=reject+1}
 }
mtext(paste("Percent of p-values below ", alpha, " is ",reject/sim*100,"%",sep=""),side=1,line=4)

