######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 16: Miscellaneous Statistical Tests
######################################################################


##### One-way ANOVA #####


data(iris)
boxplot(iris$Sepal.Width~iris$Species,data=iris,col="gray85",
	border=c("red","blue","purple"),main="Sepal Width of Three Types of Iris")


result.aov=aov(iris$Sepal.Width ~ iris$Species,data=iris)
summary(result.aov)


TukeyHSD(result.aov)


names(result.aov)


sigma(result.aov)


bartlett.test(iris$Sepal.Width ~ iris$Species,data=iris)


qqnorm(result.aov$residuals)
qqline(result.aov$residuals, lwd=2)


##### Stacking Data #####


df.columns=data.frame("C1"=1:3,"C2"=c(5,10,15),"C3"=seq(100,300,by=100))
df.columns


df.stack=stack(df.columns)
colnames(df.stack)=c("data","type")
df.stack

##### Chi-Square Tests #####


sample.1=c(55,45,52,20,16,12)
chi.results.1=chisq.test(sample.1)
chi.results.1


names(chi.results.1)


chi.results.1$expected


test.p=c(0.3,0.2,0.2,0.1,0.1,0.1)
chi.results.2=chisq.test(sample.1,p=test.p)
chi.results.2


sample.2=c(45,40,30,25,38,22)
samp.matrix=matrix(c(sample.1,sample.2),byrow=TRUE,nrow=2)
colnames(samp.matrix)=c("s1","s2","s3","s4","s5","s6")
rownames(samp.matrix)=c("Die 1","Die 2")
samp.matrix


chi.results.3=chisq.test(samp.matrix)
chi.results.3


chi.results.3$expected


data.col=c("r","g","b","y","g","r","O","b","g","y","y")
table(data.col)


##### Testing Standard Deviations #####


data(iris)
str(iris)


unique(iris$Species)


iris.versi=iris[iris$Species=="versicolor",]
head(iris.versi, n=5)


library(EnvStats)
result.versi.var=varTest(iris.versi$Petal.Length,alternative="greater",
	conf.level=0.95,sigma.squared=0.25)
result.versi.var

names(result.versi.var)


iris.setosa=iris[iris$Species=="setosa",]
head(iris.setosa,n=5)


result.iris.width.var=var.test(iris.versi$Petal.Width,iris.setosa$Petal.Width,
	alternative="two.sided",conf.level=0.95)
result.iris.width.var


result.iris.width.var$p.value

## Book error: should be result.iris not test.iris in both below.

result.iris.width.var$conf.int[1]

result.iris.width.var$conf.int[2]

