######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 1: Getting Started
######################################################################


2^6*3*643

#####

f=function(x){x^3-3*x^2-9*x+2}
curve(f,-4,6,lwd=2,col="red")
grid(NULL,NULL,col="black")

#####

example=rnorm(150,0,1)
summary(example)

t.test(example,mu=0,alternative="less",conf.level=0.90)

#####

# Note: The Arctic-Ice-Data-R.csv should be downloaded and placed in a folder.
# Change the directory so that it points to the folder with the file.

Ice=read.table("Arctic-Ice-Data-R.csv",header=TRUE,sep=",")
names(Ice)

#####

rm(Ice)
Ice=read.table("Ice-Data-R.csv",header=TRUE,sep=",")