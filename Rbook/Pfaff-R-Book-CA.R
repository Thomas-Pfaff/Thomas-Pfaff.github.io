######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Appendix A:  Loops, Vectors, and Apply
######################################################################


total=0
for (i in 1:100){
 total=total+ 1/i^2 }
total


f=function(x){1/x^2}
sum(f(1:100))


sum(sapply(1:100,function(x){1/x^2}))


vector1=numeric(100)
vector1[1]=1
for (i in 2:100){
 vector1[i]=vector1[i-1]+ 1/i^2}
vector1[c(1:3,98:100)]


f=function(x){1/x^2}
vector2=cumsum(f(1:100))
vector2[c(1:3,98:100)]


vector3=cumsum(sapply(1:100,function(x){1/x^2}))
vector3[c(1:3,98:100)]


set.seed(42)
A=matrix(sample(1:10,50,replace=TRUE), ncol=10)
A

## Book error: There is a missing + between count1[col.j] and ifelse().

count1=numeric(10)
for(col.j in 1:10){
 for(row.i in 1:5) {
 count1[col.j]=count1[col.j]+
 ifelse(A[row.i,col.j]%%2==0,1,0)}}
count1


count2=apply(A,2,function(x){sum(ifelse(x%%2==0,1,0))})
count2


