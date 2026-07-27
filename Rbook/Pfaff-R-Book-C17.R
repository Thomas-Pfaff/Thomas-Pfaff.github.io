######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 17:  Matrices
######################################################################


A=matrix(c(1,2,3,3,5,6),nrow=3,ncol=2)
A


A[1,2]=4
A


A=cbind(A,c(7,8,9))
A


B=matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=TRUE)
B


B=rbind(B,c(7,8))
B


C=rbind(A[1:2,],c(0,0,0),A[3,])
C


D=C%*%A
D


E=t(D)
E


##### Eigenvalues, Eigenvectors and Other Operations #####

## Results will vary due to randomness. ##

A=replicate(2, sample(1:100, 2))
A


det(A)


B=A%*%solve(A)
round(B,digits=1)


b=c(1,3)
b


x=solve(A,b)
x


y=eigen(A)
Evals=y$val
Evals


Evecs=y$vec
Evecs


A%*%Evecs[,1]-Evals[1]*Evecs[,1]


library(expm)
A=matrix(c(1,0,1,1),2,2)
A%^%6


A=matrix(c(1,0,1,1),2,2)
f=function(A,n){
 B=A
 for(i in 1:(n-1)) A=A%*%B
 return(A)}
f(A,10)


##### Row Operations #####


A=matrix(c(2,2,1,1),2,2,byrow=TRUE)
A


A[1,]=A[1,]*2
A


A[1,]=A[1,]-4*A[2,]
A


B=A[2,]
A[2,]=A[1,]
A[1,]=B
A

