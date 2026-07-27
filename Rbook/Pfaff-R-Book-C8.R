######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 8: Planes, Surfaces, Rotations, and Solids
######################################################################


saddle=function(x,y){x^2-y^2}
x=y=seq(-2,2,length=50)
z=outer(x,y,saddle)
persp(x,y,z,theta=120,phi=30,col="blue",shade=0.2,ticktype="detailed")


contour(x,y,z,main="Contour Plot for the Saddle")
contour(x,y,z,levels=c(0.5,1.5,2.5,3.5),col="red",add=TRUE)
contour(x,y,z,levels=c(-0.5,-1.5,-2.5,-3.5),col="blue",add=TRUE)


##### Interactive: Surface Plots #####

## Note: Figures produced by rgl can be manipulated with a mouse.

library(rgl)
open3d()
saddle=function(x,y){x^2-y^2}
x=y=seq(-4,4,length=100)
z=outer(x,y,saddle)
persp3d(x,y,z,col="blue",zlim=c(-6,6),front="lines",xlab="x axis",
	ylab="y axis",zlab="z axis")
planes3d(4,-2,-1,-3,col="red")


parab=function(x,y){x^2+y^2-20}
x=y=seq(-7,7,length=100)
z=outer(x,y,parab)
n.col=100
colors=rev(rainbow(n.col,start=0,end=5/6))
z.col=cut(z,n.col)
persp3d(x,y,z,col=colors[z.col],xlim=c(-10,10),ylim=c(-10,10),
	zlim=c(-30,90),front="lines",axes=TRUE,xlab="x-axis",
	ylab="y-axis",zlab="z direction")
abclines3d(0,0,0,diag(3),col="gray40",lwd=2)


##### Interactive: Rotations around the x-axis #####
## Note rgl needed for the code below.

persp3d(c(98,99),c(98,99),diag(2),xlim=c(0,5),ylim=c(-5,5),
	zlim=c(-5,5),aspect=c(3,1,1),xlab="x axis",ylab="y axis",zlab="z axis")
abclines3d(0,0,0,diag(3),col="gray40",lwd=2)
x=seq(0,4,length=50)
y=x^2/4
wire3d(turn3d(x,y,n=60,smooth=TRUE),col="blue")


f=function(x){x^2/4}
x1=c(1,1,2,2)
y1=c(0,f(1),f(1),0)
shade3d(turn3d(x1,y1,n=60),col="red")
x2=c(2,2,3,3)
y2=c(0,f(2),f(2),0)
shade3d(turn3d(x2,y2,n=60),col="red")
x3=c(3,3,4,4)
y3=c(0,f(3),f(3),0)
shade3d(turn3d(x3,y3,n=60),col="red")


#####

## Book error: there is an stray } in library(rootSolve)

f=function(x){x^3-7*x^2+12*x+10}
g=function(x){(x-3)^4+5}
curve(f,0,6,ylim=c(0,40))
curve(g,0,6,add=TRUE)
library(rootSolve)
h=function(x){f(x)-g(x)}
roots=uniroot.all(h,c(0,10))
x.int=seq(roots[1],roots[2],length=50)
y.f=f(x.int)
y.g=g(x.int)
points(x.int,y.f,col="red")
points(x.int,y.g,col="blue")
x.rot=c(x.int,rev(x.int))
y.rot=c(y.f,rev(y.g))
polygon(x.rot,y.rot,col="gray")


aspect3d(15,1,1)
x1=y1=c(0,1)
constant=function(x,y){0*x+0*y-99}
z1=outer(x1,y1,constant)
persp3d(x1,y1,z1,xlim=c(0,6),ylim=c(-20,20),zlim=c(-20,20),axes=TRUE,
	aspect=FALSE)
abclines3d(0,0,0,a=diag(3),col="gray40",lwd=2)
x2=seq(0,6,length=50)
y2=f(x2)
wire3d(turn3d(x2,y2,n=60,smooth=TRUE),col="blue")
y3=g(x2)
wire3d(turn3d(x2,y3,n=60,smooth=TRUE),col="green")
shade3d(turn3d(x.rot,y.rot,n=60,smooth=TRUE),col="red")


##### Interactive: Geometric Solids #####
## Note rgl needed for the code below.


cols=rainbow(8)
layout3d(matrix(1:16, 4,4),heights=c(1,3,1,3))
text3d(0,0,0,"tetrahedron"); next3d()
shade3d(tetrahedron3d(col=cols[1])); next3d()
text3d(0,0,0,"dodecahedron"); next3d()
wire3d(dodecahedron3d(col=cols[2]),lwd=2); next3d()
text3d(0,0,0,"cube"); next3d()
wire3d(cube3d(col=cols[3]),lwd=4); next3d()
text3d(0,0,0,"sphere"); next3d()
spheres3d(0,y=NULL,z=NULL,radius=3,col=cols[4]); next3d()
text3d(0,0,0,"octahedron"); next3d()
shade3d(octahedron3d(col=cols[5])); next3d()
text3d(0,0,0,"a 'cylinder'"); next3d()
shade3d(cylinder3d(diag(3),radius=1,sides=5,closed=-1),col=cols[6]); next3d()
text3d(0,0,0,"icosahedron"); next3d()
shade3d(icosahedron3d(col=cols[7])); next3d()
text3d(0,0,0,"an extrusion"); next3d()
x=c(0,1,2,0)
y=c(0,1,0,0)
shade3d(extrude3d(x,y,thickness=3),col=cols[8])