######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 19:  Some Discrete Mathematics
######################################################################

################################################################################
##### Binomial Coefficients, Pascal's Triangle, and a Little Number Theory #####
################################################################################

n=12
par(mar=c(0,0,0,0))
plot(0,0,type="n",xlim=c(-n,n),ylim=c(-n,0),axes=FALSE,ylab="",xlab="")
for(i in 0:n){
 for(j in 0:i){
 if (choose(i,j)%%2==1){color="black"}
 else {color="red"}
 text( 2*j-i,-i,label=choose(i,j),col=color)
 }}


n=19
sum(sapply(0:n,function(x){choose(n,x)}))


library(numbers)
primeFactors(sum(sapply(0:n,function(x){choose(n,x)})))

## Note: If the numbers packages is loaded you don't need to and shouldn't
## load it again.

library(numbers)
isIntpower(sum(sapply(0:n,function(x){choose(n,x)})))

######################
##### Set Theory #####
######################

letters[10:22]


rainbow=c("red","orange","yellow","green","blue","indigo","violet")
U=c(LETTERS,letters,1:20,month.abb,rainbow)
set.A=c(LETTERS[1:10],letters[5:15],1:10,month.abb[3:7],"red","yellow")
set.B=c(LETTERS[5:20],letters[10:25],5:20,month.abb[6:12],"yellow","blue")


intersect(set.A,set.B)


setequal(set.A,set.B)


length(setdiff(set.A,set.B))


is.element("blue",set.A)


set.A.c=setdiff(U,set.A)
setequal(union(set.A.c,set.A),U)


##### Venn Diagrams #####


library(eulerr)
plot(euler(list(A=set.A,B=set.B)),fills=c("firebrick1","cornflowerblue"),
	labels=c("Set A","Set B"))


plot(euler(list(A=set.A,B=set.B,C=rainbow)),
	fills=c("firebrick1","cornflowerblue","goldenrod2"),
	edges=list(col=c("firebrick1","cornflowerblue","black")),
	quantities=list(TRUE,col="white",fontsize=20),
	labels=list(labels=c("Set A","Set B","Rainbow"),
	fontsize=c(15,20,10),col=c("white","navy","black")))


##### Power Set, Cartesian Product, and Intervals #####


library(sets)
power=gset_power(rainbow)
length(power)


gset_cartesian(rainbow,1:3)


reals(0,1,"(]") & reals(0.5,1.5,"()")


reals(1,2) <= reals(1,2,"[)")


##### A Cantor Set Example #####

## Book error: Missing end ) in the first element of the definitin of B, before division by 3.

n=8
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
for (k in c(1,.1,.01,.001)){
 A=reals(0,1,"[]")
 plot(0,0,type="n",xlim=c(0,k),ylim=c(0,n+1),axes=FALSE,ylab="",xlab="")
 segments(min(A),n+1,max(A),n+1,lwd=2,col="blue")
 axis(1, at=seq(0,1,k/10),label=seq(0,1,k/10))
 for(i in 0:n) {
 A.union=interval()
 for(j in 1:(2^i)){
  B=reals(min(A[j])+(max(A[j])-min(A[j]))/3, min(A[j])+2*(max(A[j])-min(A[j]))/3,"()")
  A.temp=interval_complement(B, A[j])
  segments(min(A.temp[1]),n-i,max(A.temp[1]),n-i,lwd=2,col="blue")
  segments(min(A.temp[2]),n-i,max(A.temp[2]),n-i,lwd=2,col="red")
  A.union=A.union|A.temp}
  A=A.union } }


sum(sapply(1:length(A),function(x){max(A[x])-min(A[x])}))

########################
##### Graph Theory #####
########################

##### Creating and Displaying Graphs #####


library(igraph)
complete.9=make_full_graph(9)
par(mar=c(1,1,1,1))
plot(complete.9,layout=layout_in_circle,vertex.shape="square",vertex.color="lightgoldenrod",
	vertex.size=9,vertex.frame.color="navyblue",vertex.label.color="navyblue",
	edge.color="purple",edge.width=2)


par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
lattice.3.5=make_lattice(c(3,5))
plot(lattice.3.5,vertex.color="black",edge.color="black",vertex.size=9,
	vertex.frame.color=NA,vertex.shape="circle",vertex.label=NA)
path.10=make_ring(10,circular=FALSE)
plot(path.10,vertex.color="black",edge.color="black",vertex.size=9,
	vertex.frame.color=NA,vertex.shape="circle",vertex.label=NA)
ring.10=make_ring(10,circular=TRUE)
plot(ring.10,vertex.color="black",edge.color="black",vertex.size=9,
	vertex.frame.color=NA,vertex.shape="circle",vertex.label=NA)
star.10=make_star(10,mode="undirected",center=1)
plot(star.10,vertex.color="black",edge.color="black",vertex.size=9,
	vertex.frame.color=NA,vertex.shape="circle",vertex.label=NA)


par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
bipartite.5.7=make_full_bipartite_graph(5,7,directed=FALSE)
plot(bipartite.5.7,layout=layout_as_bipartite,vertex.color="black",edge.color="black",
	vertex.size=9,vertex.frame.color=NA,vertex.shape="circle",vertex.label=NA)
tree.10.3=make_tree(10,3,mode="undirected")
plot(tree.10.3,layout=layout_as_tree,vertex.color="black",edge.color="black",
	vertex.size=9,vertex.frame.color=NA,vertex.shape="circle",vertex.label=NA)
plot(tree.10.3,vertex.color="black",edge.color="black",vertex.size=9,
	vertex.frame.color=NA,vertex.shape="circle",vertex.label=NA)
petersen=make_graph("Petersen")
plot(petersen,layout=layout_nicely,vertex.color="black",edge.color="black",vertex.size=9,
	vertex.frame.color=NA,vertex.shape="circle",vertex.label=NA)


g = make_empty_graph(5, directed=FALSE)
par(mar=c(1,1,1,1))
par(mfrow=c(3,2))
plot.graph=function(g){plot(g,layout=layout_in_circle,vertex.color="white",
	edge.color="black",vertex.size=15,vertex.frame.color="black",vertex.shape="circle",
	vertex.label.color="black")}
plot.graph(g)
g=add_edges(g,c(1,2,1,5,2,3,3,4,4,2,5,5))
plot.graph(g)
g=delete_edges(g,c("5|5","3|4"))
plot.graph(g)
g=add_vertices(g,3)
plot.graph(g)
g=add_edges(g,c(6,1,7,1,8,1,6,5,7,5,8,5))
plot.graph(g)
g=delete_vertices(g, c("5","3"))
plot.graph(g)


V(g)


E(g)


##### Random Graphs #####

## Note: The code below requires the igraph package.

graph.p=sample_gnp(20,0.25,directed=FALSE,loops=FALSE)
par(mar=c(1,1,1,1))
plot(graph.p,layout=layout_nicely,vertex.color="black",edge.color="black",vertex.size=5,
	vertex.frame.color=NA,vertex.shape="circle",vertex.label=NA)


graph.m=sample_gnm(20,choose(20,2)/10,directed=FALSE,loops=FALSE)
par(mar=c(1,1,1,1))
plot(graph.m,layout=layout_nicely,vertex.color="white",edge.color="black",vertex.size=9,
	vertex.frame.color="black",vertex.label.color="black",edge.width=2)


components(graph.m)

## Results will vary due to randomness.

reps=10000
n=0
p=0.2
n.vertices=20
for (i in 1:reps){
 graph.p=sample_gnp(n.vertices,p,directed=FALSE,loops=FALSE)
 if (components(graph.p)$no > 1){n=n+1} }
paste("Probability of disconnected graph with ", n.vertices,
	" vertices and probability of including an edge p=", p, " is ", n/reps,".", sep="")


##### Some Graph Invariants ##### 


path.6=make_ring(6,circular=FALSE)
articulation_points(path.6)


degree(path.6)


table(degree(path.6))


eccentricity(path.6)


distances(path.6)


complete.5=make_full_graph(5)
clique_num(complete.5)


length(cliques(complete.5, min = 3, max = 4))


as_adjacency_matrix(complete.5,type="both")


all_simple_paths(complete.5, 1, 5)
