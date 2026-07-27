######################################################################
### R for College Mathematics and Statistics
### Thomas J Pfaff
### Code for Chapter 10: Simulation
######################################################################

## Results will vary due to randomness in the code below. ##


##### A Coin Flip Simulation #####


flips=20
trials=100000
p.succ=0.40
simulation=rbinom(trials,flips,p.succ)
totals=tabulate(simulation+1,flips+1)
prob=totals/trials
barplot(prob,col="red",names.arg=0:flips,ylab="Probability",xlab="Number of Heads",
	main=paste("A coin flipped", flips,"times with",p.succ*100,"% chance of heads\nsimulated",
	format(trials,scientific=FALSE, big.mark=","),"times."))


prob[11:(flips+1)]


sum(prob[c(11:(flips+1))])


cumsum(prob)


##### An Elevator Problem #####


Trials=100000
People=12
Selection=function(n){
 Adults=sample(0:1,n,replace=TRUE)
 for (i in 1:n){
 if (Adults[i]==0){Adults[i]=rnorm(1,168.5,67.76)}else {Adults[i]=rnorm(1,195.7,68.02)}}
 return(sum(Adults)) }
simulation=replicate(Trials, Selection(People))
hist(simulation,freq=FALSE,border="blue",ylab="Probability",xlab="Weight in Pounds",
	main=paste(format(Trials,scientific=FALSE,big.mark=","),
	"Simulations of the Sum of Weights of",People,"Adults"))

over=simulation[simulation>2400]
length(over)/Trials


##### A Monty Hall Problem #####


cards=matrix(c(1,1,0,0,1,0),byrow=TRUE,nrow=3)
rownames(cards)=c("Card 1", "Card 2", "Card 3")
colnames(cards)=c("Side 1", "Side 2")
cards


Trials=100000
simulation=replicate(Trials,sample(1:3,1))
for(k in 1:Trials){
 i=simulation[k]
 j=sample(c(1:2),1)
 if (cards[i,j]==0){simulation[k]=cards[i,j%%2+1]}
else{simulation[k]=9}}
simulation[1:12]


flip=simulation[simulation==1]
stay=simulation[simulation==0]
length(flip)/(length(flip)+length(stay))


##### Chuck-A-Luck #####


n.trials=100000
n.side=6
n.rolls=3
simulation=replicate(trials,sample(n.side,n.rolls,replace=TRUE))
simulation[,1:10]


chuck=colSums(simulation==1)
chuck[1:10]


m1=1
m2=2
m3=5
wins=tabulate(chuck,3)
return=wins[1]*m1+wins[2]*m2+wins[3]*m3
return/trials


game=function(n.trials,n.sides,n.dice,pay.1,pay.2,pay.3){
 simulation=replicate(n.trials,sample(n.sides,n.dice,replace=TRUE))
 chuck=colSums(simulation==1)
 wins=tabulate(chuck,3)
 money=wins[1]*pay.1+wins[2]*pay.2+wins[3]*pay.3
 expected.money=money/n.trials
 return(expected.money)}
max.die=20
dist=sapply(1:max.die,function(x){game(100000,x,3,1,2,5)})
dist[1:5]


plot(1:max.die,dist,pch=16,xlab="Number of Sides on Dice",ylab="Payout in $")


##### The Buffon Needle Problem #####


Trials=100000
simulation=replicate(Trials,sin(runif(1,0,pi/2))/runif(1,0,1/2))
cross=simulation[simulation>=4]
Trials/length(cross)


plot(c(0,1),c(-0.25,1.25),asp=1,pch=NA,xlab="",ylab="")
abline(h=c(0,1),lwd=2,col="red")
Pins=50
for(i in 1:Pins){
 x=runif(1,min=0,max=1)
 y=runif(1,min=0,max=1)
 theta=runif(1,0,pi)
 points(x,y,col="blue")
 if(theta <= pi/2)
 {segments(x-cos(theta)/4,y-sin(theta)/4,x+cos(theta)/4,y+sin(theta)/4,lwd=2)}
 else{theta.new=pi-theta
 	segments(x-cos(theta.new)/4,y+sin(theta.new)/4,x+cos(theta.new)/4,y-sin(theta.new)/4,lwd=2)}}

##### The Deadly Board Game #####


trials=1000000
max.square=50
rolls=50
simulation=replicate(trials,cumsum(sample(6,rolls,replace=TRUE)))
totals=tabulate(simulation, max.square)
probabilities=totals/sum(totals)
plot(1:max.square,probabilities,type="h",xlab="Square on Board",ylab="Probability",main=paste
	(format(trials,scientific=FALSE,big.mark=","),"Simulations"))


square=1:max.square
percent=probabilities*100
Percent.Position = data.frame(square, percent)
Percent.Position[order(-Percent.Position$percent),]


trials=100000
rolls=8
max.square=20
simulation=replicate(trials,cumsum(sample(6,rolls,replace=TRUE)))
simulation[,1:5]


landing.squares=cbind(c(simulation),rep(seq_len(trials),each=rolls))
landing.squares[1:17,]


landing.squares.cut=landing.squares[landing.squares[, 1] <= max.square,]
landing.squares.cut[1:17,]


games=matrix(0,nrow=max.square,ncol=trials)
games[landing.squares.cut]=1
games[1:17,1:5]


coin.placements=combn(max.square,3)
coin.placements[,1:5]


calculate.success=function(choice){mean(colSums(games[choice, ]) > 0)}
three.coin.probabilities=apply(coin.placements,2,calculate.success)
three.coin.probabilities[1:5]


max(three.coin.probabilities)


which.max(three.coin.probabilities)


coin.placements[,which.max(three.coin.probabilities)]
















