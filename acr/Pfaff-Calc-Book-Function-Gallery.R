######################################################################################################
### Pfaff Applied Calc with R
### Function Gallery
### Caution: Variable names are reused from section to section
### References for all data are in the pdf file
######################################################################################################

### Load Packages
library(rootSolve)
library(Deriv)

### Define colors
Col1<-"red3"
Col2<-"royalblue3"
Col3<-"#542788"
# Transparent black
mycol <- rgb(0, 0, 0, alpha = 0.5, names = "Tblack")


######################################################################################################
## CO2, x years after 1950, Average yearly ppm
######################################################################################################

## URL for the data from GitHub
dataURL<-"https://thomas-pfaff.github.io/acr/Mauna-Loa-CO2-R.csv"

## Read data from online csv file
CO2.data <-read.csv(url(dataURL),header=TRUE)

## Check data
str(CO2.data)
tail(CO2.data)

## Define x and y, mostly to save typing
x<-CO2.data$Years.After.1950
y<-CO2.data$CO2.parts.per.million  

## Define some useful varibales
yr<-2021 # last year of data
max<-500 # max value for y-axis
last_year<-max(x)

## Main graph
par(mar=c(4.5,4.5,2,2))
plot(x,y,xlim=c(0,100),ylim=c(300,max), pch=16, col=mycol, cex=1.75, xlab="Years After 1950", ylab="Yearly Average CO2 (ppm)",cex.axis=1.5,cex.lab=1.5 )
grid(NULL,NULL,col="black",lwd=2)

## Curve fit and add to  graph
CO2.curve<-lm( y ~ I(x) + I(x^2))
Coef<-CO2.curve$coefficients
CO2.func<-function(x){Coef[[1]] + Coef[[2]]*x + Coef[[3]]*x^2}
curve(CO2.func,0,100,lwd=3,col=Col1,add=TRUE)

## Output function
paste(Coef[[3]],"t^2 + ",Coef[[2]],"t+",Coef[[1]])

######################################################################################################
## Global Temp,x years after 1950, Gloval Average Temp in deg C
######################################################################################################

## URL for the data from GitHub
dataURL<-"https://thomas-pfaff.github.io/acr/Global-Temperature-R.csv"

## Read data from online csv file
Temp.data <-read.csv(url(dataURL),header=TRUE)

## Define x and y, mostly to save typing
x<-Temp.data$Years.After.1950
y<-Temp.data$Ave.Global.Temp  

## Define some useful varibales
last_year<-max(x)
yr<-last_year+1950  # last year of data
min<-13.5 # min value for y-axis
max<-16.5  # max value for y-axis

## Create plot
par(mar=c(4.5,4.5,2,2))
y_label<- bquote("Mean Global Temp in" ~ degree ~ "C")
plot(x, y, pch=16, xlim=c(0,100), ylim=c(min,max), xlab="Years after 1950", ylab=y_label, col=mycol, cex=1.75, ,cex.axis=1.5,cex.lab=1.5 )
grid(NULL,NULL,col="black",lwd=2)

## Create model and add to plot
Quad.fit<-lm(y~x+I(x^2))
Coef<-Quad.fit$coefficients
GTemp<-function(x){Coef[[1]] + Coef[[2]]*x + Coef[[3]]*x^2}
curve(GTemp,0,100,lwd=3,col=Col1,add=TRUE)

## Output function
paste(Coef[[3]],"t^2 + ",Coef[[2]],"t+",Coef[[1]])


######################################################################################################
## Poverty, x years after 1990, y billions of people
## https://ourworldindata.org/poverty
######################################################################################################

## URL for the data from GitHub
dataURL<-"https://thomas-pfaff.github.io/acr/Poverty.csv"

## Read data from online csv file
Data <-read.csv(url(dataURL),header=TRUE)

## Check data
str(Data)
tail(Data)
names(Data)

## Define x and y, mostly to save typing
x<-Data$Year -1990
y<-(Data$Number.of.people.below..2.15.a.day)/1000000000

## Create plot
par(mar=c(4.5,4.5,2,2))
plot(x,y,xlim=c(0,30),ylim=c(0,2), pch=16, col=mycol, cex=1.75, xlab="", ylab="Number of People in Extreme Poverty (billions)",cex.axis=1.5,cex.lab=1.5 )
title(xlab="Years after 1990", cex.lab=1.75)
grid(NULL,NULL,col="black",lwd=2)

## Create model and add to plot
fit<-lm(y~x+I(x^2)+I(x^3))   
summary(fit)
names(fit)
Coef<-coef(fit)
Poverty<-function(x){Coef[[1]]+ Coef[[2]]*x +  Coef[[3]]*x^2 + Coef[[4]]*x^3}
curve(Poverty,0,30,lwd=3,col=Col1,add=TRUE)

## Output function
paste(Coef[[1]],"+", Coef[[2]],"x +", Coef[[3]],"x^2 +", Coef[[4]],"x^3")

######################################################################################################
## Life Exp by GDP, x GDP per capita (inflation and country adjusted), y life expectancy at birth
## https://ourworldindata.org/life-expectancy#life-expectancy-and-gdp
######################################################################################################

## URL for the data from GitHub
dataURL<-"https://thomas-pfaff.github.io/acr/life-expectancy-vs-gdp-per-capita-2018.csv"

## Read data from online csv file
Data <-read.csv(url(dataURL),header=TRUE)

## Check data
str(Data)
tail(Data)
names(Data)

## Define x and y, mostly to save typing
x<-Data$GDP.per.capita
y<-Data$Life.expectancy.at.birth..historical.

## Create plot
par(mar=c(4.5,4.5,2,2))
plot(x,y,xlim=c(0,170000),ylim=c(50,85), pch=16, col=mycol, cex=1.75, xlab="", ylab="Life Expectancy at Birth by Country",cex.axis=1.5,cex.lab=1.5 )
title(xlab="GDP per capita", cex.lab=1.75)
grid(NULL,NULL,col="black",lwd=2)

## Create model and add to plot
fit<-nls(y ~ a*log(x+1)+b, start = list(a = 5, b = 20))
summary(fit)
Coef<-coef(fit)
lifeE<-function(x){Coef[[1]]*log(x+1) + Coef[[2]]}
curve(lifeE,0,170000,lwd=3,col=Col1,add=TRUE)

## Output function
paste(Coef[[1]],"log(x+1)+", Coef[[2]])

######################################################################################################
##### Wind, x years since 1980, y cumulative installed megawatts
######################################################################################################

## Set up for all three wind graphs

## URL for the data from GitHub
dataURL<-"https://thomas-pfaff.github.io/acr/Wind-R.csv"

## Read data from online csv file
wind.data<-read.csv(url(dataURL),header=TRUE)

## Define x and y, mostly to save typing
x<-wind.data$Year.Since.1980
ySpa<-wind.data$Spain
yUsa<-wind.data$U.S.
yWorld<-wind.data$World
last_year<-max(x)

#####################
## World
## 5th deg poly
#####################

## Create plot
xWorld<-x[1:(last_year+1)]
y1World<-yWorld[1:(last_year+1)]
par(mar=c(4.5,4.5,2,2))
plot(xWorld, y1World, pch=16, xlim=c(5,last_year),xlab="Years after 1980", ylab="Cumulative Installed World Wind Power in Megawatts", col=mycol, cex=1.75, ,cex.axis=1.5,cex.lab=1.5 )
grid(NULL,NULL,col="black",lwd=2)

## Create model and add to plot
World.fit<-lm(y1World~xWorld+I(xWorld^2)+I(xWorld^3)+I(xWorld^4)+I(xWorld^5))
summary(World.fit)
cWorld<-coef(World.fit)
fifth.World<-function(x){cWorld[[1]] + cWorld[[2]]*x +cWorld[[3]]*x^2+cWorld[[4]]*x^3+cWorld[[5]]*x^4+cWorld[[6]]*x^5}
curve(fifth.World,5,last_year, col=Col1, lwd=3, add=TRUE)

## Output function
paste(cWorld[[6]],"t^5 + ", cWorld[[5]],"t^4 + ",cWorld[[4]],"t^3 + ", cWorld[[3]],"t^2 + ",cWorld[[2]],"t+",cWorld[[1]])

#####################
## Spain
## 6th deg poly
#####################

## Create plot
xSpa<-x[12:(last_year+1)]
y1Spa<-ySpa[12:(last_year+1)]
par(mar=c(4.5,4.5,2,2))
plot(xSpa, y1Spa,pch=16, xlim=c(10,last_year),xlab="Years after 1980", ylab="Cumulative Installed Spain Wind Power in Megawatts", col=mycol, cex=1.75, ,cex.axis=1.5,cex.lab=1.5 )
grid(NULL,NULL,col="black",lwd=2)

## Create model and add to plot - Note no x term
Spain.fit<-lm(y1Spa~xSpa+I(xSpa^2)+I(xSpa^3)+I(xSpa^4)+I(xSpa^5)+I(xSpa^6))
summary(Spain.fit)
cSpa<-coef(Spain.fit)
SpainF<-function(x){ cSpa[[1]]+cSpa[[2]]*x+ cSpa[[3]]*x^2+cSpa[[4]]*x^3+cSpa[[5]]*x^4+cSpa[[6]]*x^5+cSpa[[7]]*x^6  }
curve(SpainF,10,last_year, col=Col1, lwd=3, add=TRUE)

## Output function
paste(cSpa[[7]],"t^6 + ",cSpa[[6]],"t^5 + ", cSpa[[5]],"t^4 + ",cSpa[[4]],"t^3 + ", cSpa[[3]],"t^2 + ",cSpa[[2]],"t+",cSpa[[1]])


#####################
## U.S.
## 5th deg poly
#####################


## Create plot
xUsa<-x[6:(last_year+1)]
y1Usa<-yUsa[6:(last_year+1)]
par(mar=c(4.5,4.5,2,2))
plot(xUsa, y1Usa, pch=16, xlim=c(5,last_year),xlab="Years after 1980", ylab="Cumulative Installed U.S. Wind Power in Megawatts", col=mycol, cex=1.75, ,cex.axis=1.5,cex.lab=1.5 )
grid (NULL,NULL, col = "black",lwd=2)

## Create model and add to plot
Usa.fit<-lm(y1Usa~xUsa+I(xUsa^2)+I(xUsa^4)+I(xUsa^5))
summary(Usa.fit)
cUsa<-coef(Usa.fit)
UsaF<-function(x){cUsa[[1]] + cUsa[[2]]*x +cUsa[[3]]*x^2+cUsa[[4]]*x^4+cUsa[[5]]*x^5}
curve(UsaF, 5,last_year, col=Col1, lwd=3, add=TRUE)

## Output function
paste(cUsa[[5]],"t^5 + ", cUsa[[4]],"t^4 + ", cUsa[[3]],"t^2 + ",cUsa[[2]],"t+",cUsa[[1]])


######################################################################################################
## Gini Energy, x Cumulative Percent Rank, y Cumulative Percent Consumption
######################################################################################################

## URL for the data from GitHub
dataURL<-"https://thomas-pfaff.github.io/acr/Gini-Energy-R.csv"

## Read data from online csv file
Gini <-read.csv(url(dataURL),header=TRUE)

## Define x and y, mostly to save typing
xa<-0
xb<-1
ya<-0
yb<-1
x1<-Gini$CumCountryRank
y1<-Gini$CumCountryConsumption
x2<-Gini$CumStateRank
y2<-Gini$CumStateConsumption
xname<-"Cumulative Percent Rank"
yname<-"Cumulative Percent Consumption"

## Create plot
par(mar=c(4.5,4.5,2,2))
plot(x1, y1 ,  pch=16, xlim=c(xa,xb), ylim=c(ya,yb), xlab=xname, ylab=yname, col=mycol,cex=1.75, ,cex.axis=1.5,cex.lab=1.5 )
points(x2, y2, pch=17, col=mycol,cex=1.75, ,cex.axis=1.5,cex.lab=1.5 )
grid (NULL,NULL, col = "black",lwd=2)
v3<-c("Countries","U.S. States")
legend("topleft", v3, lty=c(0,0),  pch=c(16,17), lwd=c(2,2),cex=c(1.5,1.5), col=c("black","black"), y.intersp=1.25,bg="white")
legend("topleft", v3, lty=c(1,1),  pch=c(NA,NA), lwd=c(2,2),cex=c(1.5,1.5), col=c(Col1,Col2), y.intersp=1.25)

## Create country model and add to plot
model1<-lm(y1~0+ x1+I(x1^2)+I(x1^3)+I(x1^4)+I(x1^5)+I(x1^6)+I(x1^7)+I(x1^8)+I(x1^9))
summary(model1)
pcoef<-coef(model1)
p1<-function(x){pcoef[[1]]*x +pcoef[[2]]*x^2 +pcoef[[3]]*x^3 +pcoef[[4]]*x^4+pcoef[[5]]*x^5+pcoef[[6]]*x^6+pcoef[[7]]*x^7+pcoef[[8]]*x^8+pcoef[[9]]*x^9}
curve(p1, xa,xb, lwd=3, add=TRUE, col=Col1)

# Output function
paste(pcoef[[1]],"x +",pcoef[[2]],"x^2 +",pcoef[[3]],"x^3 +",pcoef[[4]],"x^4+",pcoef[[5]],"x^5+",pcoef[[6]],"x^6+",pcoef[[7]],"x^7+",pcoef[[8]],"x^8+",pcoef[[9]],"x^9")

## Create state model and add to plot
model2<-lm(y2~0+ x2+I(x2^2)+I(x2^3)+I(x2^4)+I(x2^5)+I(x2^6))
summary(model2)
pcoef2<-coef(model2)
p2<-function(x){pcoef2[[1]]*x +pcoef2[[2]]*x^2 +pcoef2[[3]]*x^3 +pcoef2[[4]]*x^4+pcoef2[[5]]*x^5+pcoef2[[6]]*x^6}
curve(p2, xa,xb, lwd=3, add=TRUE, col=Col2)

# Output function
paste(pcoef[[1]],"x +",pcoef[[2]],"x^2 +",pcoef[[3]],"x^3 +",pcoef[[4]],"x^4+",pcoef[[5]],"x^5+",pcoef[[6]],"x^6")

######################################################################################################
## Sea Ice, x month of year, y million square kilometers of sea ice
######################################################################################################

## URL for the data from GitHub
dataURL<-"https://thomas-pfaff.github.io/acr/Arctic-Ice-Calc-R.csv"

## Read data from online csv file
Ice <-read.csv(url(dataURL),header=TRUE)
names(Ice)

## Define variables, mostly to save typing
x<-Ice$Month
y1<-Ice$X1980.extent.in.million.square.km
y2<-Ice$X2012.extent.in.million.square.km
y3<-Ice$X2019.extent.in.million.square.km

## Main graph
par(mar=c(4.5,4.5,2,2))
plot(x, y1, pch=15, xlim=c(1,12), ylim=c(0,20), xlab="Month", ylab="Million Square Kilometers", col=mycol,cex=1.75, ,cex.axis=1.5,cex.lab=1.5 )
points(x, y2, pch=19, xlim=c(1,12), ylim=c(0,20), col=mycol, cex=1.75)
points(x, y3 , pch=17, xlim=c(1,12), ylim=c(0,20), col=mycol,cex=1.75)
grid (NULL,NULL, col = "black",lwd=2)
v3<-c("1980", "2012", "2019")
legend("topright", v3, lty=c(0,0,0), pch=c(15,19,17), lwd=c(2,2,2), cex=c(1.5,1.5,1.5), col=c("black","black", "black"),bg="white", y.intersp=1.25)
legend("topright", v3, lty=c(1,1,1),  pch=c(NA,NA), lwd=c(2,2,2),cex=c(1.5,1.5,1.5), col=c(Col3,Col2,Col1), y.intersp=1.25)

## 1980 curve fit
fit.1980<-lm(y1~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6))
summary(fit.1980)
Coef<-fit.1980$coefficients
p.1980<-function(x){Coef[[1]] + Coef[[2]]*x +Coef[[3]]*x^2 +Coef[[4]]*x^3 +Coef[[5]]*x^4+Coef[[6]]*x^5+Coef[[7]]*x^6}
curve(p.1980,1,12, col=Col3, lwd=3, add=TRUE)

# Output function
paste(Coef[[1]],"+", Coef[[2]],"x +",Coef[[3]],"x^2 +",Coef[[4]],"x^3 +",Coef[[5]],"x^4+",Coef[[6]],"x^5+",Coef[[7]],"x^6")

## 2012 curve fit
fit.2012<-lm(y2~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6))
summary(fit.2012)
Coef<-fit.2012$coefficients
p.2012<-function(x){Coef[[1]] + Coef[[2]]*x +Coef[[3]]*x^2 +Coef[[4]]*x^3 +Coef[[5]]*x^4+Coef[[6]]*x^5+Coef[[7]]*x^6}
curve(p.2012,1,12,lwd=3,add=TRUE, col=Col2)

# Output function
paste(Coef[[1]],"+", Coef[[2]],"x +",Coef[[3]],"x^2 +",Coef[[4]],"x^3 +",Coef[[5]],"x^4+",Coef[[6]],"x^5+",Coef[[7]],"x^6")

## 2019 curve fit
fit.2019<-lm(y3~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6))
summary(fit.2019)
Coef<-fit.2019$coefficients
p.2019<-function(x){Coef[[1]] + Coef[[2]]*x +Coef[[3]]*x^2 +Coef[[4]]*x^3 +Coef[[5]]*x^4+Coef[[6]]*x^5+Coef[[7]]*x^6}
curve(p.2019,1,12,lwd=3,add=TRUE,col=Col1)

# Output function
paste(Coef[[1]],"+", Coef[[2]],"x +",Coef[[3]],"x^2 +",Coef[[4]],"x^3 +",Coef[[5]],"x^4+",Coef[[6]],"x^5+",Coef[[7]],"x^6")

######################################################################################################
##  Blood Alcohol Level, x hours since does, y blood alcohol level in mg/ml
##  https://deepblue.lib.umich.edu/bitstream/handle/2027.42/45071/10928_2005_Article_BF01065396.pdf?sequence=1&isAllowed=y
######################################################################################################

## URL for the data from GitHub
dataURL<-"https://thomas-pfaff.github.io/acr/AlcoholSurge.csv"

## Read data from online csv file
Adata <-read.csv(url(dataURL),header=TRUE)
str(Adata)

## Create plot (45ml dose)
par(mar=c(4.5,4.5,2,2))
plot(Adata$Time45,Adata$Dose45,pch=16,col=mycol,cex=1.75,xlab="Time (hrs)", ylab="Blood Alchohol Level (mg/ml)",cex.axis=1.5,cex.lab=1.5 )
grid(NULL,NULL,col="black",lwd=2)

## Create model and add to plot
Surge<-nls(Dose45 ~ a*Time45*exp(-b*Time45), data=Adata, start = list(a =0.6, b = 1/1))
summary(Surge)
coef<-coef(Surge)
f<-function(x){coef[[1]]*x*exp(-coef[[2]]*x)}
curve(f,0,25,lwd=3,col=Col1,add=TRUE,n=1000)

## Output function
paste("$",coef[[1]],"xe^{-",coef[[2]],"x}$")


######################################################################################################
## Breast cancer, x largest tumor radius (mm), y probability malignant
## https://archive.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+(diagnostic)
######################################################################################################

## URL for the data from GitHub
dataURL<-"https://thomas-pfaff.github.io/acr/wdbc.csv"

## Read data from online csv file
Data <-read.csv(url(dataURL),header=TRUE)
str(Data)

## Create binary variable
Data$Result<-ifelse(Data$Outcome=="B",0,1)

## Check data
str(Data)
tail(Data)

## Define x and y, mostly to save typing
x<-Data$V1
y<-Data$Result

## Create plot 
par(mar=c(4.5,4.5,2,2))
plot(x,jitter(y,factor=.3),xlim=c(6,40),ylim=c(-.1,1.1), pch=16, col=mycol,
 cex=1.2, xlab="Largest Tumor Radius (mm)", ylab="Probability Malignant",cex.axis=1.5,cex.lab=1.5 )
grid(NULL,NULL,col="black",lwd=2)

## Create model and add to plot
logreg <- glm(y ~ x , family = "binomial", data = Data)
summary(logreg)
coef<-logreg$coefficients
Lfun<-function(x){exp(coef[[1]]+coef[[2]]*x) / ( 1 +exp(coef[[1]]+coef[[2]]*x))}
curve(Lfun,6,40,lwd=3,col=Col1,add=TRUE)

## Output function
paste("\frac{e^{", coef[[1]], "+", coef[[2]],"x} }{1+ e^{", coef[[1]],"+",coef[[2]],"x}}")



######################################################################################################
## Cheddar Prices, x months after Jan 1 2010, y price per pouns (U.S. $) 
######################################################################################################

## URL for the data from GitHub
dataURL<-"https://thomas-pfaff.github.io/acr/Cheese.csv"

## Read data from online csv file
Data <-read.csv(url(dataURL),header=TRUE)

## Check data
str(Data)
tail(Data)

## Define x and y, mostly to save typing
x<-Data$Months.After.Jan.1.2010
y<-Data$Chedder

## Create plot 
par(mar=c(4.5,4.5,2,2))
plot(x,y,xlim=c(0,120),ylim=c(4,6), pch=16, col=mycol, cex=1.75, xlab="", ylab="Price per Pound (U.S. Dollars)",cex.axis=1.5,cex.lab=1.5 )
title(xlab="Months After January 1 2010", cex.lab=1.75)
grid(NULL,NULL,col="black",lwd=2)


## Create model and add to plot
fit<-lm(y~x+I(x^2)+I(x^3))
summary(fit)
Coef<-fit$coefficients
prices<-function(x){Coef[[1]] + Coef[[2]]*x +Coef[[3]]*x^2 +Coef[[4]]*x^3}
curve(prices,0,120,lwd=3,col=Col1,add=TRUE)

## Output function
paste("$$", Coef[[1]],"+", Coef[[2]],"x+",  Coef[[3]],"x^2+",  Coef[[4]],"x^3$$")


