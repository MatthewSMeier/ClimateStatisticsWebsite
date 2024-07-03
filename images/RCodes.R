################## R Codes for #################################
#Climate Mathematics: Theory and Applications
#A Cambridge University Press book By SSP Shen and RCJ Somerville
#July 2019
#
#The R codes were written by 
#Samuel Shen, Distinguished Professor
#San Diego State University
#Email: sshen@sdsu.edu
#www.climatemathematics.org
#September 2018, San Diego

############################################################################
#
# Chapter 2: Basics of  R Programming
#
############################################################################

1+4
#[1] 5  This is the result
2+pi/4-0.8
#[1] 1.985398 
x<-1
y<-2
z<-4
t<-2*x^y-z
t
#[1] -2
u=2        # "=" sign and "<-" are almost equivalent
v=3        # The text behind the "#" sign is comments
u+v
#[1] 5
sin(u*v)    # u*v = 6 in the sine function is considered radian by R 
#[1] -0.2794155

#Enter temperature data in c() 
tmax <- c(77, 72, 75, 73, 66, 64, 59)
#Show the data
tmax
#[1] 77 72 75 73 66 64 59

#Generate a sequence using different methods
seq(1,8)
seq(8)
seq(1,8, by=1)
seq(1,8, length=8)
seq(1,8, length.out =8)

#Define a function
samfctn <- function(x) x*x
samfctn(4)
#[1] 16
fctn2 <- function(x,y,z) x+y-z/2
fctn2(1,2,3)
#[1] 1.5 

#Plot temperature data
plot(1:7, c(77, 72, 75, 73, 66, 64, 59))

#More plot examples
plot(sin, -pi, 2*pi)   #plot the curve of y=sin(x) from -pi to 2 pi

square <- function(x) x*x   #Define a function
plot(square, -3,2)   # Plot the defined function

# Plot a 3D surface
x <- seq(-1, 1, length=100)
y <- seq(-1, 1, length=100)
z <- outer(x, y, function(x, y)(1-x^2-y^2))  
#outer (x,y, function) renders z function on the x, y grid
persp(x,y,z, theta=330) 
# yields a 3D surface with perspective angle 330 deg

#Contour plot
contour(x,y,z) #lined contours
filled.contour(x,y,z) #color map of contours

#Symbolic calculations by R

D(expression(x^2,'x'), 'x') 
# Take derivative of x^2 w.r.t. x 
2 * x #The answer is 2x

fx= expression(x^2,'x')  #assign a function  
D(fx,'x') #differentiate the function w.r.t. x
2 * x  #The answer is 2x

fx= expression(x^2*sin(x),'x') 
#Change the expression and use the same derivative command
D(fx,'x')
2 * x * sin(x) + x^2 * cos(x)

fxy = expression(x^2+y^2, 'x','y') 
#One can define a function of 2 or more variables
fxy #renders an expression of the function in terms of x and y
#expression(x^2 + y^2, "x", "y")
D(fxy,'x') #yields the partial derivative with respect to x: 2 * x
D(fxy,'y') #yields the partial derivative with respect to y: 2 * y

square = function(x) x^2
integrate (square, 0,1) 
#Integrate x^2 from 0 to 1 equals to 1/3 with details below
#0.3333333 with absolute error < 3.7e-15

integrate(cos,0,pi/2) 
#Integrate cos(x) from 0 to pi/2 equals to 1 with details below
#1 with absolute error < 1.1e-14

#Vectors and matrices by R

c(1,6,3,pi,-3) #c() gives a vector and is considered a 4X1 column vector
#[1]  1.000000  6.000000  3.000000  3.141593 -3.000000
seq(2,6) #Generate a sequence from 2 to 6
#[1] 2 3 4 5 6
seq(1,10,2) # Generate a sequence from 1 to 10 with 2 increment 
#[1] 1 3 5 7 9
x=c(1,-1,1,-1)
x+1 #1 is added to each element of x
#[1] 2 0 2 0
2*x #2 multiplies each element of x
#[1]  2 -2  2 -2
x/2 # Each element of x is divided by 2
#[1]  0.5 -0.5  0.5 -0.5
y=seq(1,4)
x*y  # This multiplication * multiples each pair of elements 
#[1]  1 -2  3 -4
x%*%y #This is the dot product of two vectors and yields 
#     [,1]
#[1,]   -2
t(x)  # Transforms x into a row 1X4 vector
#     [,1] [,2] [,3] [,4]
#[1,]    1   -1    1   -1
t(x)%*%y #This is equivalent to dot product and forms 1X1 matrix
#     [,1]
#[1,]   -2
x%*%t(y) #This column times row yields a 4X4 matrix
#     [,1] [,2] [,3] [,4]
#[1,]    1    2    3    4
#[2,]   -1   -2   -3   -4
#[3,]    1    2    3    4
#[4,]   -1   -2   -3   -4
my=matrix(y,ncol=2) 
#Convert a vector into a matrix of the same number of elements
#The matrix elements go by column, first column, second, etc
#Commands matrix(y,ncol=2, nrow=2)  or matrix(y,2)  
#or matrix(y,2,2) does the same job
my
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
dim(my)  #find dimensions of a matrix
#[1] 2 2
as.vector(my) #Convert a matrix to a vector, again via columns
#[1] 1 2 3 4
mx <- matrix(c(1,1,-1,-1), byrow=TRUE,nrow=2)
mx*my #multiplication between each pair of elements
#     [,1] [,2]
#[1,]    1    3
#[2,]   -2   -4
mx/my #division between each pair of elements
#     [,1]       [,2]
#[1,]  1.0  0.3333333
#[2,] -0.5 -0.2500000
mx-2*my  
#     [,1] [,2]
#[1,]   -1   -5
#[2,]   -5   -9
mx%*%my #This is the real matrix multiplication in matrix theory
#     [,1] [,2]
#[1,]    3    7
#[2,]   -3   -7
det(my) #determinant
#[1] -2
myinv = solve(my) #yields the inverse of a matrix
myinv
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
myinv%*%my #verifies the inverse of a matrix
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
diag(my) #yields the diagonal vector of a matrix
#[1] 1 4
myeig=eigen(my) #yields eigenvalues and unit eigenvectors
myeig
myeig$values
#[1]  5.3722813 -0.3722813
myeig$vectors
#           [,1]       [,2]
#[1,] -0.5657675 -0.9093767
#[2,] -0.8245648  0.4159736
mysvd = svd(my) #SVD decomposition of a matrix M=UDV'
#SVD can be done for a rectangular matrix of mXn
mysvd$d
#[1] 5.4649857 0.3659662
mysvd$u
#           [,1]       [,2]
#[1,] -0.5760484 -0.8174156
#[2,] -0.8174156  0.5760484
mysvd$v
#           [,1]       [,2]
#[1,] -0.4045536  0.9145143
#[2,] -0.9145143 -0.4045536

ysol=solve(my,c(1,3)) 
#solve linear equations matrix %*% x = b
ysol  #solve(matrix, b)
#[1]  2.5 -0.5
my%*%ysol #verifies the solution
#     [,1]
#[1,]    1
#[2,]    3

#Simple statistics by R

x=rnorm(10) #generate 10 normally distributed numbers
x
#[1]  2.8322260 -1.2187118  0.4690320 -0.2112469  0.1870511
#[6]  0.2275427 -1.2619005  0.2855896  1.7492474 -0.1640900
mean(x)
#[1] 0.289474
var(x)
#[1] 1.531215
sd(x)
#[1] 1.237423
median(x)
#[1] 0.2072969
quantile(x)
#        0%        25%        50%        75%       100% 
#-1.2619005 -0.1994577  0.2072969  0.4231714  2.8322260 
range(x) #yields the min and max of x
#[1] -1.261900  2.832226
max(x)
#[1] 2.832226

boxplot(x) #yields the box plot of x
w=rnorm(1000)

summary(rnorm(12)) #statistical summary of the data sequence
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1.9250 -0.6068  0.3366  0.2309  1.1840  2.5750 

hist(w) 
#yields the histogram of 1000 random numbers with a normal distribution

#Linear regression and linear trend line
#2007-2016 data of the global temperature anomalies
#Source: NOAAGlobalTemp data
t=2007:2016
T=c(.36,.30, .39, .46, .33, .38, .42, .50, .66, .70)
lm(T ~ t) #Linear regression model of temp vs time
#(Intercept)            t  
#-73.42691      0.03673 
#Temperature change rate is 0.03673  deg C/yr or 0.37 deg C/decade
plot(t,T, type="o",xlab="Year",ylab="Temperature [deg C]",
     main="2007-2016 Global Temperature Anomalies 
     and Their Linear Trend [0.37 deg C/decade] ")
abline(lm(T ~ t), lwd=2, col="red") #Regression line

#The R packages and the datasets used in this book are 
#listed below and can be downloaded and installed first 
#before proceeding to the R codes in the rest of the book. 
#The R packages: 
#animation, chron, e1071, fields, ggplot2, lattice, 
#latticeExtra, maps, mapdata, mapproj, matrixStats, ncdf, 
#NLRoot, RColorBrewer, rgdal, rasterVis, raster, sp, TTR

#To load the package "animation", you can do 
#library(animation)

#You can also load all these packages in one shot
#using pacman
#install.packages("pacman")
library(pacman)
pacman::p_load(animation, chron, e1071, fields, ggplot2, lattice, 
            latticeExtra, maps, mapdata, mapproj, matrixStats, ncdf4, 
            NLRoot, RColorBrewer, rgdal, rasterVis, raster, sp, TTR)

#The zipped data file:
# https://cambridge.org/climatemathematics/data.zip

#On your computer, you can create a directory called 
#climmath under your user name. 
#The one used in the book is Users/sshen/climmath  
#You unzip the data and move the data folder under 
#the Users/sshen/climmath directory. 
#A data folder will created:  
#Users/sshen/climmath/data. 
#The data folder contains about 400 MB of data. 
#Place all the R codes in the directory  Users/sshen/climmath. 
#Then, you can run all the codes in this book after replacing sshen 
#by your user name on your own computer. 


############################################################################
#
# Chapter 3: Basic Statistical Methods for Climate Data Analysis
#
############################################################################
setwd("/Users/sshen/climmath") #set the working directory
#R can directly read data from a remote data website
dat1 <- read.table("data/aravg.ann.land_ocean.90S.90N.v4.0.0.2015.txt")
dim(dat1)
tmean15=dat1[,2] #Take only the second column of this data matrix
head(tmean15) #The first five values
#[1] -0.367918 -0.317154 -0.317069 -0.393357 -0.457649 -0.468707
mean(tmean15)
#[1] -0.2034367
sd(tmean15)
#[1] 0.3038567
var(tmean15)
#[1] 0.09232888
library(e1071) 
#This R library is needed to compute the following parameters
#install.packages("e1071") #if it is not installed on your computer
skewness(tmean15)
#[1] 0.7141481
kurtosis(tmean15)
#[1] -0.3712142
median(tmean15)
#[1] -0.29694
quantile(tmean15,probs= c(0.05,0.25, 0.75, 0.95))
#        5%        25%        75%        95% 
#-0.5792472 -0.4228540 -0.0159035  0.3743795 

#Plot Fig. 3.1
yrtime15 = seq(1880, 2015)
reg8015 = lm(tmean15 ~ yrtime15)
# Display regression results
reg8015
#Call:
#lm(formula = tmean15 ~ yrtime15)
#Coefficients:
#(Intercept)     yrtime15  
#-13.208662     0.006678  
# Plot the temperature time series and its trend line
plot(yrtime15,tmean15,xlab="Year",ylab="Temperature deg C", 
     main="Global Annual Mean Land and Ocean Surface 
     Temperature Anomalies 1880-2015", type="l", lwd=2)
abline(reg8015, col="red")
text(1930, 0.4, "Linear temperature trend 0.67 deg C per century", 
     col="red",cex=1.2)

#Covariance
x=yrtime15
y=tmean15
cov(x, y)
#[1] 10.36856

#Correlation
cor(tmean15, yrtime15)
#[1] 0.8659857
#Verify this result by formulas
x=yrtime15
y=tmean15
sx=sd(yrtime15)
sy=sd(tmean15)
cxy=cov(x,y)
rxy=cxy/(sx*sy)
rxy
#[1] 0.8659857 #The same result as cor(tmean15, yrtime15)

#Verify the lienar trend by formulas
rxy*sy/sx
#[1] 0.006677908 #verified
cxy/sx^2
#[1] 0.006677908 #verified

#Plot histogram of the tmean15 data
h = hist(tmean15, main="Histogram of 1880-2015 Temperature Anomalies",
         xlab=expression(paste("Temperature anomalies [", degree, "C]")), 
         xlim=c(-1,1), ylim=c(0,50)) 
xfit<-seq(-1,1, length=100)
areat = sum((h$counts)*diff(h$breaks[1:2]))#Normalization area
#diff(h$breaks[1:2])= h$breaks[2] - h$breaks[1] 
#is the histogram's bin width
yfit<-areat*dnorm(xfit, mean=mean(tmean15), sd=sd(tmean15))
#Plot the normal fit on the histogram
lines(xfit, yfit,col="blue",lwd=2) 

#Box plot
boxplot(tmean15, ylim=c(-0.8,0.8), 
        ylab=expression(paste("Temperature anomalies [", degree, "C]")))

#Plot Fig. 3.4
#Use setwd("working directory") to work in the desired directory
#rm(list=ls())
setwd("/Users/sshen/climmath")
par(mgp=c(1.5,0.5,0))
ust=read.csv("data/USJantemp1951-2016-nohead.csv",header=FALSE)
soi=read.csv("data/soi-data-nohead.csv", header=FALSE) 
soid=soi[,2] #Take the second column SOI data
soim=matrix(soid,ncol=12,byrow=TRUE) 
#Make the SOI into a matrix with each month as a column
soij=soim[,1] #Take the first column for Jan SOI
ustj=ust[,3] #Take the third column: Jan US temp data
setEPS()
postscript("fig0304.eps", height=7, width=7)
par(mar=c(4.5,5,2.5,1), xaxs = "i", yaxs = "i")
plot(soij,ustj,xlim=c(-4,4), ylim=c(-8,8),
     main="January SOI and the U.S. Temperature Anomalies",
     xlab="SOI [dimensionless]", 
     ylab=expression(paste("Temperature Anomalies [", degree, "F]")), 
     pch=19, cex.lab=1.3)
# Plot the scatter plot
soiust=lm(ustj ~ soij) #Linear regression
abline(soiust, col="blue", lwd=4) #Linear regression line
dev.off()

#Plot Fig. 3.5
#Q-Q plot for the standardized temperature anomalies
tstand = (tmean15-mean(tmean15))/sd(tmean15)
set.seed(101)
qn=rnorm(136) #Simulate 136 points by N(0,1)
qns=sort(qn)#Sort the points
qq2=qqnorm(qns,col="blue",lwd=2)

setEPS()
postscript("fig0305.eps", height=7, width=7)
par(mar=c(4.5,5,2.5,1), xaxs = "i", yaxs = "i")
qt=qqnorm(tstand, 
          main = "Q-Q plot for the Standardized Global Temp Anomalies vs N(0,1)", 
          ylab="Quantile of Temperature Anomalies", 
          xlab="Quantile of N(0,1)", xlim=c(-3,3),ylim=c(-3,3),
          cex.lab=1.3, cex.axis=1.3)
qqline(tstand, col = "red", lwd=3)
points(qq2$x, qq2$y, pch=19, col="purple")
dev.off()

#Plot Fig. 3.6
plot.new()
layout(matrix(c(1,2,3), 1, 3, byrow = TRUE), 
       widths=c(3,3,3), heights=c(1,1,1))
lasvegas=c(0.58,0.42)
sandiego=c(0.4,0.6)
seattle=c(0.16,0.84)
names(lasvegas)=c("Clear","Cloudy")
names(sandiego)=c("Clear","Cloudy")
names(seattle)=c("Clear","Cloudy")
barplot(lasvegas,col=c("skyblue","gray"), ylim=c(0,1),
        ylab="Probability", cex.lab=1.2)
mtext("Las Vegas", side=3, line=1)
barplot(sandiego,col=c("skyblue","gray"), ylim=c(0,1))
mtext("San Diego", side=3, line=1)
barplot(seattle,col=c("skyblue","gray"), ylim=c(0,1))
mtext("Seattle", side=3, line=1)
mtext("Probability Distribution of Cloudiness", 
      cex=1.2,side = 3, line = -1.5, outer = TRUE)

#Plot Fig. 3.7
# Density function, probability, and normalization of N(0,1)
par(mar=c(4.0,4.0,1.5,0.5))
cord.x <- c(-3,seq(-3,3,0.01),-1) 
cord.y <- c(0,dnorm(seq(-3,3,0.01)),0) 
# Make a curve
curve(dnorm(x,0,1), xlim=c(-3,3), lwd=3,
      main='PDF of the Standard Normal Distribution',
      xlab="Random variable x",
      ylab='Probability density') 
# Add the shaded area using many lines
polygon(cord.x,cord.y,col='skyblue')
polygon(c(-1.5,-1.5, -1.2, -1.2),c(0, dnorm(-1.5),
                                   dnorm(-1.2), 0.0),col='white')
text(0,0.18, "Area=1", cex=1.5)
text(-1.65,0.045,"f(x)")
text(-1.35,0.075,"dx")
text(-1.6,0.005,"x")
text(-0.9,0.005,"x+dx")
arrows(-2,0.2,-1.35,0.13, length=0.1)
text(-2,0.21,"dA=f(x)dx")
text(0,0.09,expression(paste(integral(f(x)*dx,- infinity,infinity),"=1")))

#Plot Fig. 3.8
#Normal distribution plot
x <- seq(-8, 8, length=200)
plot(x,dnorm(x, mean=0, sd=1), type="l", lwd=4, col="red",
     ylim = c(0,1),
     xlab="Random variable x",
     ylab ="Probability density",
     main=expression(Normal~Distribution ~ N(mu,sigma^2)))
lines(x,dnorm(x, mean=0, sd=2), type="l", lwd=2, col="blue")
lines(x,dnorm(x, mean=0, sd=0.5), type="l", lwd=2, col="black")
lines(x,dnorm(x, mean=3, sd=1), type="l", lwd=2, col="purple")
lines(x,dnorm(x, mean=-4, sd=1), type="l", lwd=2, col="green")
#ex.cs1 <- expression(plain(sin) * phi,  paste("cos", phi))
ex.cs1 <- expression(paste(mu, "=0",~","~ sigma, "=1"), 
                     paste(mu, "=0",~","~ sigma, "=2"),
                     paste(mu, "=0",~","~ sigma, "=1/2"),
                     paste(mu, "=3",~","~ sigma, "=1"),
                     paste(mu, "=-4",~","~ sigma, "=1"))
legend("topleft",legend = ex.cs1, lty=1, 
       col=c('red','blue','black','purple','green'), cex=1, bty='n')

#Calculate probability from a normal distribution
mu=0
sig=1
intg <- function(x){(1/(sig*sqrt(2*pi)))*exp(-(x-mu)^2/(2*sig^2))}
integrate(intg,-2,2)
#0.9544997 with absolute error < 1.8e-11
#Or using the R built-in function dnorm to get the same result
integrate(dnorm,-2,2)
#0.9544997 with absolute error < 1.8e-11
integrate(dnorm,-1.96,1.96)
#0.9500042 with absolute error < 1e-11

#Plot Fig. 3.9
#Plot t-distribution by R
x <- seq(-4, 4, length=200)
plot(x,dt(x, df=3), type="l", lwd=4, col="red",
     ylim = c(0,0.6),
     xlab="Random variable t",
     ylab ="Probability density",
     main="Student t-distribution T(t,df)")
lines(x,dt(x, df=1), type="l", lwd=2, col="blue")
lines(x,dt(x, df=2), type="l", lwd=2, col="black")
lines(x,dt(x, df=6), type="l", lwd=2, col="purple")
lines(x,dt(x, df=Inf), type="l", lwd=2, col="green")
#ex.cs1 <- expression(plain(sin) * phi,  paste("cos", phi))
ex.cs1 <- c("df=3", "df=1","df=2","df=6","df=Infinity")
legend("topleft",legend = ex.cs1, lty=1, 
       col=c('red','blue','black','purple','green'), cex=1, bty=n)

#Confidence interval simulation
mu=14 #true mean
sig=0.3 #true sd
n=50 #sample size
d=1.96*sig/sqrt(n)
lowerlim=mu-d
upperlim=mu+d
ksim=10000 #number of simulations
k=0 #k is the simulation counter
xbar=1:ksim
for (i in 1:ksim)
{
  xbar[i]=mean(rnorm(n, mean=mu, sd=sig))
  if (xbar[i] >= lowerlim & xbar[i] <= upperlim)
    k=k+1
}
print(c(k,ksim))
#[1]  9496 10000

#Plot the histogram Fig. 3.10
hist(xbar,breaks=51,
     main="Histogram of the Simulated 
     Simulated Mean Temperatures",xaxt="n",
     xlab=expression(paste("Temperature [", degree, "C]")), 
     ylim=c(0,600))
axis(1,pos = -20, at = c(13.92, mu, 14.08))
text(14,530,"95% Confidence Interval (13.92, 14.08)")

#Plot Fig. 3.11
#Plot confidence intervals and tail probabilities
par(mar=c(2.5,3.5,2.0,0.5))
rm(list=ls())
par(mgp=c(1.4,0.5,0))
curve(dnorm(x,0,1), xlim=c(-3,3), lwd=3,
      main='Confidence Intervals and Confidence Levels',
      xlab="True mean as a random variable", ylab="",
      xaxt="n", cex.lab=1.2) 
title(ylab='Probability density', line=2, cex.lab=1.2)
polygon(c(-1.96, seq(-1.96,1.96,len=100), 1.96),
        c(0,dnorm(seq(-1.96,1.96,len=100)),0),col='skyblue')
polygon(c(-1.0,seq(-1.0, 1, length=100), 1),
        c(0, dnorm(seq(-1.0, 1, length=100)), 0.0),col='white')
polygon(c(-3.0,seq(-3.0, -1.96, length=100), -1.96),
        c(0, dnorm(seq(-3.0, -1.96, length=100)), 0.0),col='red')
polygon(c(1.96,seq(1.96, 3.0, length=100), 3.0),
        c(0, dnorm(seq(1.96, 3.0, length=100)), 0.0),col='red')
points(c(-1,1), c(0,0), pch=19, col="blue")
points(0,0, pch=19)
points(c(-1.96,1.96),c(0,0),pch=19, col="red")
text(0,0.02, expression(bar(x)), cex=1.0)
text(-1.50,0.02, "SE", cex=1.0)
text(-0.60,0.02, "SE", cex=1.0)
text(1.50,0.02, "SE", cex=1.0)
text(0.60,0.02, "SE", cex=1.0)
text(0,0.2, "Probability 
     = 0.68")
arrows(-2.8,0.06,-2.35,0.01, length=0.1)
text(-2.5,0.09, "Probability
     =0.025") 

#Estimate the mean and error bar for a large sample
#Confidence interval for NOAAGlobalTemp 1880-2015
setwd("/Users/sshen/climmath")
dat1 <- read.table("data/aravg.ann.land_ocean.90S.90N.v4.0.0.2015.txt")
dim(dat1)
tmean15=dat1[,2]
MeanEst=mean(tmean15)
sd1 =sd(tmean15)
StandErr=sd1/sqrt(length(tmean15))
ErrorMar = 1.96*StandErr
MeanEst
#[1] -0.2034367
print(c(MeanEst-ErrorMar, MeanEst+ErrorMar))
#[1] -0.2545055 -0.1523680

#Plot Fig. 3.12
setEPS()
postscript("fig0312.eps", height=7, width=10)
par(mar=c(2.3,3.0,2.0,0.5))
rm(list=ls())
par(mgp=c(1.0,0.5,0))
curve(dnorm(x,0,1), xlim=c(-3,3), lwd=3,
      main='Z-score, p-value, and significance level',
      xlab="z: Standard normal random variable",
      ylab='Probability density',xaxt="n",yaxt="n",
      cex.lab=1.2, ylim=c(-0.1,0.4)) 
lines(c(-3,3),c(0,0))
arrows(-3,-0.1,-2.02,-0.1, lwd=12,col='skyblue', length=0.2, code=3)
arrows(3,-0.1,-1.90,-0.1, lwd=12,col='green', length=0.2, code=3)
polygon(c(-3.0,seq(-3.0, -2.5, length=100), -2.5),
        c(0, dnorm(seq(-3.0, -2.5, length=100)), 0.0),col='skyblue')
polygon(c(-1.96,seq(-1.96, 3, length=100), 3),
        c(0, dnorm(seq(-1.96, 3, length=100)), 0.0),col='lightgreen')
points(-1.96,0, pch=19, col="red")
points(-2.5,0,pch=19, col="skyblue")
text(-1.5,-0.02, expression(z[0.025]~'=-1.96'), cex=1.3, col='red')
text(-2.40,-0.02, "z-score", cex=1.3, col='skyblue')
arrows(-2.8,0.06,-2.6,0.003, length=0.1)
lines(c(-1.96,-1.96),c(-0.1, .4),lwd=1.5, col='red')
text(-2.5,0.09, "p-value", cex=1.3) 
text(1.0,-0.06, expression(H[0] ~'region'), cex=1.5)
text(-2.5,-0.06, expression(H[1] ~'region'), cex=1.5)
text(0,0.15, expression(H[0] ~'probability 0.975'), cex=1.5)
dev.off()

#Hypothesis test for NOAAGlobalTemp 2006-2015
setwd("/Users/sshen/climmath")
dat1 <- read.table("data/aravg.ann.land_ocean.90S.90N.v4.0.0.2015.txt")
tm0615=dat1[127:136,2]
MeanEst=mean(tm0615)
MeanEst
#[1] 0.4107391
sd1 =sd(tm0615)
sd1
#[1] 0.1023293
n=10
t_score=(MeanEst -0)/(sd1/sqrt(n))
t_score
#[1] 12.69306
1-pt(t_score, df=n-1)
#[1] 2.383058e-07 #p-value
qt(1-0.025, df=n-1)
#[1] 2.262157 #critical t-score

#Hypothesis test for global temp for 1981-1990 and 1991-2000
setwd("/Users/sshen/climmath")
dat1 <- read.table("data/aravg.ann.land_ocean.90S.90N.v4.0.0.2015.txt")
tm8190=dat1[102:111,2]
tm9100=dat1[112:121,2]
barT1=mean(tm8190)
barT2=mean(tm9100)
S1sd=sd(tm8190)
S2sd=sd(tm9100)
n1=n2=10
Spool=sqrt(((n1 - 1)*S1sd^2 + (n2 - 1)*S2sd^2)/(n1 + n2 -2))
t = (barT2 - barT1)/(Spool*sqrt(1/n1 + 1/n2))
tlow = qt(0.025, df= n1 + n2 -2)
tup = qt(0.975, df= n1 + n2 -2)
paste("t-score=", round(t,digits=5), 
      "tlow=", round(tlow,digits=5), 
      "tup=", round(tup,digits=5))
#[1] "t-score= 2.57836 tlow= -2.10092 tup= 2.10092"
pvalue = 1-pt(t,  df= n1 + n2 -2)
paste( "p-value=", pvalue)
#[1] "p-value= 0.00947040009284539"
paste("1981-90 temp=", barT1, "1991-00 temp=",barT2)
#[1] "1981-90 temp= 0.0368621 1991-00 temp= 0.1612545"
barT2 - barT1
#[1] 0.1243924

#Statistical inference of a linear trend
setwd("/Users/sshen/climmath")
dat1 <- read.table("data/aravg.ann.land_ocean.90S.90N.v4.0.0.2015.txt")
tm=dat1[,2]
x = 1880:2015
summary(lm(tm ~ x))
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.321e+01  6.489e-01  -20.36   <2e-16 ***
#  x            6.678e-03  3.331e-04   20.05   <2e-16 ***


############################################################################
#
# Chapter 4: Climate Data Matrices and Linear Algebra
#
############################################################################

#Matrix subtraction
matrix(c(1,1,1,-1), nrow=2) - matrix(c(1,3,2,0), nrow=2)
#     [,1] [,2]
#[1,]    0   -1
#[2,]   -2   -1

#Matrix multiplication
matrix(c(1,1,1,-1), nrow=2) %*% matrix(c(1,2,3,4), nrow=2)
#      [,1] [,2]
#[1,]    3    7
#[2,]   -1   -1
matrix(c(1,2,3,4), nrow=2) %*% matrix(c(1,1,1,-1), nrow=2) 
#       [,1] [,2]
#[1,]    4   -2
#[2,]    6   -2

#Find the inverse of a matrix
solve(matrix(c(1,1,1,-1), nrow=2))
#     [,1] [,2]
#[1,]  0.5  0.5
#[2,]  0.5 -0.5

#Verify the inverse matrix 
matrix(c(0.5,0.5,0.5,-0.5), nrow=2) %*% matrix(c(1,1,1,-1), nrow=2) 
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1

#Solve a group of linear equations
solve(matrix(c(1,1,1,-1),nrow=2),c(20,4))
#[1] 12  8  #This is the result x1=12, and x2=8.

#Compute a covariance matrix
A=matrix(c(1,-1,2,0,3,1),nrow=2)
A
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]   -1    0    1
covm=(1/(dim(A)[2]))*A%*%t(A)
covm #is the covariance matrix.
#          [,1]      [,2]
#[1,] 4.6666667 0.6666667
#[2,] 0.6666667 0.6666667

#Matrix multiplication of a vector changes the vector direction 
u=c(1,-1)
v=covm%*%u
v
#     [,1]
#[1,]    4
#[2,]    0
#u and v are in different directions.

#Matrix mutiplication of an eigenvector does change direction
ew=eigen(covm)
eigen(covm)$values
#[1] 4.7748518 0.5584816
eigen(covm)$vectors
#[,1]       [,2]
#[1,] -0.9870875  0.1601822
#[2,] -0.1601822 -0.9870875
#Verify the eigenvectors and eigenvalues
covm%*%ew$vectors[,1]/ew$values[1]
#[,1]
#[1,] -0.9870875
#[2,] -0.1601822
#This is the first eigenvector

#SVD: Decomposition and recovery of a space-time data matrix 
#Develop a 2-by-3 space-time data matrix for SVD
A=matrix(c(1,-1,2,0,3,1),nrow=2)
A
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]   -1    0    1
#Perform SVD calculation
msvd=svd(A)
msvd
msvd$d
#[1] 3.784779 1.294390
msvd$u
#          [,1]       [,2]
#[1,] -0.9870875 -0.1601822
#[2,] -0.1601822  0.9870875
msvd$v
#           [,1]       [,2]
#[1,] -0.2184817 -0.8863403
#[2,] -0.5216090 -0.2475023
#[3,] -0.8247362  0.3913356
#One can verify that A=UDV', where V' is transpose of V.
verim=msvd$u%*%diag(msvd$d)%*%t(msvd$v)
verim
#     [,1]         [,2] [,3]
#[1,]    1 2.000000e+00    3
#[2,]   -1 1.665335e-16    1
round(verim)
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]   -1    0    1
#This is the original data matrix A

#Covariance matrix and its eigen-problem
covm = (1/(dim(A)[2]))*A%*%t(A)
eigcov = eigen(covm)
eigcov$values
#[1] 4.7748518 0.5584816
eigcov$vectors
#           [,1]       [,2]
#[1,] -0.9870875  0.1601822
#[2,] -0.1601822 -0.9870875

#Eigenvalues from SVD and from a covariance matrix
((msvd$d)^2)/(dim(A)[2])
#[1] 4.7748518 0.5584816
eigcov$values
#[1] 4.7748518 0.5584816

#Plot Fig. 4.2
#SOI and the Standardized SLP data at Darwin and Tahiti 
setwd("/Users/sshen/climmath")
setEPS()
postscript("fig0402a.eps", height=4.9, width=7)
par(mar=c(4.0,4.2,1.5,0.5))
Pta<-read.table("data/PSTANDtahiti.txt", header=F)
# Remove the first column that is the year
ptamon<-Pta[,seq(2,13)]
#Convert the matrix into a vector according to mon: Jan 1951, Feb 1951, ..., Dec 2015
ptamonv<-c(t(ptamon))
#Generate time ticks from Jan 1951 to Dec 2015
xtime<-seq(1951, 2016-1/12, 1/12)
# Plot the Tahiti standardized SLP anomalies
plot(xtime, ptamonv,type="l",xlab="Year",
     ylab="Presure Anomalies", cex.lab=1.3, cex.axis=1.3,
     main="Standardized Tahiti SLP Anomalies", col="red",
     xlim=range(xtime), ylim=c(-4,4))
text(1952,3.5, "(a)", cex=1.3)
dev.off()

# Do the same for Darwin SLP
setEPS()
postscript("fig0402b.eps", height=4.9, width=7)
par(mar=c(4.0,4.2,1.5,0.5))
Pda<-read.table("data/PSTANDdarwin.txt", header=F)
pdamon<-Pda[,seq(2,13)]
pdamonv<-c(t(pdamon))
plot(xtime, pdamonv,type="l",cex.lab=1.3, cex.axis=1.3,
     xlab="Year",ylab="Presure", 
     main="Standardized Darwin SLP Anomalies", col="blue",
     xlim=range(xtime), ylim=c(-4,4))
text(1952,3.5, "(b)", cex=1.3)
dev.off()

#Plot the SOI index 
setEPS()
postscript("fig0402c.eps", height=4.9, width=7)
par(mar=c(4.0,4.2,2.0,0.5))
SOI <- ptamonv-pdamonv
plot(xtime, SOI ,type="l", cex.lab=1.3, cex.axis=1.3,
     xlab="Year",ylab="SOI index", 
     col="black",xlim=range(xtime), ylim=c(-6,6), lwd=1)
#Add ticks on top edge of the plot box
axis (3, at=seq(1951,2015,4), labels=seq(1951,2015,4))
#Add ticks on the right edge of the plot box
axis (4, at=seq(-4,4,2), labels=seq(-4,4,2))
lines(xtime, rep(0,length(xtime)))
text(1985, 5, "SOI = Tahiti Pressure - Darwain Pressure", cex=1.3)
text(1952,5.6, "(c)", cex=1.3)
#abline(lm(SOI ~ xtime), col="red", lwd=2)
dev.off()

#CSOI and AMO time series comparison
setEPS()
postscript("fig0402d.eps", height=4.9, width=7)
#par(mar=c(4.0,4.2,1.5,0.5))
par(mar=c(4,4.2,1.6,4))
cnegsoi<--cumsum(ptamonv-pdamonv)
#par(mgp=c(2,2,4,0))
plot(xtime, cnegsoi,type="l", cex.lab=1.3, cex.axis=1.3,
     xlab="Year",ylab="Negative CSOI index", 
     main="CSOI and AMO Index Comparison", 
     col="purple",xlim=range(xtime), ylim=range(cnegsoi), lwd=1.5)
legend(1960,15, col=c("purple"),lty=1,lwd=2.0,
       legend=c("CSOI"),bty="n",text.font=2,cex=1.0)
text(1951,12, "(d)", cex=1.3)
#AMO data and plot
amodat=read.table("data/AMO1951-2015.txt", header=FALSE)
amots=as.vector(t(amodat[,2:13]))
par(new=TRUE)
plot(xtime, amots,type="l",col="darkgreen",
     cex.lab=1.3, cex.axis=1.3,
     lwd=1.5,axes=FALSE,xlab="",ylab="")
legend(1960,0.45, col=c("darkgreen"),lty=1,lwd=2.0,
       legend=c("AMO index"),bty="n",text.font=2,cex=1.0)
#Suppress the axes and assign the y-axis to side 4
axis(4, cex.axis=1.3)
mtext("AMO index",side=4,line=3, cex=1.3)
dev.off()

#Compute the weighted SOI by SVD
#Time-space data format
ptada = cbind(ptamonv,pdamonv)
#Space-time data format
ptada = t(ptada)
#Implement SVD for the sapce-time data
svdptd = svd(ptada)
recontd=svdptd$u%*%diag(svdptd$d[1:2])%*%t(svdptd$v)
recontd=ptada
#Two EOFs from the space-time data
U=svdptd$u
U
#           [,1]      [,2]
#[1,] -0.6146784 0.7887779
#[2,]  0.7887779 0.6146784
#These are the weights for the WSOI
svdptd$d
#[1] 31.34582 22.25421
D=diag(svdptd$d)
D
#         [,1]     [,2]
#[1,] 31.34582  0.00000
#[2,]  0.00000 22.25421

#PCs: The principal components
V=svdptd$v
V
#               [,1]          [,2]
#  [1,] -5.820531e-02  1.017018e-02
#  [2,] -4.026198e-02 -4.419324e-02
#  [3,] -2.743069e-03 -8.276652e-02
#  ......

#Plot Fig. 4.3
U=svdptd$u #The matrix of the EOF spatial patterns
V=svdptd$v #The matrix of the PC tempral patterns
D=diag(svdptd$d) #The diagonal matrix of standard deviations 
recontada=U%*%D%*%t(V) #Verify the SVD decomposition
svd_error = ptada - recontada
max(svd_error)
#[1] 3.996803e-15
min(svd_error)
#[1] -4.218847e-15

#Plot WSOI1
setEPS()
postscript("fig0403a.eps", height=4.9, width=7)
par(mar=c(4,4.2,2.0,0.5))
xtime<-seq(1951, 2016-1/12, 1/12)
wsoi1=D[1,1]*t(V)[1,]
plot(xtime, wsoi1,type="l", cex.lab=1.3, cex.axis=1.3,
     xlab="Year",ylab="WSOI1", 
     col="black",xlim=range(xtime), ylim=c(-4,4))
axis (3, at=seq(1951,2015,4), labels=seq(1951,2015,4))
text(1985,-3.7,"Weighted SOI1 Index", cex=1.5)
text(1952,3.5, "(a)", cex=1.3)
dev.off()

#Plot WSOI2
setEPS()
postscript("fig0403b.eps", height=4.9, width=7)
par(mar=c(4,4.2,2.0,0.5))
xtime<-seq(1951, 2016-1/12, 1/12)
wsoi2=D[2,2]*t(V)[2,]
plot(xtime, wsoi2,type="l", cex.lab=1.3, cex.axis=1.3,
     xlab="Year",ylab="WSOI2", 
     col="blue",xlim=range(xtime), ylim=c(-4,4))
axis (3, at=seq(1951,2015,4), labels=seq(1951,2015,4))
text(1985,-3.7,"Weighted SOI2 Index", cex=1.5)
text(1952,3.5, "(b)", cex=1.3)
dev.off()

setEPS()
postscript("fig0403c.eps", height=4.9, width=7)
par(mar=c(4,4.2,2.0,4.2))
cwsoi1=cumsum(wsoi1)
plot(xtime, cwsoi1,type="l",cex.lab=1.3, cex.axis=1.3,
     xlab="Year",ylab="Weighted SOI 1", 
     col="black",lwd=3, ylim=c(-200,50),
     main="Comparison between CWSOI1 and the Smoothed AMO Index")
text(1951,40, "(c)", cex=1.3)
#axis (3, at=seq(1951,2015,4), labels=seq(1951,2015,4))
legend(1970,20, col=c("black"),lty=1,lwd=3.0,
       legend=c("CWSOI1"),bty="n",text.font=2,cex=1.0)
#Superimpose CSOI time series on this CWSOI1
cnegsoi<--cumsum(ptamonv-pdamonv)
lines(xtime, cnegsoi,type="l",col="purple", lwd=3.0)
legend(1970,2, col=c("purple"),lty=1,lwd=3.0,
       legend=c("CSOI"),bty="n",text.font=2,cex=1.0)
#24-month ahead moving average of the monthly AMO index
amodat=read.table("data/AMO1951-2015.txt", header=FALSE)
amots=as.vector(t(amodat[,2:13]))
#install.packages("TTR")
library("TTR")
amomv=SMA(amots,n=24, fill=NA) 
#Average of the previous n points
par(new=TRUE)
xtime=seq(1951,2015,len=780)
plot(xtime, amomv[37:816],type="l",col="darkgreen",
     lwd=3,axes=FALSE,xlab="",ylab="", 
     ylim=c(-0.5, 0.5))
legend(1970,0.23, col=c("darkgreen"),lty=1,lwd=3.0,
       legend=c("AMO index"),bty="n",text.font=2,cex=1.0)
#Suppress the axes and assign the y-axis to side 4
axis(4, cex.axis=1.3, col.axis="darkgreen")
mtext("Smoothed AMO index",
      side=4,line=3, cex=1.3, col="darkgreen")
dev.off()

#Plot cumulative WSOI2: CWSOI2
setEPS()
postscript("fig0403d.eps", height=4.9, width=7)
par(mar=c(4,4.2,2.0,0.5))
cwsoi1=cumsum(wsoi1)
wsoi2=D[2,2]*t(V)[2,]
cwsoi2=cumsum(wsoi2)
plot.new()
#par(mar=c(4,4,3,4))
plot(xtime, cwsoi2,type="l",xlab="Year",ylab="CWSOI2", 
     col="blue",lwd=3, ylim=c(-200,50),
     main="CWSOI2 Index: The Cumulative PC2")
dev.off()

#Plot Fig. 4.4
#Display the two ENSO modes on a world map
library(maps)
library(mapdata)

plot.new()
par(mfrow=c(2,1))

par(mar=c(0,0,0,0)) #Zero space between (a) and (b)
map(database="world2Hires",ylim=c(-70,70), mar = c(0,0,0,0))
grid(nx=12,ny=6)
points(231, -18,pch=16,cex=2, col="red")
text(231, -30, "Tahiti 0.61", col="red")
points(131, -12,pch=16,cex=2.6, col="blue")
text(131, -24, "Darwin -0.79", col="blue")
axis(2, at=seq(-70,70,20), 
     col.axis="black", tck = -0.05, las=2, line=-0.9,lwd=0)
axis(1, at=seq(0,360,60),  
     col.axis="black",tck = -0.05, las=1, line=-0.9,lwd=0)
text(180,30, "El Nino Southern Oscillation Mode 1",col="purple",cex=1.3)
text(10,-60,"(a)", cex=1.4)
box()

par(mar=c(0,0,0,0)) #Plot mode 2
map(database="world2Hires",ylim=c(-70,70),  mar = c(0,0,0,0))
grid(nx=12,ny=6)
points(231, -18,pch=16,cex=2.6, col="red")
text(231, -30, "Tahiti 0.79", col="red")
points(131, -12,pch=16,cex=2, col="red")
text(131, -24, "Darwin 0.61", col="red")
text(180,30, "El Nino Southern Oscillation Mode 2",col="purple",cex=1.3)
axis(2, at=seq(-70,70,20), 
     col.axis="black", tck = -0.05, las=2, line=-0.9,lwd=0)
axis(1, at=seq(0,360,60),  
     col.axis="black",tck = -0.05, las=1, line=-0.9,lwd=0)
text(10,-60,"(b)", cex=1.4)
box()

#An example of multivariate regression
x1=c(1,2,3) #Given the coordinates of the 3 points
x2=c(2,1,3)
y=c(-1,2,1)
df=data.frame(x1,x2,y) #Put data into the data.frame format
fit <- lm(y ~ x1 + x2, data=df)
fit#Show the regression results
#Call:
#  lm(formula = y ~ x1 + x2, data = df)
#Coefficients:
#  (Intercept)           x1           x2  
#-5.128e-16    1.667e+00   -1.333e+00  

1.667*x1-1.333*x2  #Verify that 3 points determining a plane
#[1] -0.999  2.001  1.002

#Multivariate regression with four data points
u=c(1,2,3,1)
v=c(2,4,3,-1)
w=c(1,-2,3,4)
mydata=data.frame(u,v,w)
myfit <- lm(w ~ u + v, data=mydata)
summary(myfit)#Show the result
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)   1.0000     1.8708   0.535    0.687
#u             2.0000     1.2472   1.604    0.355
#v            -1.5000     0.5528  -2.714    0.225

#Another example of multivariate regregression
dat=matrix(rnorm(40),nrow=10, dimnames=list(c(letters[1:10]), c(LETTERS[23:26])))
fdat=data.frame(dat)
fit=lm(Z~ W + X + Y, data=fdat)
summary(fit)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.36680    0.16529   2.219   0.0683 
#W            0.11977    0.20782   0.576   0.5853  
#X           -0.53277    0.19378  -2.749   0.0333 
#Y           -0.04389    0.14601  -0.301   0.7739 


############################################################################
#
# Chapter 5: Energy Balance Models]{Energy Balance Models for Climate 
#
############################################################################

#Plot Fig. 5.2
#NASA Diviner Data Source:
#http://pds-geosciences.wustl.edu/lro/lro-l-dlre-4-rdr-v1/lrodlr_1001/data/gcp/
setwd("/Users/sshen/climmath")
d19=read.table("data/tbol_snapshot.pbin4d-19.out-180-0.txt",header=FALSE)
dim(d19)
#[1] 259200      3  #259200 grid points at 0.5 lat-lon resolution
#259200=720*360, starting from (-179.75, -89.75) going north
#then back to south pole then going north 
#until the end (179.75, 89.75)
m19=matrix(d19[,3],nrow=360)
dim(m19)
#[1] 360 720

library(maps)
Lat1=seq(-89.75,by=0.5,len=360)
Lon1=seq(-189.75,by=0.5, len=720)
mapmat=t(m19)
#mapmat=pmin(mapmat,10)
#mapmat= mapmat[,seq(length(mapmat[1,]),1)], no flipping
plot.new()
png(filename=paste("Moon Surface Temperature, Snapshot=", 19,".png"), 
    width=800, height=400)
int=seq(0,400,length.out=40)
rgb.palette=colorRampPalette(c('skyblue',  'green', 'blue', 'yellow', 'orange', 
                               'pink','red', 'maroon', 'purple', 'black'),interpolate='spline')
filled.contour(Lon1, Lat1, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title("Moon Surface Temperature Observed by NASA Diviner, Snapshot 19", 
                                xlab="Longitude", ylab="Latitude"),
               plot.axes={axis(1); axis(2);grid()},
               key.title=title(main="deg K"))
dev.off()

#Plot the equator temperature for a snapshot 
plot.new()
png(filename=paste("Moon's Equatorial Temperature at Snapshot", 19,".png"), 
    width=600, height=400)
plot(Lon1,m19[180,],type="l", col="red",lwd=2, 
     xlab="Longitude", ylab="Temperature [deg K]",
     main="Moon's Equatorial Temperature at Snapshot 19")
text(-100,250,"Nighttime",cex=2)
text(80,250,"Daytime",cex=2, col="orange")
dev.off()

#Plot the noon time meridional temperature for a snapshot 
plot.new()
par(mar=c(3.5,4,2,0.5))
png(filename=paste("Moon's Noon Time Meridional Temperature at Snapshot", 19,".png"), 
    width=600, height=400)
plot(Lat1,m19[,540],type="l", col="red",lwd=2, 
     xlab="Latitude", ylab="Temperature [deg K]",
     main="Moon's Noon Time Meridional Temperature at Snapshot 19")
dev.off()

#Compute the bright side average temperature of the moon
bt=d19[129601:259200,]
aw=cos(bt[,2]*pi/180)
wbt=bt[,3]*aw
bta=sum(wbt)/sum(aw)
bta
#[1] 302.7653  deg K

#Compute the dark side average temperature
dt=d19[0:12960,]
aw=cos(dt[,2]*pi/180)
wdt=dt[,3]*aw
dta=sum(wdt)/sum(aw)
dta
#[1] 124.7387  deg K

#Equator noon temperature of the moon from an EBM
lat=0*pi/180
sigma=5.670367*10^(-8)
alpha= 0.12 
S=1368
ep=0.98
k=7.4*10^(-4)
h=0.4
T0=260
fEBM=function(T){(1-alpha)*S*cos(lat*pi/180) -(ep*sigma*T^4 + k*(T-T0)/h)}
#Numerically solve this EBM: fEBM=0
uniroot(fEBM,c(100,420))
#[1] 383.6297

#Plot Fig. 5.5
#Define a piecewise albedo function 
a1=0.7
a2=0.3
T1=250
T2=280
ab= function(T) {ifelse(T < T1, a1, ifelse(T < T2,((a1-a2)/(T1-T2))*(T-T2)+ a2, a2))}
#Define the range of temperature 
T=seq(200,350,len=200)
#Plot the albedo as a nonlinear function of T
setwd("/Users/sshen/climmath")
png(file="fig05-05.png", width=400, heigh=300)
plot(T, ab(T), type="l", lwd=2.0,
     ylim=c(0,1), xlab="Surface Temperature [deg K]",
     ylab="Albedo", main="Nonlinear Albedo Feedback")
dev.off()
#One can plot the albedo function directly without using a function
curve(ifelse(x < 260, 0.7, ifelse(x < 285,-0.016*x+ 4.86, 0.3)), from=200, to=350)

#Plot Fig. 5.6
#Formulate and solve an EBM
S <- 1368
ep <- 0.6
sg <- 5.670373*10^(-8)
T <- seq(200,350, by=0.1)
Ein<-(1-ab(T))*(S/4)
Eout<- ep*sg*T^4
png("fig05-07.png",width=8,height=6, units = 'in', res = 300)
plot(T, Ein, xlim=c(200, 350), ylim=c(0,300),
     xaxp=c(200, 350, 15), yaxp=c(0, 300, 10),
     type="l",col="red", lwd=3,
     panel.first = grid(30, lty = "dotted", lwd = 1),
     main="Simple Nonlinear EBM with Albedo Feedback: Eout = Ein", 
     ylab="Energy [W/sq.m]",
     xlab="Surface temperature T [deg K]")
lines(T, Eout,col="blue",lwd=2.0)
lines(T, Eout-Ein,col="black",lwd=2.0) 
y0<-0.0*T
lines(T, y0,col="purple")
text(310, 248, "Ein", col="red", cex=1.5)
text(290, 275, "Eout", col="blue", cex=1.5)
text(300, 100, "Eout - Ein", col="black", cex=1.5)
text(234,10, "T1", cex=1.5)
text(267,10, "T2", cex=1.5)
text(287,10, "T3", cex=1.5)
points(234, 0, pch=16)
points(263, 0, pch=16)
points(290, 0, pch=16)
dev.off()
# The three intersections of the green and purple lines 
# are three solutions: T1=234, T2=263, T3=290 deg K. 

#Solve an EBM
S<-1368
ep<-0.6
sg<-5.670373*10^(-8)
f <- function(T){return(ep*sg*T^4 - (1-ab(T))*(S/4))}
uniroot(f,c(260,275))
#$root
#[1] 263.4303
uniroot(f,c(275,295))
uniroot(f,c(220,240))

#Bifurcation diagram based an EBM model
#T and solor constant Q relation
png("QT-relation.png",width=6,height=8, units = 'in', res = 200)
q = function(T){return(ep*sg*T^4/ (1-ab(T)))}
plot(q(T),T,type="l", lwd=2, xlim=c(200,700),ylim=c(200,350),
     main="Solar constant and temperature in an EBM",
     ylab="Temperature [deg K]",
     xlab="Solar radiaiton Q=S/4 [W/sq.m]")
Tm=seq(250,280)
lines(q(Tm),Tm,col="red", lwd=3)
dev.off()

############################################################################
#
# Chapter 6: Calculus Applications to Climate Science I: Derivatives
#
############################################################################

#Plot Fig. 6.1
setEPS()
postscript("fig0601.eps", height=8, width=10)
#Cess' Budyko parameters
par(mar=c(4.5,5,2,0.5))
A = 212
B = 1.6
ep = 0.6
sg = 5.670373*10^(-8)
T = seq(-50,50, by=0.1)
S = 1365
alf = 0.30
plot(T,A+B*T, xlim=c(-50,50), ylim=c(0,500), 
     type="l",lwd=4, cex.lab=1.5, cex.axis=1.5,
     main="Radiation Law and Its Approximations",
     xlab="Temperature T [deg C]",
     ylab=expression(paste("Outgoing Radiation: ", E[out], " [W/sq. m]"))
)
lines(T, ep*sg*(273.15+T)^4,col="blue", lwd=3)
lines(T, 189.4 + 2.77*T,type="l", col="purple",lwd=3)
lines(T, (1-alf)*S/4*rep(1, length(T)), col="red",lwd=2)
legend(-50, 510, legend=c("Budyko radiation formula", 
                          "Stefan-Boltzmann radiation law",
                          "Linear Approximation of SB law",
                          "Observed outgoing radiation"),
       col=c("black", "blue","purple","red"),
       lty=1, bty="n",lwd=3, cex=1.4)
text(-40,180, "Budyko formula",cex=1.5)
text(18,310, "Stefan-Boltzmann law", col="blue",cex=1.5)
text(-8,65, "Linear approximation to Stefan-Boltzmann law", 
     col="purple",cex=1.5)
text(-37,253, expression(paste( E[out], "= 239 [W/sq. m]")), 
     col="red", cex=1.5)
dev.off()

#Generate Table 6.1
x <- seq(-0.3,0.3, by=0.1)
fx <- c(1:7)
lx <- c(1:7)
ex <- c(1:7)
for(i in 1:7){fx[i]=(1+x[i])^4
lx[i]=1+4*x[i]
ex[i]=((1+x[i])^4-(1+4*x[i]))/((1+x[i])^4)*100
}
round(cbind(x, fx, lx, ex), digits=4)

#Solve an equation by the bisection method
#install.packages("NLRoot") #if not installed before
library(NLRoot)
func<-function(x){x^3-x-1}
BFfzero(func, 0, 2)
#[1] 1.324716 is the solution

#Another bisection example
f1<-function(x){(1+x)^4-(2+x)}
BFfzero(f1,0,2)
#[1] 0.2207428 is the solution

#Solve an EBM by the bisection method
S <- 1368
ep <- 0.6
sg <- 5.670373*10^(-8)
f2 <- function(T){ep*sg*T^4 - (1-(0.5 - 0.2*tanh((T-265)/10)))*(S/4)}
BFfzero(f2,270,300)
#[1] 289.3097 is the solution

#R code for the Newton's method to solve an equation
newton <- function(f, tol=1E-12,x0=1,N=20) {
  h <- 0.001
  i <- 1; x1 <- x0
  p <- numeric(N)
  while (i<=N) {
    df.dx <- (f(x0+h)-f(x0))/h
    x1 <- (x0 - (f(x0)/df.dx))
    p[i] <- x1
    i <- i + 1
    if (abs(x1-x0) < tol) break
    x0 <- x1
  }
  return(p[1:(i-1)])
}
f <- function(x) { x^3 + 4*x^2 -10 }
root <- newton(f, tol=1E-12, x0=1, N=10)
root
#[1] 1.454256 1.368917 1.365238 1.365230 1.365230 1.365230 1.365230
uniroot(f,c(1,2), tol=10^(-6))

#Plot Fig. 6.2
#Illustration of Newton's method
setEPS()
postscript("fig0602.eps", height=6, width=8)
par(mar=c(0,1,0,0))
x = seq(0.2,1.7, len=30)
f = function(x) { x^3 + 4*x^2 -10 }
g = function(x) { 3*x^2 + 8*x }
plot(x,f(x), type='l', lwd=1.5, bty="n", 
     xaxt="n",yaxt ="n", xlab="", ylab="",
     xlim=c(0,1.8),ylim=c(-10,7))
axis(1, at=c(0, 0.5, 1.0, 1.5, 1.7), pos=0, cex.lab=1.3)
axis(2, at=c(-8, -6, -4, -2, 0, 2, 4), pos=0, cex.lab=1.3, las=1)
arrows(0,0, 1.8,0, angle=10, length=0.25)
arrows(0,-11, 0,7, angle=10, length=0.25)
text(1.8, 0.5, expression(x), cex=1.4)
text(0.05, 7, expression(y), cex=1.4)
text(0.4, -8, expression(y == x^3 + 4*x^2 -10), cex=1.4) 
#Plot the initial points and then follow tangent lines
x0 = 1.0
x1 = 1.4543
x2 = 1.3689
lines(x,f(x0) + g(x0)*(x - x0),type="l", lty=2, col="red")
text(0.73,-8.5, expression(paste("Tangent line through ", P[0])),
     col="red", cex=1.2, srt=42)
points(1,0, pch=19)
text(1.0, 1.3, "Initial guess", cex=1.4, col="blue")
text(1.0, 0.5, expression(x[0]), cex=1.4, col="blue")
arrows(1,0,1,-5, angle=10, length=0.15, col="blue")
points(x0, f(x0), col="red", pch=20)
text(x0+ 0.02, f(x0) - 0.5, expression(P[0]), cex=1.4, col="red")
arrows(x0,f(x0), x1,0, angle=10, length=0.15, col="blue")
points(x1,0, pch=19)
text(x1+0.04, 0.3, expression(x[1]), cex=1.4, col="blue")
arrows(x1,0,x1,f(x1), angle=10, length=0.15, col="blue")
points(x1, f(x1), col="red", pch=20)
text(x1+ 0.05, f(x1), expression(P[1]), cex=1.4, col="red")
lines(x,f(x1) + g(x1)*(x - x1),type="l", lty=2, col="red")
text(1.05,-6.5, expression(paste("Tangent line through ", P[1])),
     col="red", cex=1.2, srt=56)
arrows(x1,f(x1),x2,0, angle=10, length=0.15, col="blue")
points(x2,0, pch=19)
text(1.34,0.5, expression(x[2]), cex=1.4, col="blue")
text(0.8, 6, "Newton's method for finding a root:", cex=1.8)
text(0.8, 5,"Follow the blue arrows from the initial guess", 
     cex=1.3, col="blue")
text(0.8, 4, expression(x[0] %=>% P[0] %=>% x[1] %=>% P[1] %=>% x[2] %=>% ...), 
     cex=1.3, col="blue")                            
dev.off()

#Use Newton's method to solve an equation
f = function(x) { (x+1)^4 -(2+x) }
root <- newton(f, tol=1E-12, x0=1, N=10)
root

#Find multiple solutions of an EBM
S <- 1365
ep <- 0.6
sg <- 5.670373*10^(-8)
#Define the function for energy
f3 <- function(T){return(ep*sg*T^4 - 
                           (1-(0.5 - 0.2 * tanh ((T-265)/10)))*(S/4))}
root1 <- newton(f3, tol=1E-12, x0=220,N=20)
root1
#  [1] 235.6965 234.3860 234.3817 234.3817 234.3817
root2 <- newton(f3, tol=1E-12, x0=270,N=20)
root2
#[1] 262.0567 264.5071 264.3378 264.3377 264.3377 264.3377
root3 <- newton(f3, tol=1E-12, x0=300,N=20)
root3
#[1] 289.9086 289.1469 289.1401 289.1401 289.1401 289.1401

root5 <- newton(f3, tol=1E-12, x0=100, N=20)
root5
#[1] 827.2544 623.5417 474.8968 372.5619 313.3648 292.0552 289.2231
#[8] 289.1402 289.1401 289.1401 289.1401

#Use R to find derivatives of a function symbolically
D(expression(x^2),"x")
# 2 * x

D(expression(exp(-x^2)),"x")
# -(exp(-x^2) * (2 * x))

D(expression(sin(-3*t)-2*cos(4*t-0.3*pi)),"t")
# -(cos(-3 * t) * 3 - 2 * (sin(4 * t - 0.3 * pi) * 4))

D(expression(-g*t^2/2 + v0*t + h0),"t")
# v0 - g * (2 * t)/2
# Find derivative of this result function to find the second derivative
D(expression(v0 - g * (2 * t)/2),"t")
# -(g * 2/2)
#or simply
D(D(expression(-g*t^2/2 + v0*t + h0),"t"),"t")
# -(g * 2/2)
#The third-order derivative
D(D(D(expression(-g*t^2/2 + v0*t + h0),"t"),"t"),"t")
#[1] 0


############################################################################
#
# Chapter 7: Calculus Applications to Climate Science II: Integrals
#
############################################################################

#Plot Fig. 7.3: Corpus Christi: Elevation vs. F=g/(RT)
setwd("/Users/sshen/climmath")
dat=read.table("data/CorpsCristiTX13295R1.txt", header=FALSE)
dim(dat)
plot.new()
da2=dat
for(i in 1:146){if(dat[i,4] >9999){da2[i,4]=NA} }
R=287.055 #[J/(kg K)]
g=9.8 #[m/sec^2]
  
par(mar=c(4.2,4.2,3,0.5))
plot(da2[,3],(g/R)/(273.15+da2[,4]/10),type="l", 
     ylim=c(0,0.0002), lwd=8,col="red",
     xlab="Elevation: z [m]", ylab="F = g/(RT)",
     main="Hypsometric Equation Integrand Based on the Radiosonde Data 
     Corpus Christi, Texas, 0:00UTC, 12 September 2013",
     cex.axis=1.3, cex.lab=1.2)
lines(c(14, 9992),c((g/R)/(273.15+26.8),(g/R)/(273.15-34.1)), lwd=2, col="blue")
lines(c(14, 14),c(0,(g/R)/(273.15+26.8)), lwd=2, col="blue")
lines(c(9992, 9992),c(0,(g/R)/(273.15-34.1)), lwd=2, col="blue")
lines(c(14, 9992),c(0,0), lwd=2, col="blue")
text(1200,0.000075,"F1", cex=1.2)
text(11500,0.000075,"F2", cex=1.2)
text(1500,0.000007,"z1", cex=1.2)
text(11500,0.000007,"z2", cex=1.2)
points(c(14,10000,14,10000), 
       c(0,0,(g/R)/(273.15+26.8),(g/R)/(273.15-34.1)), 
       pch=19, cex=0.6)


#Plot Fig. 7.5
#Planck's law of blackbody radiation for the Sun and Earth
setwd("/Users/sshen/climmath")
#Solar radiation
setEPS()
postscript("fig0705a.eps", height=10, width=8)
h=6.626070040
k=1.38064852
c=3
x=seq(0.01,3,len=2000)
et2=function(x){(2*h*c^2/x^5)/(exp(h*c/(k*x*T))-1)}
plot(1, lwd=0, xlim=c(0,3), ylim=c(0,30))
par(mar=c(4,5.0,1.5,0.5))
plot(1,type="l",lwd=0, xlim=c(0,3), ylim=c(0,30),
     main="Planck's Law of Blackbody Radiation Around 5,000 deg K",
     xlab=expression(paste("Wavelength [", mu, "m ]")),
     ylab=expression(paste("Spectral power flux of radiation: ", "[kW/(", m^2, " nm)]")),
     cex.lab=1.8, cex.axis=1.6, cex.main=1.5)
segments(0.41,0,0.41,30,lwd=4,col="purple")
segments(0.43,0,0.43,30,lwd=6,col="blue4")
segments(0.46,0,0.47,30,lwd=10,col="blue")
segments(0.52,0,0.52,30,lwd=14,col="green")
segments(0.57,0,0.57,30,lwd=8,col="yellow")
segments(0.61,0,0.61,30,lwd=9,col="orange")
segments(0.69,0,0.69,30,lwd=24,col="red")
text(1.1,-0.6, "infrared", cex=1.6)
text(0.63,-0.6, "visible", cex=1.6)
text(0.15,-0.6, "ultraviolet", cex=1.6)
T=5.0
lines(x,et2(x), type="l", col="darkolivegreen2", lwd=2)
T=5.772
lines(x,et2(x), type="l", col="blue4", lwd=2)
T=4.0
lines(x,et2(x), type="l", col="brown3", lwd=2)
legend(1.5,25, 
       legend=c("T=5772 deg K","T=5000 deg K", "T=4000 deg K" ), 
       col=c("blue4","darkolivegreen2","brown3"),
       lty=1, lwd=3,
       cex=1.5)
dev.off()

#Earth radiation
setEPS()
postscript("fig0705b.eps", height=10, width=8)
par(mar=c(4,5.0,1.5,0.5))
h=6.626070040
k=1.38064852
c=3
et=function(x){10^6*(2*h*c^2/x^5)/(exp(h*c/(k*x*T))-1)}
x=seq(0.01,80,len=2000)
T=0.288
plot(x,et(x), type="l", col="black", lwd=2,
     main="Planck's Law of Blackbody Radiation Around 15 deg C",
     xlab=expression(paste("Wavelength [", mu, "m ]")),
     ylab=expression(paste("Spectral power flux of radiation: ", 10^-6, "[kW/(", m^2, " nm)]")),
     cex.lab=1.8, cex.axis=1.6, cex.main=1.5, ylim=c(0,10))
T=0.290
lines(x,et(x), type="l", lty=3, lwd=2)
T=0.286
lines(x,et(x), type="l", lty=2, lwd=2)
legend(40,9, 
       legend=c("T=15+2 deg C","T=15     deg C", "T=15-2 deg C" ), 
       #       col=c("red","black","blue"),
       lty=c(3,1,2), lwd=3,
       cex=1.5)
dev.off()


#Put the two figures together
#Solar and Earth radiation
h=6.626070040
k=1.38064852
c=3
x=seq(0.01,80,len=2000)
et=function(x){10^6*(2*h*c^2/x^5)/(exp(h*c/(k*x*T))-1)}
#setEPS()
#postscript("fig0705.eps", height=8, width=8)
par(mfrow=c(1,2))
#Solar radiation
h=6.626070040
k=1.38064852
c=3
x=seq(0.01,3,len=2000)
et2=function(x){(2*h*c^2/x^5)/(exp(h*c/(k*x*T))-1)}
#setEPS()
#postscript("fig0705b.eps", height=8, width=8)
#Solar radiation
par(mar=c(4,5.0,1.5,0.5))
plot(1,type="l",lwd=0, xlim=c(0,3), ylim=c(0,30),
     main="Planck's Law of Blackbody Radiation Around 5,000 deg C",
     xlab=expression(paste("Wavelength [", mu, "m ]")),
     ylab=expression(paste("Spectral power density: ", "[kW/(", m^2, " nm)]")),
     cex.lab=1.4, cex.axis=1.4)
segments(0.41,0,0.41,30,lwd=4,col="purple")
segments(0.43,0,0.43,30,lwd=4,col="blue4")
segments(0.46,0,0.47,30,lwd=10,col="blue")
segments(0.52,0,0.52,30,lwd=14,col="green")
segments(0.57,0,0.57,30,lwd=8,col="yellow")
segments(0.61,0,0.61,30,lwd=8,col="orange")
segments(0.69,0,0.69,30,lwd=24,col="red")
text(1.0,-0.4, "infrared", cex=1.2)
text(0.6,-0.4, "visible", cex=1.2)
text(0.1,-0.4, "ultraviolet", cex=1.2)
T=5.0
lines(x,et2(x), type="l", col="black", lwd=2)
T=5.772
lines(x,et2(x), type="l", col="red", lwd=2)
T=4.0
lines(x,et2(x), type="l", col="blue", lwd=2)
legend(1.5,25, 
       legend=c("T=5772 deg C","T=5000 deg C", "T=4000 deg C" ), 
       col=c("red","black","blue"),
       lty=1, lwd=3,
       cex=1.5)
#dev.off()

#Earth radiation
par(mar=c(4,5.0,1.5,0.5))
x=seq(0.01,80,len=2000)
T=0.288
plot(x,et(x), type="l", col="black", lwd=2,
     main="Planck's Law of Blackbody Radiation Around 15 deg C",
     xlab=expression(paste("Wavelength [", mu, "m ]")),
     ylab=expression(paste("Spectral power density: ", 10^-6, "[kW/(", m^2, " nm)]")),
     cex.lab=1.4, cex.axis=1.4, ylim=c(0,10))
T=0.290
lines(x,et(x), type="l", col="red", lwd=2)
T=0.286
lines(x,et(x), type="l", col="blue", lwd=2)
legend(40,9, 
       legend=c("T=15+2 deg C","T=15     deg C", "T=15-2 deg C" ), 
       col=c("red","black","blue"),
       lty=1, lwd=3,
       cex=1.5)
#dev.off()


############################################################################
#
# Chapter 8: Conservation Laws in Climate Dynamics
#
############################################################################

#This chapter does not contain any R codes.



############################################################################
#
# Chapter 9: R Graphics for Climate Science
#
############################################################################

#Plot Fig. 9.1
setEPS()
postscript("fig0901.eps", height=4, width=8)
par(mar=c(4.0,4.2,1.8,4.1))
Time <- 2001:2010
Tmean <- c(12.06, 11.78,11.81,11.72,12.02,12.36,12.03,11.27,11.33,11.66)
Prec <- c(737.11,737.87,774.95,844.55,764.03,757.43,741.17,793.50,820.42,796.80)
plot(Time,Tmean,type="o",col="red", lwd=1.5, xlab="Year",
     ylab=expression(paste("Tmean [", degree,"C]")),
     main="Contiguous U.S. Annual Mean Temperature and Total Precipitation")
legend(2000.5,12.42, col=c("red"),lty=1,lwd=2.0,
       legend=c("Tmean"),bty="n",text.font=2,cex=1.0)
#Allows a figure to be overlaid on the first plot
par(new=TRUE)
plot(Time, Prec,type="o",col="blue",lwd=1.5,axes=FALSE,xlab="",ylab="")
legend(2000.5,839, col=c("blue"),lty=1,lwd=2.0,
       legend=c("Prec"),bty="n",text.font=2,cex=1.0)
#Suppress the axes and assign the y-axis to side 4
axis(4)
mtext("Precipitation [mm]",side=4,line=3)
#legend("topleft",col=c("red","blue"),lty=1,legend=c("Tmean","Prec"),cex=0.6)
#Plot two legends at the same time make it difficult to adjust the font size
#because of different scale
dev.off()

#Plot Fig. 9.2
#Margins, math symbol, and figure setups
setEPS()
postscript("fig0902.eps", height=4, width=8)
#Margins, math symbol, and figure setups
par(mar=c(5,4.5,2.5,2.5))
x<-0.25*(-30:30)
y<-sin(x)
x1<-x[which(sin(x) >=0)] 
y1<-sin(x1)
x2<-x[which(sin(x) < 0)]
y2<-sin(x2)
plot(x1,y1,xaxt="n", xlab="",ylab="",lty=1,type="h",
     lwd=3, tck=-0.02, ylim=c(-1,1), col="red",
     col.lab="purple",cex.axis=1.4)
lines(x2,y2,xaxt="n", xlab="",ylab="",lty=3,type="h",
      col="blue",lwd=8, tck=-0.02)
axis(1, at=seq(-6,6,2),line=3, cex.axis=1.8)
axis(4, at=seq(-1,1,0.5), lab=c("A", "B", "C", "D","E"), 
     cex.axis=2,las=2)
text(0,0.7,font=3,cex=6, "Sine waves", col="darkgreen") #Itatlic font
mtext(side=2,line=2, expression(y==sin(theta-hat(phi))),cex=1.5, col="blue")
mtext(font=2,"Text outside of the figure on side 3",side=3,line=1, cex=1.5)#Bold font
mtext(font=1, side=1,line=1, 
      expression(paste("Angle in radians: ", 
                       theta-phi[0])),cex=1.5, col="red")
dev.off()

#Plot Fig. 9.3
par(mar=c(8,6,3,2))
par(mgp=c(2.5,1,0))
plot(1:200/20, rnorm(200),sub="Subtitle: 200 random values",
     xlab= "Time", ylab="Random values", main="Normal random values", 
     cex.lab=1.5, cex.axis=2, cex.main=2.5, cex.sub=2.0)

#Adjust positions of axis labels
par(mgp=c(2,1,0))
plot(sin,xlim=c(10,20))

#Plot Fig. 9.4
#A fancy plot of the NOAAGlobalTemp time series
setwd("/Users/sshen/climmath")
NOAATemp = read.table("data/aravg.ann.land_ocean.90S.90N.v4.0.1.2016.txt", header=F)
par(mar=c(4,4,3,1))
x<-NOAATemp[,1]
y<-NOAATemp[,2]
z<-rep(-99,length(x))
for (i in 3:length(x)-2) z[i]=mean(c(y[i-2],y[i-1],y[i],y[i+1],y[i+2]))
n1<-which(y>=0)
x1<-x[n1]
y1<-y[n1]
n2<-which(y<0)
x2<-x[n2]
y2<-y[n2]
x3<-x[2:length(x)-2]
y3<-z[2:length(x)-2]
plot(x1,y1,type="h",xlim=c(1880,2016),lwd=3, 
     tck=0.02, ylim=c(-0.7,0.7), #tck>0 makes ticks inside the plot
     ylab="Temperature [deg C]",
     xlab="Time",col="red",
     main="NOAA Global Average Annual Mean Temperature Anomalies")
lines(x2,y2,type="h",
      lwd=3, tck=-0.02,  col="blue")
lines(x3,y3,lwd=2)

#Plot Fig. 9.5
#Plot US temp and prec times series on the same figure
par(mfrow=c(2,1))
par(mar=c(0,5,3,1)) #Zero space between (a) and (b)
Time <- 2001:2010
Tmean <- c(12.06, 11.78,11.81,11.72,12.02,12.36,12.03,11.27,11.33,11.66)
Prec <- c(737.11,737.87,774.95,844.55,764.03,757.43,741.17,793.50,820.42,796.80)
plot(Time,Tmean,type="o",col="red",xaxt="n", xlab="",ylab="Tmean [dec C]")
text(2006, 12,font=2,"US Annual Mean Temperature", cex=1.5)
text(2001.5,12.25,"(a)")
#Plot the panel on row 2
par(mar=c(3,5,0,1))
plot(Time, Prec,type="o",col="blue",xlab="Time",ylab="Prec [mm]")
text(2006, 800, font=2, "US Annual Total Precipitation", cex=1.5)
text(2001.5,840,"(b)")

#Figure layout for multiple panels
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), 
       widths=c(3,3), heights=c(2,2))
plot(sin,type="l", xlim=c(0,20))
plot(sin,xlim=c(0,10))
plot(sin,xlim=c(10,20))

#Contours and color-filled contours
x <- y <- seq(-1, 1, len=25)
z <- matrix(rnorm(25*25),nrow=25)
contour(x,y,z, main="Contour Plot of Normal Random Values")
filled.contour(x,y,z, main="Filled Contour Plot of Normal Random Values")
filled.contour(x,y,z, color.palette = heat.colors)
filled.contour(x,y,z, color.palette = colorRampPalette(c("red", "white", "blue")))

#Plot Fig. 9.6
#Plot a 5-by-5 grid global map of standard normal random values
library(maps)
plot.new()
#Step 1: Generate a 5-by-5 grid (pole-to-pole, lon 0 to 355)
Lat<-seq(-90,90,length=37) #Must increasing
Lon<-seq(0,355,length=72) #Must increasing
#Generate the random values
mapdat<-matrix(rnorm(72*37),nrow=72)
#The matrix uses lon as row going and lat as column
#Each row includes data from south to north 
#Define color
int=seq(-3,3,length.out=81)
rgb.palette=colorRampPalette(c('black','purple','blue','white', 
                               'green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
#Plot the values on the world map                         
filled.contour(Lon, Lat, mapdat, color.palette=rgb.palette, levels=int,
               plot.title=title(xlab="Longitude", ylab="Latitude",
                                main="Standard Normal Random Values on a World Map: 5 Lat-Lon Grid"),
               plot.axes={ axis(1); axis(2);map('world2', add=TRUE);grid()}
)
#filled.contour() is a contour plot on an x-y grid. 
#Background maps are added later in plot.axes={}
#axis(1) means ticks on the lower side 
#axis(2) means ticks on the left side 

#Plot Fig. 9.7
#Plot a 5-by-5 grid regional map to cover USA and Canada
Lat3<-seq(10,70,length=13)
Lon3<-seq(230,295,length=14)
mapdat<-matrix(rnorm(13*14),nrow=14)
int=seq(-3,3,length.out=81)
rgb.palette=colorRampPalette(c('black','purple','blue','white', 
                               'green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
filled.contour(Lon3, Lat3, mapdat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="Standard Normal Random Values over Canada and USA: 5-deg Lat-Lon Grid",
                                xlab="Lon", ylab="Lat"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()})

#Plot Fig. 9.8
#R plot of NCEP/NCAR Reanalysis PSD monthly temp data .nc file
#http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.
#reanalysis.derived.surface.html
#.nc is the netCDF data format. It is a Network Common Data Form
#started in 1989 at UCAR, and in the 2010s netCDF became the dominant 
#data format for writing big climate data.

#rm(list=ls(all=TRUE))
setwd("/Users/sshen/Desktop/Papers/KarlTom/Recon2016/Test-with-Gregori-prec-data")

# Download netCDF file
# Library
#install.packages("ncdf")
library(ncdf4)

# 4 dimensions: lon,lat,level,time
nc=ncdf4::nc_open("air.mon.mean.nc")
nc
nc$dim$lon$vals # output values 0.0->357.5
nc$dim$lat$vals #output values 90->-90
nc$dim$time$vals
#nc$dim$time$units
#nc$dim$level$vals
Lon <- ncvar_get(nc, "lon")
Lat1 <- ncvar_get(nc, "lat")
Time<- ncvar_get(nc, "time")
head(Time)
#[1] 65378 65409 65437 65468 65498 65529
library(chron)
month.day.year(1297320/24,c(month = 1, day = 1, year = 1800))
#1948-01-01
precnc<- ncvar_get(nc, "air")
dim(precnc)
#[1] 144  73 826, i.e., 826 months=1948-01 to 2016-10, 68 years 10 mons
#plot a 90S-90N precip along a meridional line at 160E over Pacific
par(mar=c(4.5,5,3,1))
plot(seq(-90,90,length=73), precnc[65,,1], 
     type="l", xlab="Latitude", 
     ylab="Temperature  [deg C]",
     main="90S-90N Temperature [degree C] 
     along a meridional line at 160E: January 1948",
     lwd=3, cex.lab=1.5, cex.axis=1.5)

#Plot Fig. 9.9
#Verify previous calculutions and plots in Fig. 9.9
#Compute and plot climatology and standard deviation Jan 1948-Dec 2015
par(mar=c(3.2,3.3,2.2,0))
library(maps)
climmat=matrix(0,nrow=144,ncol=73)
sdmat=matrix(0,nrow=144,ncol=73)
Jmon<-12*seq(0,67,1)+1
for (i in 1:144){
  for (j in 1:73) {climmat[i,j]=mean(precnc[i,j,Jmon]); 
  sdmat[i,j]=sd(precnc[i,j,Jmon]) 
  }
}
mapmat=climmat
#Note that R requires coordinates increasing from south to north -90->90
#and from west to east from 0->360. We must arrange Lat and Lon this way.
#Correspondingly, we have to flip the data matrix left to right according to 
#the data matrix precnc[i,j,]: 360 (i.e. 180W) lon and from North Pole 
#and South Pole, then lon 178.75W, 176.75W, ..., 0E. This puts Greenwich 
#at the center, China on the right, and USA on the left. However, our map should 
#have the Pacific at the center, and USA on the right. Thus, we make a flip.
Lat=-Lat1
mapmat= mapmat[,length(mapmat[1,]):1]#Matrix flip around a column
#mapmat= t(apply(t(mapmat),2,rev))
int=seq(-50,50,length.out=81)
rgb.palette=colorRampPalette(c('black','blue','darkgreen','green', 
                               'white','yellow','pink','red','maroon'),interpolate='spline')
par(cex.axis=1.3,cex.lab=1.3)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="NCEP RA 1948-2015 January climatology [deg C]",
                                xlab="Longitude",ylab="Latitude"),
               plot.axes={axis(1); axis(2); map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"))

#plot standard deviation
#plot.new()
par(mgp=c(2,1,0))
par(mar=c(3.2,3.3,2.2,0))
par(cex.axis=1.3,cex.lab=1.3)
mapmat= sdmat[,seq(length(sdmat[1,]),1)]
mapmat=pmax(pmin(mapmat,6),0)
int=seq(0,6,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 'green','yellow','pink','red','maroon'),
                             interpolate='spline')
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="NCEP 1948-2015 Jan SAT RA Standard Deviation [deg C]",
                                xlab="Longitude", ylab="Latitude"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"))


#Plot 9.10
#Plot the January 1983 temperature using the above setup
mapmat83J=precnc[,,421]
mapmat83J= mapmat83J[,length(mapmat83J[1,]):1]
int=seq(-50,50,length.out=81)
rgb.palette=colorRampPalette(c('black','blue','darkgreen',
                               'green', 'white','yellow','pink','red','maroon'),interpolate='spline')
filled.contour(Lon, Lat, mapmat83J, color.palette=rgb.palette, levels=int,
               plot.title=title(main="January 1983 surface air temperature [deg C]",
                                xlab="Longitude",ylab="Latitude"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"))

#Plot Fig. 9.12
#Zoom in to a specific lat-lon region: Pacific
int=seq(-6,6,length.out=81)
rgb.palette=colorRampPalette(c('black','blue','darkgreen','green', 
                               'white','yellow','pink','red','maroon'), interpolate='spline')
matdiff = precnc[,,421] -climmat
matdiff= matdiff[,length(matdiff[1,]):1]
matdiff=pmax(pmin(matdiff,6),-6)

filled.contour(Lon, Lat, matdiff, 
               xlim=c(100,300), ylim=c(-50,50), 
               color.palette=rgb.palette, levels=int,
               plot.title=title(
                 main="January 1983 surface air temperature anomaly [deg C]",
                 xlab="Longitude",ylab="Latitude"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"))

#Plot Fig. 9.13
#Wind directions due to the balance between PGF and Coriolis force
#using an arrow plot for vector fields on a map
library(fields)
library(maps)
library(mapproj)

lat<-rep(seq(-75,75,len=6),12)
lon<-rep(seq(-165,165,len=12),each=6)
x<-lon
y<-lat
u<- rep(c(-1,1,-1,-1,1,-1), 12)
v<- rep(c(1,-1,1,-1,1,-1), 12)
par(mfrow=c(1,1))
par(mar=c(3,3,2,0.5))
wmap<-map(database="world", boundary=TRUE, interior=TRUE)
grid(nx=12,ny=6)
#map.grid(wmap,col=3,nx=12,ny=6,label=TRUE,lty=2)
points(lon, lat,pch=16,cex=0.8)
arrow.plot(lon,lat,u,v, arrow.ex=.08, length=.08, col='blue', lwd=2) 
box()
axis(1, at=seq(-165,135,60), lab=c("165W","105W","45W","15E","75E","135E"), 
     col.axis="black",tck = -0.05, las=1, line=-0.9,lwd=0)
axis(1, at=seq(-165,135,60), 
     col.axis="black",tck = 0.05, las=1, labels = NA)
axis(2, at=seq(-75,75,30),lab=c("75S","45S","15S","15N","45N","75N"), 
     col.axis="black", tck = -0.05, las=2, line=-0.9,lwd=0)
axis(2, at=seq(-75,75,30),
     col.axis="black", tck = 0.05, las=1, labels = NA)
text(0, 0, "Intertropical Convergence Zone (ITCZ)", col="orange")
text(0, 30, "Subtropical High", col="orange")
text(0, -30, "Subtropical High", col="orange")
mtext(side=3, "Polar High", col="orange", line=0.0)

#Plot Fig. 9.14
#Plot the wind field over the ocean 
#Ref: https://rpubs.com/alobo/vectorplot
#Agustin.Lobo@ictja.csic.es
#20140428

library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)

#install.packages("rasterVis")
#install.packages("latticeExtra")
library(latticeExtra)
library(rasterVis)

#install.packages("raster")
library(raster)
library(sp)
library(rgdal)

#download.file("ftp://eclipse.ncdc.noaa.gov/pub/seawinds/SI/uv/clm/uvclm95to05.nc", 
#              "uvclm95to05.nc", method = "curl")
setwd("/Users/sshen/climmath")
mincwind <- nc_open("data/uvclm95to05.nc")

length(mincwind)
#[1] 14
u <- ncvar_get(mincwind, "u")
dim(u)
#[1] 1440  719   12 #lon, lat, and month
v <- ncvar_get(mincwind, "v")
dim(v)
u9 <- raster(t(u[, , 9])[ncol(u):1, ])
v9 <- raster(t(v[, , 9])[ncol(v):1, ])

filled.contour(u[, , 9])
filled.contour(u[, , 9],color.palette = heat.colors)
filled.contour(u[, , 9],color.palette = colorRampPalette(c("red", "white", "blue")))
contourplot(u[, , 9])

u9 <- raster(t(u[, , 9])[ncol(u):1, ])
v9 <- raster(t(v[, , 9])[ncol(v):1, ])
w <- brick(u9, v9)
wlon <- ncvar_get(mincwind, "lon")
wlat <- ncvar_get(mincwind, "lat")
range(wlon)
range(wlat)

projection(w) <- CRS("+init=epsg:4326")
extent(w) <- c(min(wlon), max(wlon), min(wlat), max(wlat))

plot(w[[1]])
plot(w[[2]])

vectorplot(w * 10, isField = "dXY", region = FALSE, margin = FALSE, narrows = 10000)
slope <- sqrt(w[[1]]^2 + w[[2]]^2)
aspect <- atan2(w[[1]], w[[2]])
vectorplot(w*6, isField = "dXY", region = slope, 
           margin = FALSE, 
           par.settings=BuRdTheme,
           narrows = 10000, at = 0:10)
#vectorplot(stack(slope * 10, aspect), isField = TRUE, region = FALSE, margin = FALSE)

#Plot Fig. 9.15
#ggplot for USA States
library(ggplot2)
states <- map_data("state") #"states" is in a data.frame
p<- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), 
               color = "white") + 
  coord_fixed(1.3) 
#if fill=FALSE, the large color legend on the right is off.
p<- p + xlab("Longitude")+ ylab("Latitude") 
p + ggtitle("Color Map of the 48 Lower States")

#Plot Fig. 9.16 and animation
#Free fall animation by 21 frames
g=9.8
n=21
t=seq(0,10,len=n)
#install.packages("animation")
library(animation)
## set up an empty frame, then add points one by one
par(bg = "white") # ensure the background color is white
ani.record(reset = TRUE) # clear history before recording
for (i in 1:n) {
  plot(0, 490-(1/2)*g*(t[i])^2, pch=19, lwd=12, col="black", 
       xlab="Horizontal location", xlim=c(-10,10),
       ylim=c(0,500), ylab="Vertical Position [m]",
       main=paste("Free Fall Time=", format(t[i],digits = 2, nsmall=1), "sec")
  )
  ani.record() # is: function (reset = FALSE, replay.cur = FALSE) 
}
## Now we can replay it, with an appropriate pause between frames:
## Smaller interval means faster animation. Default: interval=1
oopts = ani.options(interval = 0.5, 
                    ani.width=200, 
                    ani.height=400,
                    title="Free Fall"
)
#Animate the frames in the plot window of R Studio
ani.replay() #is: function (list) 
## Show the animation on an HTML page
saveHTML(ani.replay(), img.name = "Fall_animation")


############################################################################
#
# Chapter 10: Advanced R Analysis and Plotting: EOFs, Trends, and Global Data
#
############################################################################


#Fig. 10.1
x<-seq(0, 2*pi, len=100)
y<-seq(0, 2*pi, len=100)
mydat<-array(0,dim=c(100,100,10))
for(t in 1:10){
  z<-function(x,y){z=sin(t)*(1/pi)*sin(x)*sin(y)+exp(-0.3*t)*(1/pi)*sin(8*x)*sin(8*y)}
  mydat[,,t]=outer(x,y,z)
}

#Plot the original z(x,y,t) waves for a given t
filled.contour(x,y,mydat[,,10], color.palette =rainbow, 
               plot.title=title(main="Orignal field at t=10", 
                                xlab="x", ylab="y", cex.lab=1.0),
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(0,3*pi, by = 1), cex=1.0) 
                 axis(2,seq(0, 2*pi, by = 1), cex=1.0)}
)

#Space-time data decomposition by SVD
da1<- matrix(0,nrow=length(x)*length(y),ncol=10)
for (i in 1:10) {da1[,i]=c(t(mydat[,,i]))}
da2<-svd(da1)
uda2<-da2$u
vda2<-da2$v
dda2<-da2$d
dda2
#[1] 3.589047e+01 1.596154e+01 7.764115e-14 6.081008e-14

#Plot Fig. 10.2
#Plot EOF modes
#Plot EOF1
par(mar=c(4,4,2.5,4))
par(mgp=c(2,1,0))
int=seq(-0.3,0.3,length.out=21)
rgb.palette=colorRampPalette(c('black','purple','blue','white', 
                               'green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
filled.contour(x,y,matrix(-uda2[,1],nrow=100), color.palette =rgb.palette,
               plot.title=title(main="SVD Mode 1: EOF1", 
                                xlab="x", ylab="y", cex.lab=1.0),
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(0,2*pi, by = 1), cex=1.0) 
                 axis(2,seq(0, 2*pi, by = 1), cex=1.0)})
#Plot EOF2
par(mar=c(4,4,2.5,4))
par(mgp=c(2,1,0))
int=seq(-0.3,0.3,length.out=21)
rgb.palette=colorRampPalette(c('black','purple','blue','white', 
                               'green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
filled.contour(x,y,matrix(-uda2[,2],nrow=100), color.palette =rgb.palette,
               plot.title=title(main="SVD Mode 2: EOF2", 
                                xlab="x", ylab="y", cex.lab=1.0),
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(0,2*pi, by = 1), cex=1.0) 
                 axis(2,seq(0, 2*pi, by = 1), cex=1.0)})

#Plot the theoretical orthogonal modes
z1 <- function(x,y){(1/pi)*sin(x)*sin(y)}
z2 <- function(x,y){(1/pi)*sin(8*x)*sin(8*y)}
fcn1<-outer(x,y,z1)
fcn2<-outer(x,y,z2)
#Plot accurate Mode 1
par(mar=c(4,4,2.5,4))
par(mgp=c(2,1,0))
int=seq(-0.3,0.3,length.out=21)
rgb.palette=colorRampPalette(c('black','purple','blue','white', 
                               'green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
#color.palette =rainbow
filled.contour(x,y,fcn1, color.palette =rgb.palette,
               plot.title=title(main="Accurate Mode 1", 
                                xlab="x", ylab="y", cex.lab=1.0),
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(0,3*pi, by = 1), cex=1.0) 
                 axis(2,seq(0, 2*pi, by = 1), cex=1.0)}
)

#Plot accurate Mode 2
par(mar=c(4,4,2.5,4))
par(mgp=c(2,1,0))
int=seq(-0.3,0.3,length.out=21)
rgb.palette=colorRampPalette(c('black','purple','blue','white', 
                               'green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
filled.contour(x,y,fcn2, color.palette =rgb.palette, 
               plot.title=title(main="Accurate Mode 2", 
                                xlab="x", ylab="y", cex.lab=1.0),
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(0,3*pi, by = 1), cex=1.0) 
                 axis(2,seq(0, 2*pi, by = 1), cex=1.0)}
)
#Plot Fig. 10.3
#Plot PCs and coefficients of the functional patterns
t=1:10
plot(1:10, vda2[,1],type="o", ylim=c(-1,1), lwd=2, 
     ylab="PC or Coefficient", xlab="Time",
     main="SVD PCs vs. Accurate Temporal Coefficients")
legend(0.5,1.15, lty=1, legend=c("PC1: 69% variance"), 
       bty="n",col=c("black"))
lines(1:10, vda2[,2],type="o", col="red", lwd=2)
legend(0.5,1.0, lty=1, legend=c("PC2: 31% varance"), 
       col="red", bty="n",text.col=c("red"))
lines(t, -sin(t), col="blue", type="o")
legend(0.5,0.85, lty=1, legend=c("Mode 1 coefficient: 80% variance"), 
       col="blue", bty="n",text.col="blue")
lines(t, -exp(-0.3*t), type="o",col="purple")
legend(0.5,0.70, lty=1, legend=c("Mode 2 coefficient: 20% variance"), 
       col="purple", bty="n",text.col="purple")

#Verify orthogonality of PCs 
t(vda2[,1])%*%vda2[,2]
#  [1,] -5.551115e-17
t=1:10
t(-sin(t))%*%(-exp(-0.3*t))
#[1,] 0.8625048

#Data reconstruction by two modes
B<-uda2[,1:2]%*%diag(dda2)[1:2,1:2]%*%t(vda2[,1:2])

#Data reconstruction by all the ten modes
B1<-uda2%*%diag(dda2)%*%t(vda2)

max(B-B1)
#[1] 1.164677e-13 implies B = B1

#Plot Fig. 10.4: The two-mode reconstruction by SVD
plot.new()
filled.contour(x,y,matrix(B[,5],nrow=100), color.palette =rainbow, 
               plot.title=title(main="2-mode SVD reconstructed field t=5",
                                xlab="x", ylab="y", cex.lab=1.0),
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(0,3*pi, by = 1), cex=1.0) 
                 axis(2,seq(0, 2*pi, by = 1), cex=1.0)})

# Read netCDF data files
#install.packages("ncdf")
library(ncdf4)
#Download netCDF dataset air.mon.mean.nc
#https://climatemathematics.sdsu.edu/data/air.mon.mean.nc
#Move air.mon.mean.nc to directory /Users/sshen/data
# 4 dimensions: lon,lat,level,time
setwd("/Users/sshen/climmath")
nc=nc_open("data/air.mon.mean.nc")
nc
nc$dim$lon$vals #output lon values 0.0->357.5
nc$dim$lat$vals #output lat values 90->-90
nc$dim$time$vals #output time values in GMT hours: 1297320, 1298064, ...
nc$dim$time$units
#[1] "hours since 1800-01-01 00:00:0.0"
#nc$dim$level$vals
Lon <- ncvar_get(nc, "lon")
Lat1 <- ncvar_get(nc, "lat")
Time<- ncvar_get(nc, "time")
#Time is the same as nc$dim$time$vals
head(Time)
#[1] 1297320 1298064 1298760 1299504 1300224 1300968
library(chron)
Tymd<-month.day.year(Time[1]/24,c(month = 1, day = 1, year = 1800))
#c(month = 1, day = 1, year = 1800) is the reference time
Tymd
#$month
#[1] 1
#$day
#[1] 1
#$year
#[1] 1948
#1948-01-01
precnc<- ncvar_get(nc, "air")
dim(precnc)
#[1] 144  73 826, i.e., 826 months=1948-01 to 2016-10, 68 years 10 mons

#Plot Fig. 10.5
#plot a 90S-90N temp along a meridional line at 180E
plot(seq(-90,90,length=73),precnc[72,,1], type="o", lwd=2,
     xlab="Latitude", ylab="Temperature [oC]",
     main="NCEP/NCAR Reanalysis Surface Air Temperature [deg C] 
     along a Meridional Line at 180E: Jan 1948")

#Write data as a CSV space-time matrix with a header
precst=matrix(0,nrow=10512,ncol=826)
temp=as.vector(precnc[,,1])
head(temp)
for (i in 1:826) {precst[,i]=as.vector(precnc[ , , i])}
dim(precst)
#[1] 10512   826
#Build lat and lon for 10512 spatial positions usig rep
LAT=rep(Lat1, 144)
LON=rep(Lon[1],each=73)
gpcpst=cbind(LAT, LON, precst)
dim(gpcpst)
#[1] 10512   828
#The first two columns are lat and lon. 826 mons: 1948.01-2016.10
#Convert the Julian day and hour into calendar mons for time 
tm=month.day.year(Time/24, c(month = 1, day = 1, year = 1800))
tm1=paste(tm$year,"-",tm$month)
tm2=c("Lat","Lon",tm1) #This is the deader
#Assign the header to the space-time data matrix
colnames(gpcpst) <- tm2
setwd("/Users/sshen/climmath/data")
#setwd routes the desired csv file to a given directory
write.csv(gpcpst,file="ncepJan1948_Oct2016.csv")

#Compute the January climatology and standard deviation: 1948-2015
monJ=seq(1,816,12)
gpcpdat=gpcpst[,3:818]
gpcpJ=gpcpdat[,monJ]
climJ<-rowMeans(gpcpJ) #Use all the 68 Januarys from 1948 to 2015
library(matrixStats)# rowSds command is in the matrixStats package
sdJ<-rowSds(gpcpJ)

#Plot Fig. 9.9
#Verify previous calculutions and plots in Fig. 9.9
#Plot Jan climatology
Lat=-Lat1
mapmat=matrix(climJ,nrow=144)
mapmat= mapmat[,seq(length(mapmat[1,]),1)]
plot.new()
int=seq(-50,50,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen','green', 
                               'white','yellow','pink','red','maroon'),interpolate='spline')
library(maps)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="NCEP Jan SAT RA 1948-2015 climatology [deg C]"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"))

#Plot Jan Standard Deviation
Lat=-Lat1
mapmat=matrix(sdJ,nrow=144)
mapmat=pmax(pmin(mapmat,6),0)
mapmat= mapmat[,seq(length(mapmat[1,]),1)]
plot.new()
int=seq(0,6,length.out=91)
rgb.palette=colorRampPalette(c('black','blue', 'green', 
                               'yellow', 'pink','red','maroon'),interpolate='spline')
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="NCEP Jan SAT RA 1948-2015 Standard Deviation [deg C]"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"))

#Plot Fig. 10.6 (Eigenvalues and variances explained)
#Compute the Jan EOFs
monJ=seq(1,816,12)
gpcpdat=gpcpst[,3:818]
gpcpJ=gpcpdat[,monJ]
climJ<-rowMeans(gpcpJ)
library(matrixStats)
sdJ<-rowSds(gpcpJ)
anomJ=(gpcpdat[,monJ]-climJ)/sdJ #standardized anomalies
anomAW=sqrt(cos(gpcpst[,1]*pi/180))*anomJ #Area weighted anormalies
svdJ=svd(anomAW)  #execute SVD

#plot eigenvalues
par(mar=c(3,4,2,4))
plot(100*(svdJ$d)^2/sum((svdJ$d)^2), type="o", ylab="Percentage of variance [%]",
     xlab="Mode number", main="Eigenvalues of covariance matrix")
legend(20,5, col=c("black"),lty=1,lwd=2.0,
       legend=c("Percentange variance"),bty="n",
       text.font=2,cex=1.0, text.col="black")
par(new=TRUE)
plot(cumsum(100*(svdJ$d)^2/sum((svdJ$d)^2)),type="o",
     col="blue",lwd=1.5,axes=FALSE,xlab="",ylab="")
legend(20,50, col=c("blue"),lty=1,lwd=2.0,
       legend=c("Cumulative variance"),bty="n",
       text.font=2,cex=1.0, text.col="blue")
axis(4)
mtext("Cumulative variance [%]",side=4,line=2)

#Plot Fig. 10.7 (EOFs and PCs)
#plot EOF1: The physical EOF= eigenvector divided by area factor
mapmat=matrix(svdJ$u[,1]/sqrt(cos(gpcpst[,1]*pi/180)),nrow=144)
rgb.palette=colorRampPalette(c('blue','green','white', 
                               'yellow','red'),interpolate='spline')
int=seq(-0.04,0.04,length.out=61)
mapmat=mapmat[, seq(length(mapmat[1,]),1)]
filled.contour(Lon, Lat, -mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="January EOF1 from 1948-2015 NCEP Temp Data"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="Scale"))
#
#plot PC1
pcdat<-svdJ$v[,1]
Time<-seq(1948,2015)
plot(Time, -pcdat, type="o", main="PC1 of NCEP RA Jan SAT: 1948-2015",
     xlab="Year",ylab="PC values",
     lwd=2, ylim=c(-0.3,0.3))

#Plot Fig. 10.8 
#plot EOF2: The physical EOF= eigenvector divided by area factor
mapmat=matrix(svdJ$u[,2]/sqrt(cos(gpcpst[,1]*pi/180)),nrow=144)
rgb.palette=colorRampPalette(c('blue','green','white', 
                               'yellow','red'),interpolate='spline')
int=seq(-0.04,0.04,length.out=61)
mapmat=mapmat[, seq(length(mapmat[1,]),1)]
filled.contour(Lon, Lat, -mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="January EOF2 from 1948-2015 NCEP Temp Data"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="Scale"))
#
#plot PC2
pcdat<-svdJ$v[,2]
Time<-seq(1948,2015)
plot(Time, -pcdat, type="o", main="PC2 of NCEP RA Jan SAT: 1948-2015",
     xlab="Year",ylab="PC values",
     lwd=2, ylim=c(-0.3,0.3))


#Plot Fig. 10.9 
#plot EOF3: The physical EOF= eigenvector divided by area factor
mapmat=matrix(svdJ$u[,3]/sqrt(cos(gpcpst[,1]*pi/180)),nrow=144)
rgb.palette=colorRampPalette(c('blue','green','white', 
                               'yellow','red'),interpolate='spline')
int=seq(-0.04,0.04,length.out=61)
mapmat=mapmat[, seq(length(mapmat[1,]),1)]
filled.contour(Lon, Lat, -mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="January EOF3 from 1948-2015 NCEP Temp Data"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="Scale"))
#
#plot PC3
pcdat<-svdJ$v[,3]
Time<-seq(1948,2015)
plot(Time, -pcdat, type="o", main="PC3 of NCEP RA Jan SAT: 1948-2015",
     xlab="Year",ylab="PC values",
     lwd=2, ylim=c(-0.3,0.3))

#Plot Fig. 10.10 (EOFs and PCs from de-trended standardized data)
#EOF from de-trended data
monJ=seq(1,816,12)
gpcpdat=gpcpst[,3:818]
gpcpJ=gpcpdat[,monJ]
climJ<-rowMeans(gpcpJ)
library(matrixStats)
sdJ<-rowSds(gpcpJ)
anomJ=(gpcpdat[,monJ]-climJ)/sdJ
trendM<-matrix(0,nrow=10512, ncol=68)#trend field matrix
trendV<-rep(0,len=10512)#trend for each grid box: a vector
for (i in 1:10512) {
  trendM[i,] = (lm(anomJ[i,] ~ Time))$fitted.values
  trendV[i]<-lm(anomJ[i,] ~ Time)$coefficients[2]
}
dtanomJ = anomJ - trendM
dim(dtanomJ)
dtanomAW=sqrt(cos(gpcpst[,1]*pi/180))*dtanomJ
svdJ=svd(dtanomAW)

#Plot EOF1
mapmat=matrix(svdJ$u[,1]/sqrt(cos(gpcpst[,1]*pi/180)),nrow=144)
rgb.palette=colorRampPalette(c('blue','green','white', 
                               'yellow','red'),interpolate='spline')
int=seq(-0.04,0.04,length.out=61)
mapmat=mapmat[, seq(length(mapmat[1,]),1)]
filled.contour(Lon, Lat, -mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="January EOF1 from 1948-2015 NCEP 
De-trended Standardized Temp Anom Data"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="Scale"))
#
#plot PC1
pcdat<-svdJ$v[,1]
Time<-seq(1948,2015)
plot(Time, -pcdat, type="o", 
     main="PC1 of NCEP RA Jan SAT: 1948-2016
     De-trended Standardized Data",
     xlab="Year",ylab="PC values",
     lwd=2, ylim=c(-0.3,0.3))

#Plot Fig. 10.11 (EOF2 and PC2 of de-trended standardized data)
#Plot EOF2
mapmat=matrix(svdJ$u[,2]/sqrt(cos(gpcpst[,1]*pi/180)),nrow=144)
rgb.palette=colorRampPalette(c('blue','green','white', 
                               'yellow','red'),interpolate='spline')
int=seq(-0.04,0.04,length.out=61)
mapmat=mapmat[, seq(length(mapmat[1,]),1)]
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="January EOF2 from 1948-2015 NCEP 
De-trended Standardized Temp Anom Data"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="Scale"))
#
#plot PC2
pcdat<-svdJ$v[,2]
Time<-seq(1948,2015)
plot(Time, pcdat, type="o", 
     main="PC2 of NCEP RA Jan SAT: 1948-2016
     De-trended Standardized Data",
     xlab="Year",ylab="PC values",
     lwd=2, ylim=c(-0.3,0.3))

#Plot Fig. 10.12 (Global average January SAT and its trend)
#Plot the area-weighted global average Jan temp from 1948-2015
#Begin from the space-time data matrix gpcpst[,1]
vArea=cos(gpcpst[,1]*pi/180)
anomA=vArea*anomJ
dim(anomA)
JanSAT<-colSums(anomA)/sum(vArea)
plot(Time, JanSAT, type="o", lwd=2,
     main="Global Average Jan SAT Anomalies from NCEP RA",
     xlab="Year",ylab="Temperature [oC]")
regSAT<-lm(JanSAT ~ Time)
#0.48oC/100a trend
abline(regSAT, col="red", lwd=4)
text(1965,0.35,"Linear trend 0.48oC/100a", col="red", cex=1.3)

#Plot Fig. 10.13 (Temperature trend map)
#plot the trend of Jan SAT non-standardized anomaly data 
#Begin with the space-time data matrix
monJ=seq(1,816,12)
gpcpdat=gpcpst[,3:818]
gpcpJ=gpcpdat[,monJ]
plot(gpcpJ[,23])
climJ<-rowMeans(gpcpJ)
anomJ=(gpcpdat[,monJ]-climJ)
trendV<-rep(0,len=10512)#trend for each grid box: a vector
for (i in 1:10512) {
  trendV[i]<-lm(anomJ[i,] ~ Time)$coefficients[2]
}
mapmat1=matrix(10*trendV,nrow=144)
mapv1=pmin(mapmat1,1) #Compress the values >5 to 5
mapmat=pmax(mapv1,-1) #compress the values <-5 t -5
rgb.palette=colorRampPalette(c('blue','green','white', 'yellow','red'),
                             interpolate='spline')
int=seq(-1,1,length.out=61)
mapmat=mapmat[, seq(length(mapmat[1,]),1)]
par(mar=c(4,4,2,4), mgp=c(2,1,0), cex.lab=1.4, cex.axis=1.2)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(
                 main="Trend of the NCEP RA1 Jan 1948-2015 Anom Temp",
                 xlab="Longitude",ylab="Latitude"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="oC/10a"))

#Plot Fig. 10.14 (GPCP precipitation from 90S to 90N)
#Download the GPCP data from the ESRL website
#https://www.esrl.noaa.gov/psd/data/gridded/data.gpcp.html
#Select the monthly precipitation data by clicking 
#the Download File: precip.mon.mean.nc
#Move the data file to your assigned working directory 
#/Users/sshen/climmath/data
setwd("/Users/sshen/climmath")
#install.packages("ncdf")
library(ncdf4)
# 4 dimensions: lon,lat,level,time
nc=ncdf4::nc_open("data/precip.mon.mean.nc")
nc
nc$dim$lon$vals
nc$dim$lat$vals
nc$dim$time$vals
#nc$dim$time$units
#nc$dim$level$vals
Lon <- ncvar_get(nc, "lon")
Lat <- ncvar_get(nc, "lat")
Time<- ncvar_get(nc, "time")
head(Time)
#[1] 65378 65409 65437 65468 65498 65529
library(chron)
month.day.year(65378,c(month = 1, day = 1, year = 1800))
#1979-01-01
precnc<- ncvar_get(nc, "precip")
dim(precnc)
#[1] 144  72 451 #2.5-by-2.5, 451 months from Jan 1979-July 2016
#plot a 90S-90N precip along a meridional line at 160E
par(mar=c(4.5,4.5,2,0.5))
plot(seq(-90,90,length=72), precnc[64,,1], type="l", lwd=2,
     xlab="Latitude", ylab="Precipitation [mm/day]",
     main="90S-90N precipitation along the meridian at 160E: Jan 1979",
     cex.lab=1.5, cex.axis=1.5)

#Plot Fig. 10.15 (GPCP climatology and standard deviation maps)
#Write the data as space-time matrix with a header
precst=matrix(0,nrow=10368,ncol=451)
temp=as.vector(precnc[,,1])
head(temp)
for (i in 1:451) {precst[,i]=as.vector(precnc[ , , i])}
#precst is the space-time GPCP data 
LAT=rep(Lat, 144)
LON=rep(Lon[1],72)
for (i in 2:144){LON=c(LON, rep(Lon[i],72))}
gpcpst=cbind(LAT, LON, precst)

#Convert the Julian day into calender dates for time 
tm=month.day.year(Time, c(month = 1, day = 1, year = 1800))
tm1=paste(tm$year,"-",tm$month)
#tm1=data.frame(tm1)
tm2=c("Lat","Lon",tm1)
colnames(gpcpst) <- tm2
#Look at a sample section of the space-time data 
gpcpst[890:892,1:5]
#        Lat   Lon   1979 - 1  1979 - 2  1979 - 3
#[1,] -26.25 31.25 0.05752099 0.1805309 0.2104454
#[2,] -23.75 31.25 0.10855579 0.2562788 0.2883888
#[3,] -21.25 31.25 0.08718992 0.2738046 0.2569866
write.csv(gpcpst,file="gpcp9716jul.csv")
#
#Compute and plot the GPCP climatology from Jan 1979-July 2016
library(maps)
climmat=precnc[,,1]
for(i in 2:451){climmat =climmat + precnc[,,i]}
climmat=climmat/451
mapmat=climmat
mapmat=pmax(pmin(mapmat,10),0)
int=seq(min(mapmat),max(mapmat),length.out=11)
rgb.palette=colorRampPalette(c('bisque2','cyan', 'green', 'yellow',
                               'pink','indianred2', 'red','maroon','black'),
                             interpolate='spline')
plot.new()
par(mar=c(4, 4.5, 2, 1))
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="GPCP 1979-2016 Precipitation Climatology [mm/day]",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1,seq(0,360, by=30), cex.axis=1.5); 
                 axis(2, seq(-90,90,by=30), cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="mm/day"),
               key.axes={axis(4, seq(0,10, len=11), cex.axis=1.5)})

#Compute and plot standard deviation from Jan 1979-July 2016
sdmat=(precnc[,,1]-climmat)^2
for(i in 2:451){sdmat =sdmat + (precnc[,,i]-climmat)^2}
sdmat=sqrt(sdmat/451)
mapmat=sdmat
mapmat=pmax(pmin(mapmat,5),0)
int=seq(min(mapmat),max(mapmat),length.out=21)
rgb.palette=colorRampPalette(c('bisque2','cyan', 'green', 'yellow','pink',
                               'indianred2', 'red','maroon','black'),
                             interpolate='spline')
plot.new()
par(mar=c(4.3, 4.5, 2, 1))
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="GPCP 1979-2016 Standard Deviation of Precipitation [mm/day]",
                                xlab="Longitude",ylab="Latitude", cex.lab=1.5),
               plot.axes={axis(1,seq(0,360, by=30), cex.axis=1.5); 
                 axis(2,seq(-90,90,by=30), cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="mm/day"),
               key.axes={axis(4, seq(0,5,len=11), cex.axis=1.5)})
#
#
#Compute the January precipitation climatology and plot its map
Jmon=seq(3,453,by=12)
Jclim =rowMeans(gpcpst[,Jmon])
mapmat=matrix(Jclim,nrow=144)
mapmat=pmax(pmin(mapmat,10),0)
int=seq(min(mapmat),max(mapmat),length.out=11)
rgb.palette=colorRampPalette(c('bisque2','cyan', 'green', 'yellow','pink','indianred2', 'red','maroon','black'),
                             interpolate='spline')
plot.new()
par(mar=c(4, 4.5, 2, 1))
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="GPCP 1979-2016 Precipitation Jan Climatology [mm/day]",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1,seq(0,360, by=30), cex.axis=1.5); 
                 axis(2, seq(-90,90,by=30), cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="mm/day"),
               key.axes={axis(4, seq(0,10, len=11), cex.axis=1.5)})

#Plot Fig. 9.9
#Plot Jan temperature climatology
Lat=-Lat1
mapmat=matrix(climJ,nrow=144)
mapmat= mapmat[,seq(length(mapmat[1,]),1)]
plot.new()
int=seq(-50,50,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen','green', 
                               'white','yellow','pink','red','maroon'),interpolate='spline')
library(maps)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="NCEP Jan SAT RA 1948-2015 climatology [deg C]"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"))

#Plot Jan Standard Deviation
Lat=-Lat1
mapmat=matrix(sdJ,nrow=144)
mapmat=pmax(pmin(mapmat,6),0)
mapmat= mapmat[,seq(length(mapmat[1,]),1)]
plot.new()
int=seq(0,6,length.out=91)
rgb.palette=colorRampPalette(c('black','blue', 'green', 
                               'yellow', 'pink','red','maroon'),interpolate='spline')
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="NCEP Jan SAT RA 1948-2015 Standard Deviation [deg C]"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"))

############################################################################
#
# Chapter 11: Climate Data Matrices and Linear Algebra
#
############################################################################

#NOAAGlobalTemp dataset since 1880: A merged land SAT and ocean SST anomalies
#The monthly 5-deg gridded data and their global average can be 
#downloaded from
#ftp://ftp.ncdc.noaa.gov/pub/data/noaaglobaltemp/operational
#The gridded data has 43MB
#The data can also be downloaded from the website of this book
#https://climatemathematics.sdsu.edu/data
#/NOAAGlobalTemp.gridded.v4.0.1.201701.asc

#rm(list=ls(all=TRUE))
# Download .asc file
#.asc is an ASCII data format
#This book uses "scan" to read the asc data
#and then write the data into a space-time matrix
#Climate Mathematics treats space-time matrix as 
#its data standard while big cliamte data use .nc

setwd("/Users/sshen/climmath")
da1=scan("data/NOAAGlobalTemp.gridded.v4.0.1.201701.asc")
#From Jan 1880 to Jan 2017
length(da1)
#[1] 4267130
da1[1:3]
#[1]    1.0 1880.0 -999.9 #means mon, year, temp
#data in 72 rows (2.5, ..., 357.5) and 
#data in 36 columns (-87.5, ..., 87.5)
tm1=seq(1,4267129, by=2594)
tm2=seq(2,4267130, by=2594)
length(tm1)
length(tm2)
mm1=da1[tm1] #Extract months
yy1=da1[tm2] #Extract years
head(mm1)
head(yy1)
length(mm1)
length(yy1)
rw1<-paste(yy1, sep="-", mm1) #Combine YYYY with MM
head(tm1)
head(tm2)
tm3=cbind(tm1,tm2)
tm4=as.vector(t(tm3))
head(tm4)
#[1]    1    2 2595 2596 5189 5190
da2<-da1[-tm4] #Remote the months and years data from the scanned data
length(da2)/(36*72)
#[1] 1645 #months, 137 yrs 1 mon: Jan 1880-Jan 2017
da3<-matrix(da2,ncol=1645) #Generate the space-time data
#2592 (=36*72) rows and 1645 months (=137 yrs 1 mon)
dim(da3)
#[1] 2592 1645

#Put space-time coordinates in the space-time data da3
colnames(da3)<-rw1
lat1=seq(-87.5, 87.5, length=36)
lon1=seq(2.5, 357.5,  length=72)
LAT=rep(lat1, each=72)
LON=rep(lon1,36)
gpcpst=cbind(LAT, LON, da3)
head(gpcpst)
dim(gpcpst)
#[1] 2592 1647 #The first two columns are Lat and Lon
#-87.5 to 87.5 and then 2.5 to 375.5
#The first row for time is header, not counted as data.
gpcpst[1:3,1:6] #Part of the data
#       LAT  LON 1880-1 1880-2 1880-3 1880-4
#[1,] -87.5  2.5 -999.9 -999.9 -999.9 -999.9
#[2,] -87.5  7.5 -999.9 -999.9 -999.9 -999.9
#[3,] -87.5 12.5 -999.9 -999.9 -999.9 -999.9

write.csv(gpcpst,file="NOAAGlobalT.csv")
#Output the data as a csv file

#Plot Fig. 11.4: Dec 2015 global surface temperature anomalies map
library(maps)#Need to install maps package first if not done before
Lat= seq(-87.5, 87.5, length=36)
Lon=seq(2.5, 357.5, length=72)
mapmat=matrix(gpcpst[,1634],nrow=72)
#column 1634 corresponding to Dec 2015
#Convert the vector into a lon-lat matrix for R map plotting
mapmat=pmax(pmin(mapmat,6),-6) #Put values between -6 and 6
#Matrix flipping is not needed since the data go from 2.5 to 375.5
#plot.new()
par(mar=c(4,5,3,0))
int=seq(-6,6,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen','green', 
                               'yellow','pink','red','maroon'),interpolate='spline')
mapmat= mapmat[,seq(length(mapmat[1,]),1)]
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="NOAAGlobalTemp Anomalies Dec 2015 [deg C]",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); 
                 axis(2, cex.axis=1.5);map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})


#Plot Fig. 11.5: Dec 1997 tropical Pacific SST anomalies map
#Select only the data for the tropical Pacific region
n2<-which(gpcpst[,1]>-20&gpcpst[,1]<20&gpcpst[,2]>160&gpcpst[,2]<260)
dim(gpcpst)
length(n2)
#[1] 160 (=8 latitude bends and 20 longitude bends)
pacificdat=gpcpst[n2,855:1454]

plot.new()
Lat=seq(-17.5,17.5, by=5)
Lon=seq(162.5, 257.5, by=5)
par(mar=c(4,5,3,0))
mapmat=matrix(pacificdat[,564], nrow=20)
int=seq(-5,5,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen',
                               'green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
#mapmat= mapmat[,seq(length(mapmat[1,]),1)]
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="Tropic Pacific SAT Anomalies [deg C]: Dec 1997",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

#Extract data for a specified box with given lat and lon
n2 <- which(gpcpst[,1]==32.5&gpcpst[,2]==242.5)
SanDiegoData <- gpcpst[n2,855:1454]
plot(seq(1880,2017, len=length(SanDiegoData)), 
     SanDiegoData, type="l", 
     xlab="Year", ylab="Temp [oC]",
     main="San Diego temperature anomalies history")
lm(SanDiegoData ~ seq(1880,2017, len=length(SanDiegoData)))

n2 <- which(gpcpst[,1]==52.5&gpcpst[,2]==247.5)
EdmontonData <- gpcpst[n2,855:1454]
plot(seq(1880,2017, len=length(EdmontonData)), 
     EdmontonData, type="l", 
     xlab="Year", ylab="Temp [oC]",
     main="Temperature anomalies history of Edmonton, Canada")
lm(EdmontonData ~ seq(1880,2017, len=length(EdmontonData)))

#Compute the area-weighted average of the gridded data
#36-by-72 boxes and Jan1880-Jan2016=1633 months + lat and lon
#Compute the area-weight for each box and each month 
#that has data. Thus the area-weight is a matrix.
areaw=matrix(0,nrow=2592,ncol=1647)
dim(areaw)
#[1] 2592 1647
temp=gpcpst
areaw[,1]=temp[,1]
areaw[,2]=temp[,2]
veca=cos(areaw[,1]*pi/180) #convert deg into radian
#create an area-weight matrix equal to cosine for the box with data
#and zero for the box with missing data
for(j in 3:1647) {for (i in 1:2592) {if(temp[i,j]> -290.0) {areaw[i,j]=veca[i]} }} 

#area-weight data matrix’s first two columns as lat-lon
tempw=areaw*temp
tempw[,1:2]=temp[,1:2]
#create monthly global average vector for 1645 months
#Jan 1880- Jan 2017
avev=colSums(tempw[,3:1647])/colSums(areaw[,3:1647])

#Plot Fig. 11.6: Global average temperature and its trend
plot.new()
timemo=seq(1880,2017,length=1645)
par(mar=c(3.5,3.5,3,0.5))
par(mgp=c(2.3,1.0,0.0))
plot(timemo,avev,type="l", cex.lab=1.4,
     xlab="Year", ylab="Temperature anomaly [deg C]",
     main="Area-weighted global average of 
     monthly SAT anomalies: Jan 1880-Jan 2017")
abline(lm(avev ~ timemo),col="blue",lwd=2)
text(1930,0.7, "Linear trend: 0.69 [oC] per century",
     cex=1.4, col="blue")


#Plot Fig. 11.7: Compare the warming trends of Edmonton and San Diego
#Extract data for a specified box with given lat and lon
n2 <- which(gpcpst[,1]==52.5&gpcpst[,2]==247.5)
dedm <- gpcpst[n2,855:1454]
t=seq(1880,2017, len=length(dedm))
par(mar=c(4,4.5,2.5,1))
plot(t, dedm, type="l", ylim=c(-15,15),col="red",
     cex.lab=1.3, cex.axis=1.3,
     xlab="Year", ylab="Temperature anomalies [deg C]",
     main="Monthly temperature anomalies history of 
     Edmonton, Canada, and San Diego, USA")
regedm <- lm(dedm ~ t)
abline(regedm,col="red", lwd=3)

#San Diego data
n3 <- which(gpcpst[,1]==32.5&gpcpst[,2]==242.5)
dsan <- gpcpst[n3,855:1454]
lines(t, dsan, type="l", col="blue")
regsan =lm(dsan ~ t)
abline(regsan, col="blue", lwd=3)

legend(1880,16, col=c("red","blue"),lty=1,lwd=2.0,
       legend=c("Edmonton, Canada: Trend 1.18 deg C/century, SD 3.01 deg C", 
                "San Diego, USA: Trend 0.76 deg C/century, SD 0.87 deg C"),
       bty="n",text.font=2,cex=1.0)

regedm
#0.01178  #Edmonton trend
regsan
#0.007624  #San Diego trend
sd(dedm)
#[1] 3.006863
sd(dsan)
#[1] 0.8653399


#Plot Fig. 11.8: Display the trends of 12 months using 12 panels on the same figure
#Read directly from the NCEI URL for the NOAA GlobalTemp monthly global aevrage data:  
#https://www1.ncdc.noaa.gov/pub/data/noaaglobaltemp/operational/timeseries
#Data file: aravg.mon.land_ocean.90S.90N.v4.0.1.201703.txt
#setwd("/Users/sshen/Desktop/MyDocs/teach/SIOC290-ClimateMath2016/Rcodes/NOAAGlobalTemp")
setwd("/Users/sshen/climmath")
aveNCEI <- read.table("data/aravg.mon.land_ocean.90S.90N.v4.0.1.201703.txt", 
                      header=FALSE)
dim(aveNCEI) #Jan 1880-Feb 2017 #an extra month to be deleted
#[1] 1647   10
par(mar=c(4,5,2,1))
timemo=seq(aveNCEI[1,1],aveNCEI[length(aveNCEI[,1]),1],len=length(aveNCEI[,1]) )
#create matrix of 136 yrs of data matrix
# row=year from 1880 to 2016, col=mon 1 to 12
ave=aveNCEI[,3]
myear=length(ave)/12
nyear=floor(myear)
nmon=nyear*12
avem = matrix(ave[1:nmon], ncol=12, byrow=TRUE)

#compute annual average
annv=seq(0,length=nyear)
for(y in 1:nyear){annv[y]=mean(avem[y,])}

#Put the monthly averages and annual ave in a matrix
avemy=cbind(avem,annv)

#Plot 12 panels on the same figure: Trend for each month
dev.off()
quartz(width=10,height=16,pointsize = 16)
#quartz(display = "name", width = 5, height = 5, pointsize = 12,
#       family = "Helvetica", antialias = TRUE, autorefresh = TRUE)
plot.new()
#png(file = 'monthtrend.png') #Automatical saving of a figure
timeyr=seq(aveNCEI[1,1], aveNCEI[1,1]+nyear-1)
par(mfrow = c(4, 3))  # 4 rows and 3 columns
par(mgp=c(2,1,0))
for (i in 1:12) { 
  plot(timeyr, avemy[,i],type="l", ylim=c(-1.0,1.0),
       xlab="Year",ylab="Temp [oC]",
       main = paste("Month =", i, split = ""))
  abline(lm(avemy[,i]~timeyr),col="red")
  text(1945,0.7, paste("Trend oC/century=", 
                       round(digits=3, 
                             (100*coefficients(lm(avemy[,i]~timeyr))[2]))), 
       col="red")
}
dev.off()

#Plot Fig. 11.9: Global average annual mean
par(mar=c(4,4.5,2,1.0))
avem = matrix(avev[1:1644], ncol=12, byrow=TRUE)
#compute annual average
annv=rowMeans(avem)
#Plot the annual mean global average temp
timeyr<-seq(1880, 2016)
plot(timeyr,annv,type="s", 
     cex.lab=1.4, lwd=2,
     xlab="Year", ylab="Temperature anomaly [oC]",
     main="Area-weighted global average of annual SAT anomalies: 1880-2016")
abline(lm(annv ~ timeyr),col="blue",lwd=2)
text(1940,0.4, "Linear trend: 0.69 [oC] per century",
     cex=1.4, col="blue")
text(1900,0.07, "Base line",cex=1.4, col="red")
lines(timeyr,rep(0,137), type="l",col="red")

#Plot Fig. 11.10: Fit polynomials to the global average annual mean
#Polynomial fitting to the global average annual mean
#poly9<-lm(annv ~ poly(timeyr,9, raw= TRUE))
#raw=TRUE means regular polynomial a0+a1x^2+..., non-orthogonal 
polyor9<-lm(annv ~ poly(timeyr,9, raw= FALSE))
polyor20<-lm(annv ~ poly(timeyr,20, raw= FALSE))
#raw=FALSE means orthongonal polynomial of 9th order
#Orthogonal polynomial fitting is usually better
plot(timeyr,annv,type="s", 
     cex.lab=1.4, lwd=2,
     xlab="Year", ylab="Temperature anomaly [oC]",
     main="Annual SAT time series and its orthogonal polynomial fits: 1880-2016")
lines(timeyr,predict(polyor9),col="blue", lwd=3)
legend(1880, 0.6,  col=c("blue"),lty=1,lwd=2.0, 
       legend=c("9th order orthogonal polynomial fit"),
       bty="n",text.font=2,cex=1.5)
lines(timeyr,predict(polyor20),col="red", lwd=3)
legend(1880, 0.7,  col=c("red"),lty=1,lwd=2.0, 
       legend=c("20th order orthogonal polynomial fit"),
       bty="n",text.font=2,cex=1.5)

#Deal with missing data NA
x=1:8
y=c(2,4,NA,3,6.8,NA,NA,9) 
fitted(lm(y ~ x, na.action=na.exclude))
#  1    2    3    4    5    6    7    8 
#2.08 3.04   NA 4.96 5.92   NA   NA 8.80 
##
fitted(lm(y ~ x, na.action=na.omit))
#  1    2    4    5    8 
#2.08 3.04 4.96 5.92 8.80 


#Plot Fig. 11.11: Spatial pattern of the linear temporal trend
#No missing data for the first month (January 1900) and 
#the last month (December 1999)

#Compute the trend for each box for the 20th century
timemo1=seq(1900,2000, len=1200)
temp1=temp
temp1[temp1 < -490.00] <- NA
trendgl=rep(0,2592)
for (i in 1:2592){
  if(is.na(temp1[i,243])==FALSE & is.na(temp1[i,1442])==FALSE) 
  {trendgl[i]=lm(temp1[i,243: 1442] ~ timemo1, na.action=na.omit)$coefficients[2]} 
  else 
  {trendgl[i]=NA}
}
library(maps)
Lat= seq(-87.5, 87.5, length=36)
Lon=seq(2.5, 357.5, length=72)
mapmat=matrix(100*trendgl,nrow=72)
mapmat=pmax(pmin(mapmat,2),-2)
#Matrix flipping is not needed since the data goes from 2.5 to 375.5
plot.new()
par(mar=c(4.5,4.5,3,0))
int=seq(-2,2,length.out=21)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen','green', 
                               'yellow','pink','red','maroon'),interpolate='spline')
#mapmat= mapmat[,seq(length(mapmat[1,]),1)]
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="Jan 1900-Dec 1999 temperature trends: [oC/century]",
                                xlab="Longitude",ylab="Latitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                          map('world2', add=TRUE);grid()},
               key.title=title(main=""),
               key.axes={axis(4, cex.axis=1.5)})

#Example of computing the trend for a grid bbox
i=600
timemo1 = seq(1900, 1999, length=1200)
lm(temp1[i,243:1442] ~ timemo1, na.action=na.omit)
lm(temp1[i,243:1442] ~ timemo1, na.action=na.exclude)
temp1[i,243:1442]
# 1900-1  1900-2  1900-3  1900-4  1900-5
#-0.7457      NA -1.4406 -1.0936 -0.8193


#Plot Fig. 11.12:Spatial pattern of the linear temporal trend
#under a relaxed condition: allowing up to 1/3 data missing

#Trend for each box for the 20th century: Version 2: Allow 2/3 of data
#Compute the trend
timemo1=seq(1900,2000, len=1200)
temp1=temp[,243:1442]
temp1[temp1 < -490.00] <- NA
temptf=is.na(temp1)
bt=et=rep(0,2592)
for (i in 1:2592) {
  if (length(which(temptf[i,]==FALSE)) !=0)
  {
    bt[i]=min(which(temptf[i,]==FALSE))
    et[i]=max(which(temptf[i,]==FALSE))
  }
}
##
trend20c=rep(0,2592)
for (i in 1:2592){
  if(et[i]-bt[i] > 800) 
  {trend20c[i]=lm(temp1[i,bt[i]:et[i]] ~ seq(bt[i],et[i]), 
                  na.action=na.omit)$coefficients[2]} 
  else 
  {trend20c[i]=NA}
}
#plot the 20C V2 trend map 
plot.new()
#par(mar=c(4,5,3,0))
mapmat=matrix(120*trend20c,nrow=72)
mapmat=pmax(pmin(mapmat,0.2),-0.2)
int=seq(-0.2,0.2,length.out=41)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen','green', 
                               'yellow','pink','red','maroon'),interpolate='spline')
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="Jan 1900-Dec 1999 temperature trends: [oC/decade]",
                                xlab="Longitude",ylab="Latitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main=""),
               key.axes={axis(4, cex.axis=1.5)})


#Plot Fig. 11.13: Map of the 1976-2016 trend of monhtly data
timemo2=seq(1976,2017, len=492)
temp1=temp
temp1[temp1 < -490.00] <- NA
trend7616=rep(0,2592)
for (i in 1:2592){
  if(is.na(temp1[i,1155])==FALSE & is.na(temp1[i,1646])==FALSE) 
  {trend7616[i]=lm(temp1[i,1155: 1646] ~ timemo2, na.action=na.omit)$coefficients[2]} 
  else 
  {trend7616[i]=NA}
}
#plot the 1976-2016 trend map 
plot.new()
par(mar=c(4.5,4.5,3,0))
mapmat=matrix(120*trend7616,nrow=72)
mapmat=pmax(pmin(mapmat,4),-4)
int=seq(-4,4,length.out=41)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen','green', 
                               'yellow','pink','red','maroon'),interpolate='spline')
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="Jan 1976-Dec 2016 temperature trends: [oC/decade]",
                                xlab="Longitude",ylab="Latitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);map('world2', add=TRUE);grid()},
               key.title=title(main=""),
               key.axes={axis(4, cex.axis=1.5)})

#Appendix B: Cross Product 
#GPS-Planimeter example: Compute the area of an ellipse based on
#the coordinate data on the bounndary using Green's Theorem
n=1000
a=4
b=2
t=seq(0,2*pi,length=n+1)
x=a*cos(t)
y=b*sin(t)
s=rep(0,n)
for (i in 1:n){s[i]=-y[i]*x[i+1] + x[i]*y[i+1]}
A=0.5*sum(s)
A
#[1] 25.13258
#The aera of an ellipse according to the formula: pi ab
pi*a*b
#[1] 25.13274