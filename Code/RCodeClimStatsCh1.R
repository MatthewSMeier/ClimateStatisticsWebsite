################## R Codes for #################################
# Statistics and Data Visualization in Climate Science 
#                with R and Python
# A Cambridge University Press book 
# By SSP Shen and GR North
# 
# The R code was written by 
# Samuel Shen, Distinguished Professor
# San Diego State University
# Email: sshen@sdsu.edu
# www.climatestatistics.org
# R code Version 1.0: July 2023 San Diego, California, USA
################################################################


################################################################
#
# Chapter 1: Basics of Climate Data Arrays, 
#            Statistics, and Visualization
#
################################################################


# R plot Fig. 1.1: A simple line graph of data
# go to your working directory
setwd("/Users/sshen/climstats") 
# read the data file from the folder named "data"
NOAAtemp = read.table(
  "data/aravg.ann.land_ocean.90S.90N.v4.0.1.201907.txt",
  header=FALSE) #Read from the data folder
# check the data matrix dimension
dim(NOAAtemp)
#[1] 140   6 
#140 years from 1880 to 2019
#2019 will be excluded since data only up to July 2019
#col1 is year, col2 is anomalies, col3-6 are data errors
# set the plot margins and the positions of labels
par(mar=c(3.5,3.5,2.5,1), mgp=c(2,0.8,0))
plot(NOAAtemp[1:139,1], NOAAtemp[1:139,2],
     type ="o", col="brown", lwd=3,
     main ="Global Land-Ocean Average Annual Mean 
     Surface Temperature Anomalies: 1880-2018",
     cex.lab=1.2,cex.axis=1.2,
     xlab="Year", 
     ylab=expression(
       paste("Temperature Anomaly [", degree,"C]"))
)

#R plot Fig. 1.2: Staircase chart of data
plot(NOAAtemp[1:139,1], NOAAtemp[1:139,2],
     type="s", #staircase curve for data
     col="black", lwd=2,
     main="Global Land-Ocean Average Annual Mean 
     Surface Temperature Anomalies: 1880-2018",
     cex.lab=1.2,cex.axis=1.2,
     xlab="year",
     ylab=expression(paste(
       "Temperature Anomaly [", degree,"C]"))
)

# R plot Fig. 1.3: A color bar chart of data
x <- NOAAtemp[,1]
y <- NOAAtemp[,2]
z <- rep(-99, length(x))
# compute 5-point moving average
for (i in 3:length(x)-2) z[i] = 
  mean(c(y[i-2],y[i-1],y[i],y[i+1],y[i+2]))
n1 <- which(y>=0); x1 <- x[n1]; y1 <- y[n1]
n2 <- which(y<0); x2 <- x[n2]; y2 <- y[n2]
x3 <- x[2:length(x)-2]
y3 <- z[2:length(x)-2]
plot(x1, y1, type="h", #bars for data
     xlim = c(1880,2016), lwd=3, 
     tck = 0.02,  #tck>0 makes ticks inside the plot
     ylim = c(-0.7,0.7), xlab="Year", col="red",
     ylab = expression(paste(
       "Temperature Anomaly [", degree,"C]")),
     main ="Global Land-Ocean Average Annual Mean 
     Surface Temperature Anomalies: 1880-2018",
     cex.lab = 1.2, cex.axis = 1.2)
lines(x2, y2, type="h",
      lwd = 3, tck = -0.02, col = "blue")
lines(x3, y3, lwd = 2)

#
#R code for computing statistical indices
setwd("/Users/sshen/climstats")
NOAAtemp = read.table(
  "data/aravg.ann.land_ocean.90S.90N.v4.0.1.201907.txt",
  header=FALSE)
temp2018=NOAAtemp[1:139,2] #use the temp data up to 2018
head(temp2018) #show the first six values
#[1] -0.370221 -0.319993 -0.320088 -0.396044 -0.458355 -0.470374
mean(temp2018) #mean
#[1] -0.1858632
sd(temp2018) #standard deviation
#[1] 0.324757
var(temp2018) #variance
#[1] 0.1054671
library(e1071) 
#This R library is needed to compute the following parameters
#install.packages("e1071") #if it is not in your computer
skewness(temp2018) 
#[1] 0.7742704
kurtosis(temp2018)
#[1] -0.2619131
median(temp2018)
#[1] -0.274434
quantile(temp2018,probs= c(0.05, 0.25, 0.75, 0.95))
#       5%        25%        75%        95% 
#  -0.5764861 -0.4119770  0.0155245  0.4132383 

#
#R plot Fig. 1.4. Histogram nad its fit
par(mar=c(3.5,3.5,2.5,1), mgp=c(2,0.8,0))
h <- hist(NOAAtemp[1:139, 2], 
         main="Histogram of 1880-2018 Temperature Anomalies",
         xlab=expression(paste(
           "Temperature anomalies [", degree, "C]")), 
         xlim=c(-1,1), ylim=c(0,30),
         breaks=10, cex.lab=1.2, cex.axis=1.2) 
xfit <- seq(-1, 1, length=100)
areat <- sum((h$counts)*diff(h$breaks[1:2]))#Normalization area
yfit <- areat*dnorm(xfit, 
                    mean=mean(NOAAtemp[1:139,2]), 
                    sd=sd(NOAAtemp[1:139,2]))
#Plot the normal fit on the histogram
lines(xfit, yfit, col="blue", lwd=3) 

#
# R plot Fig. 1.5: Box plot
boxplot(NOAAtemp[1:139, 2], ylim = c(-0.8, 0.8), 
        ylab=expression(paste(
          "Temperature anomalies [", degree, "C]")),
        width=NULL, cex.lab=1.2, cex.axis=1.2)

#
# R plot Fig. 1.6: Q-Q plot for the standardized 
# global average annual mean temperature anomalies
temp2018 <- NOAAtemp[1:139,2]
tstand <- (temp2018 - mean(temp2018))/sd(temp2018)
set.seed(101)
qn <- rnorm(139) #simulate 139 points by N(0,1)
qns <- sort(qn) # sort the points
qq2 <- qqnorm(qns,col="blue",lwd = 2)

setEPS() #Automatically saves the .eps file
postscript("fig0106.eps", height=7, width=7)
par(mar = c(4.5,5,2.5,1), xaxs = "i", yaxs = "i")
qt = qqnorm(tstand, 
  main = "Q-Q plot for the Standardized Global Average 
  Annual Mean Temperature Anomalies vs N(0,1)", 
   ylab="Quantile of Temperature Anomalies", 
   xlab="Quantile of N(0,1)", 
  xlim=c(-3,3), ylim = c(-3,3),
          cex.lab = 1.3, cex.axis = 1.3)
qqline(tstand, col = "red", lwd=3)
points(qq2$x, qq2$y, pch = 19, 
       col ="purple")
dev.off()

# R plot Fig. 1.7: Data line graph with a linear trend line
par(mar=c(3.5,3.5,2.5,1), mgp=c(2,0.8,0))
plot(NOAAtemp[1:139,1], NOAAtemp[1:139,2],
     type="l", col="brown", lwd=3,
     main=" Global Land-Ocean Average Annual Mean 
Surface Temperature Anomalies: 1880-2018",
     cex.lab=1.2,cex.axis=1.2,
     xlab="Year", 
     ylab=expression(paste(
       "Temperature Anomaly [", degree,"C]"))
)
abline(lm(NOAAtemp[1:139,2] ~ NOAAtemp[1:139,1]),
       lwd=3, col="blue")
lm(NOAAtemp[1:139,2] ~ NOAAtemp[1:139,1])
#       (Intercept)  NOAAtemp[1:139, 1]  
#-13.872921            0.007023 
#Trend 0.7023 degC/100a
text(1930, 0.5, 
     expression(paste("Linear trend: 0.7023",
                      degree,"C/100a")),
     cex = 1.5, col="blue")


#
# Rea read the netCDF data: NOAAGlobalTemp 
setwd("/Users/sshen/climstats")  
#install.packages("ncdf4")
library(ncdf4)
nc = ncdf4::nc_open("data/air.mon.anom.nc")
nc # describes details of the dataset
Lat <- ncvar_get(nc, "lat")
Lat # latitude data
#[1] -87.5 -82.5 -77.5 -72.5 -67.5 -62.5
Lon <- ncvar_get(nc, "lon")
Lon # longitude data
#[1]   2.5   7.5  12.5  17.5  22.5  27.5 
Time<- ncvar_get(nc, "time")
head(Time) # time data in Julian days
#[1] 29219 29250 29279 29310 29340 29371
library(chron) # convert Julian date to calendar date
nc$dim$time$units # .nc base time for conversion
#[1] "days since 1800-1-1 00:00:0.0"
month.day.year(29219,c(month = 1, day = 1, year = 1800))
#1880-01-01 # the beginning time of the dataset
tail(Time)
#[1] 79988 80019 80047 80078 80108 80139
month.day.year(80139,c(month = 1, day = 1, year = 1800))
#2019-06-01 # the end time of the dataset

# extract anomaly data in (lon, lat, time) coordinates
NOAAgridT <- ncvar_get(nc, "air") 
dim(NOAAgridT) # dimensions of the data array
#[1]  72   36 1674 #5-by-5, 1674 months from Jan 1880-Jun 2019


#
#Plot Fig. 1.8: Dec 2015 global surface temp anomalies map
library(maps)# requires maps package 
mapmat=NOAAgridT[,,1632]
# Julian date time 1632 corresponds to Dec 2015
mapmat=pmax(pmin(mapmat,6),-6) # put values in [-6, 6]
int=seq(-6, 6, length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 
   'darkgreen', 'green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
par(mar=c(3.5, 4, 2.5, 1), mgp=c(2.3, 0.8, 0))
filled.contour(Lon, Lat, mapmat, 
               color.palette=rgb.palette, levels=int,
  plot.title=title(main="NOAAGlobalTemp Anomalies: Dec 2015",
            xlab="Latitude", ylab="Longitude", cex.lab=1.2),
  plot.axes={axis(1, cex.axis=1.2, las=1); 
                          axis(2, cex.axis=1.2, las=2);
                          map('world2', add=TRUE); grid()},
  key.title=title(main=expression(paste("[", degree, "C]"))),
  key.axes={axis(4, cex.axis=1.2)})


#Fig. 1.9 is plotted by Panoply
#
#R plot Fig. 1.10: Hovmoller diagram
library(maps)
mapmat=NOAAgridT[30,12:24,1309:1668]
#Longitude= 240 deg, Lat =[-30 30] deg
#Time=Jan 1989-Dec 2018: 30 years
mapmat=pmax(pmin(mapmat,2),-2) # put values in [-2,2]
par(mar=c(4,5,3,0))
int=seq(-2,2,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 
 'darkgreen','green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
par(mar=c(3.5,3.5,2.5,1), mgp=c(2.4, 0.8, 0))
x = seq(1989, 2018, len=360)
y = seq(-30, 30, by=5)
filled.contour(x, y, t(mapmat), 
       color.palette=rgb.palette, levels=int,
       plot.title=title(main=
    "Hovmoller diagram of the NOAAGlobalTemp Anomalies",
               xlab="Time",ylab="Latitude", cex.lab=1.2),
      plot.axes={axis(1, cex.axis=1.2); 
                 axis(2, cex.axis=1.2); 
                 map('world2', add=TRUE);grid()},
        key.title=title(main = 
                expression(paste("[", degree, "C]"))),
        key.axes={axis(4, cex.axis=1.2)})


#
#R read a 4-Dimensional netCDF file
setwd("/Users/sshen/climstats")
library(ncdf4)
# read GODAS data 1-by-1 deg, 40 levels, Jan-Dec 2015
nc=ncdf4::nc_open("data/godas2015.nc")
nc
Lat <- ncvar_get(nc, "lat")
Lon <- ncvar_get(nc, "lon")
Level <- ncvar_get(nc, "level")
Time <- ncvar_get(nc, "time")
head(Time)
#[1] 78527 78558 78586 78617 78647 78678
library(chron)
month.day.year(78527,c(month = 1, day = 1, year = 1800))
# 2015-01-01
# potential temperature pottmp[lon, lat, level, time] 
godasT <- ncvar_get(nc, "pottmp")
dim(godasT)
#[1] 360 418  40  12, 
#i.e., 360 lon, 418 lat, 40 levels, 12 months=2015
t(godasT[246:250, 209:210, 2, 12]) 
#Dec level 2 (15-meter depth) water temperature [K] of
#a few grid boxes over the eastern tropical Pacific
#        [,1]     [,2]     [,3]     [,4]     [,5]
#[1,] 300.0655 299.9831 299.8793 299.7771 299.6641
#[2,] 300.1845 300.1006 299.9998 299.9007 299.8045

#
# R plot Fig. 1.11: The ocean potential temperature
# the 20th layer from surface: 195 meters depth
# compute 2015 annual mean temperature at 20th layer
library(maps)
climmat=matrix(0,nrow=360,ncol=418)
sdmat=matrix(0,nrow=360,ncol=418)
Jmon<-1:12
for (i in 1:360){
  for (j in 1:418){
    climmat[i,j] = mean(godasT[i,j,20,Jmon]); 
    sdmat[i,j]=sd(godasT[i,j,20,Jmon]) 
                  }
                }
int=seq(273,298,length.out=81)
rgb.palette=colorRampPalette(c('black','blue',
    'darkgreen','green', 'white','yellow',
    'pink','red','maroon'), interpolate='spline')
setwd("/Users/sshen/climstats")
setEPS() # save the .eps figure file 
postscript("fig0111.eps", height = 5, width = 10)
par(mar=c(3.5, 3.5, 2.5, 0), mgp=c(2, 0.8, 0))
filled.contour(Lon, Lat, climmat, 
     color.palette=rgb.palette, levels=int,
    plot.title=title(main=
"GODAS 2015 Annual Mean Temperature at 195 [m] Depth Level",
            xlab="Longitude",ylab="Latitude",
            cex.lab=1.3, cex.axis=1.3),
plot.axes={axis(1); axis(2); map('world2', add=TRUE);grid()},
            key.title=title(main="[K]"))
dev.off()

################################################################
#
# Chapter 2: Elementary Probability and Statistics  
#
################################################################
#Plot Fig. 2.1
# Go to your working directory
setwd("/Users/sshen/climstats") 
nycDat=read.csv("data/NYCDailyDatasummer2019.csv", header=T)
dim(nycDat)
#[1] 7555   13
dayNum=92
nycP=nycDat[1:dayNum, c(3,6)]

dw=c()
for (i in 1:dayNum){if (nycP[i,2] >= 0.2){dw[i] = 1} 
  else {dw[i]=0} }
dw
n0=which(dw==0)
n1=which(dw==1)
m=dayNum - 1
par(mfrow=c(1,1))
par(mar=c(2.5,4.5,2.5,4))
plot(1:dayNum, nycP[,2], type="s",
     main="Daily Precipitation of New York City: 
      1 Jun 2019- 31 Aug 2019",
     ylab="Precipitation [mm/day]", 
     xaxt="n", xlab="",
     cex.lab=1.4, cex.axis=1.4)
axis(1, at=c(1, 30, 61, 92), 
     labels=c("1 June", "1 July", "1 Aug", "31 Aug"),
     cex.axis=1.4)
par(new=TRUE)
plot(n0+1.0, dw[n0]-0.2, cex.lab=1.4,
     ylim=c(0,15), pch=15, cex=0.4, col="red",
     axes=FALSE, xlab="", ylab="")
points(n1+1, dw[n1], pch=15, col="blue", cex=0.4)
axis(4, col="blue", col.axis="blue", 
     at=c(0,1), labels =c("D", "W"), 
     las=2, cex.axis=1)
mtext("Dry or Wet Days", 
      col="blue", side=4, line=1.5, cex=1.4)

#
#Plot Fig. 2.2
p=0.6471
n=1:12
pdfn= (1-p)*p^n
par(mar=c(4.5,4.5,3,0.5))
plot(n, pdfn, 
     ylim=c(0,0.25), type="h", lwd=15,
     cex.lab=1.5, cex.axis=1.5,
     xlab="Length of Dry Spell: n", ylab="Probability",
     main="Probability Distribution Function of Dry Spell")
text(1.5,0.25, "(a)",cex=1.5)

p=0.6471
N=1:12
cdfN= 1-p^N
par(mar=c(4.5,4.5,3,0.5))
plot(N, cdfN, 
     ylim=c(0,1), type="s", lwd=3,
     cex.lab=1.5, cex.axis=1.5,
     xlab="N: Dry Spell Not Longer Than N Days", 
     ylab="Cumulative Probability",
     main="Cumulative Distribution Function of Dry Spell")
text(1.5,0.95, "(b)",cex=1.5)

#Plot Fig. 2.3
n=20
x=0:n
pdfdx=dbinom(x, size=n, prob=0.3)
par(mar=c(4.5,4.5,2,4.5))
plot(x, pdfdx, type="h", lty=1, lwd=3,
     xlab="Number of successes: k", ylab="Probability", 
     main="Binomial Distribution: n=20, p=0.3",
     cex.lab=1.4, cex.axis=1.4)
text(11, 0.05, "PDF", cex=2)
par(new=TRUE)
cdfx=pbinom(x, size=n, prob=0.3)
plot(x, cdfx, col="blue",
     type="o", lty=2, axes=FALSE,
     xlab="", ylab="", cex.axis=1.4)
axis(4, col="blue", col.axis = "blue", cex.axis=1.4)
mtext("Cumulative Probability", col="blue",
      side=4, line=3, cex=1.4)
text(11, 0.9, "CDF", cex=2, col="blue")


#Plot Fig. 2.3: Save as an eps file
setwd("/Users/sshen/climstats")
setEPS() # save the .eps figure file to the working directory
postscript("fig0203.eps", width = 7, height = 5)
n=20
x=0:n
pdfdx=dbinom(x, size=n, prob=0.3)
par(mar=c(4.5,4.5,2,4.5))
plot(x, pdfdx, type="h", lty=1, lwd=3,
     xlab="Number of successes: k", ylab="Probability", 
     main="Binomial Distribution: n=20, p=0.3",
     cex.lab=1.4, cex.axis=1.4)
text(11, 0.05, "PDF", cex=2)
par(new=TRUE)
cdfx=pbinom(x, size=n, prob=0.3)
plot(x, cdfx, col="blue",
     type="o", lty=2, axes=FALSE,
     xlab="", ylab="", cex.axis=1.4)
axis(4, col="blue", col.axis = "blue", cex.axis=1.4)
mtext("Cumulative Probability", col="blue",
      side=4, line=3, cex=1.4)
text(11, 0.9, "CDF", cex=2, col="blue")
dev.off()

#
#Plot Fig. 2.4
x=seq(-5,5, len=101)
y=dnorm(x)
plot(x, y, 
     type="l", lty=1, 
     xlab="x", ylab="Probability Density f(x)", 
     main="Standard Normal Distribution",
     cex.lab=1.4, cex.axis=1.4)
lines(x,rep(0, 101))
xx=c(x[60], x[60], x[70], x[70])
yy=c(0, y[60], y[70], 0)
polygon(xx, yy, col="brown") 
text(0.7, 0.01, "a", col="brown")
text(2.1, 0.01, "b", col="brown")
text(1.7,0.2, expression(f(x)), cex=1.4)
par(new=TRUE)
plot(x, pnorm(x), type="l", lty=2, col="blue",
     axes=FALSE, ylab="", xlab="", cex.axis=1.4)
axis(4, col="blue", col.axis = "blue", cex.axis=1.4)
text(3,0.9, expression(F(x)), cex=1.4, col="blue")
mtext("Cumulative Probability F(x)", col="blue",
      side=4, line=3, cex=1.4)
text(3.3,0.3, cex=1.3, col="brown",
     expression(P[ab]==integral(f(x)*dx, a,b)))

#
#R plot Fig. 2.5: 2023 version
par(mar=c(4.5, 4.5, 2.5, 4.5))
x<- rnorm(200, mean=5, sd=3)
h<-hist(x, breaks=15, col="gray", 
        xlim=c(-5,15), ylim=c(0,30),
        xlab=expression("Random numbers x of N(5," * 3^2 *")"),
        ylab="Frequency",
        main="Histogram and Its Normal PDF Fit", 
        cex.lab=1.4, cex.axis=1.4)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
#yfit <- yfit*diff(h$mids[1:2])*length(x)
yfit <- yfit
#diff(h$mids[1:2])*length(x) =200 
#is the total histogram area
points(x, rep(0, 200), cex=0.8, pch=3, col="blue")
par(new = TRUE)
plot(xfit, yfit, type = 'l', lwd = 3, axes = FALSE,
     xlab = "", ylab = "", col = 'red')
axis(4, col='red', col.axis="red", cex.axis =1.4)
mtext("Density", side =4, line = 3, cex = 1.4,
      col = 'red')

#Plot Fig. 2.5: The book version
par(mar=c(4.5, 4.5, 2.5, 0.5))
x<- rnorm(200, mean=5, sd=3)
h<-hist(x, breaks=15, col="gray", 
        xlim=c(-5,15), ylim=c(0,30),
        xlab=expression("Random numbers x of N(5," * 3^2 *")"),
        ylab="Frequency/Density",
        main="Histogram and Its Normal Curve Fit", 
        cex.lab=1.4, cex.axis=1.4)
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
#diff(h$mids[1:2])*length(x) =200 
#is the total histogram area
points(x, rep(0, 200), cex=0.8, pch=3, col="blue")
lines(xfit, yfit,  lwd=2)


#
#Plot Fig. 2.6
#install.packages("ggExtra") 
library(ggplot2)
df <- data.frame(x = rnorm(1000, 5, 3), y = runif(1000, 0, 5))
p <- ggplot(df, aes(x, y)) + geom_point() + theme_classic()
ggExtra::ggMarginal(p, type = "histogram")

#
#Plot Fig. 2.7: R code for Poisson distribution
k = 0:20
y = dpois(k, lambda = 5)
par(mar=c(4.5,4.5,2,4.5))
plot(k, y, 
     type="p", lty=1, xlab="k", ylab="", 
     main="Poisson Distribution: Mean Rate = 5",
     cex.lab=1.4, cex.axis=1.4)
mtext(side=2, line=3, cex=1.4, 'Probability f(k)')
par(new=TRUE)
plot(k, ppois(k, lambda = 5), type="p", pch=16, 
     lty=2, col="blue",
     axes=FALSE, ylab="", xlab="", cex.axis=1.4)
axis(4, col="blue", col.axis = "blue", cex.axis=1.4)
mtext("Cumulative Probability F(k)", col="blue",
      side=4, line=3, cex=1.4)

##
dpois(10, lambda=5)
##

#
#Plot Fig. 2.8: R code
x=seq(0,20, len=201)
ycauchy=dcauchy(x, location = 10, scale=1)
ygauss=dnorm(x, mean=10, sd=sqrt(pi/2))
plot(x, ycauchy, type="l", lwd=2, 
     ylab="Density", 
     main="Comparison between Cauchy and Gaussian Distributions",
     cex.lab=1.4, cex.axis=1.4)
legend(-1,0.35, legend="Cauchy distribution", 
       cex=1.2, bty="n", lty=1, lwd=2)
lines(x, ygauss, lty=2)
legend(-1,0.32, legend="Normal distribution", 
       cex=1.2, bty="n", lty=2)


##
x= seq(1,10, by=0.2)
plot(x,dchisq(x, df = 4))
##

#
#Plot Fig. 2.9: R code
setwd("/Users/sshen/climstats")
da1 =read.csv("data/EdmontonT.csv", header=TRUE)
x=da1[,3]
m1 = mean(x)
s1 = sd(x)
xa = (x- m1)/s1
y = matrix(xa[1:1632], ncol=6, byrow=TRUE)
w = rowSums(y^2)
hist(w, breaks= seq(0,40,by=1),
     xlim=c(0,40), ylim=c(0,50),
     main="Histogram and its Chi-square Fit
for Edmonton temperature data", 
     xlab=expression("Chi-square data ["~degree~C~"]"^2),
     cex.lab=1.4, cex.axis=1.4)
par(new=TRUE)
density = function(x) dchisq(x, df=6)
x=seq(0,40, by=0.1)
plot(x, density(x), type="l", lwd=3,
     col="blue", ylim=c(0,0.15),
     axes=FALSE, bty="n", xlab="", ylab="")
axis(4, cex.axis=1.4, col='blue', col.axis='blue')
mtext("Chi-square Density", cex=1.4,
      side = 4, line = 3, col="blue")
text(20, 0.1, cex=1.4, col="blue",
     "Chi-square distribution: df=6")

#Lognormal distribution of the June Madison precipitation
setwd("/Users/sshen/climstats")
Madison = read.csv("data/MadisonP.csv", header=TRUE)
Madison[1:2,]#Take a look at the first two lines of the data
daP = matrix(Madison[,7], ncol=12, byrow=TRUE)
x = daP[,6] #The June precipitation data
n=length(x)
m1=mean(x)
A = log(m1)- sum(log(x))/n
c = (1/(4*A))*(1 + sqrt(1 + 4*A/3))
beta = c/m1
mu=mean(log(x))
sig = sd(log(x))
round(c(mu, sig), digits = 2)
#[1] 4.52 0.65
modelmean = exp(mu + 0.5*sig^2)
modelsd = sqrt((exp(sig^2)-1)*exp(2*mu + sig^2))
datamean = mean(x)
datasd = sd(x)
round(c(modelmean, datamean, modelsd, datasd), digits =2)
#[1] 112.81 109.79  81.26  63.94



################################################################
#
# Chapter 3:  Estimation and Decision Making 
#
################################################################

# Plot Fig. 3.1: R code
#Simulation of the standard error of mean
setwd("/Users/sshen/climstats")
setEPS() # save the .eps figure file to the working directory
postscript("fig0301.eps", height = 5, width = 10)
mu = 0; sig = 10; m=10000; n = 100; k = m*n
x = rnorm(k, mean = mu, sd = sig)
par(mar = c(4, 4.5, 2, 1.5))
par(mfrow=c(1,2))
hist(x, breaks = 201, xlim = c(-40,40),
     main = expression('Histogram of x: sd(x) = 10'),
     cex.lab = 1.5, cex.axis = 1.5,
     cex.main = 1.5)
text(-25, 19000, '(a)', cex =1.3)
xmat = matrix(x, ncol = n)
xbar = rowMeans(xmat)
hist(xbar, breaks = 31, xlim = c(-40,40),
     xlab = expression(bar(x)),
     main = expression('Histogram of '*bar(x)* 
                         ': sd('*bar(x)*') = 1'),
     cex.lab = 1.5, cex.axis = 1.5,
     cex.main = 1.5)
text(-25, 750, '(b)', cex =1.3)
dev.off()


#
#Verification of 95% CI by numerical simulation
m=10000 #10,000 experiments
x = 1:m
n = 30 #sample size n
truemean = 8
da = matrix(rnorm(n*m, mean = truemean, sd = 1), nrow = m)
esmean = rowMeans(da) #sample mean
library(matrixStats)
essd = rowSds(da) #sample SD
upperci = esmean + 1.96*essd/sqrt(n) #interval upper limit
lowerci = esmean - 1.96*essd/sqrt(n) #interval lower limit
l=0
for(k in 1:m){
  if(upperci[k] >= truemean & lowerci[k] <= truemean )
    l=l+1
} #Determine if the true mean is inside the interval
l/m #Percentage of truth
#[1] 0.9425 #which is approximately 0.95


#
#Plot Fig. 3.2: R code
#Plot confidence intervals and tail probabilities
setwd("/Users/sshen/climstats")
setEPS() # save the .eps figure file to the working directory
postscript("fig0302.eps", height = 4.5, width = 6.5)
par(mar=c(2.5,3.5,2.0,0.5))
rm(list=ls())
par(mgp=c(1.4,0.5,0))
curve(dnorm(x,0,1), xlim=c(-3,3), lwd=3,
      main='Confidence Intervals and Confidence Levels',
      xlab="True mean as a normally distributed random variable", 
      ylab="", xaxt="n", cex.lab=1.2) 
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
dev.off()


#
#R code for Edmonton Jan 1880-1929 temp statistics  
setwd('/Users/sshen/climstats')
da1 =read.csv("data/Lat52.5_Lon-112.5.csv", header=TRUE)
dim(da1)
#[1] 1642    3 #1642 months: Jan 1880 - Oct 2016
da1[1:2,]
#        Date   Value
#1 1880-01-01 -7.9609
#2 1880-02-01 -4.2510
jan = seq(1, 1642, by=12)
Tjan = da1[jan,]
TJ50 = Tjan[1:50, 3]
xbar = mean(TJ50)
sdEdm = sd(TJ50)
EM = 1.96*sdEdm/sqrt(50) 
CIupper = xbar + EM
CIlower = xbar - EM
round(c(xbar, sdEdm, EM, CIlower, CIupper), digits =2)
#[1] -2.47  4.95  1.37 -3.84 -1.10

#
#Plot Fig. 3.3: R code
#52.5N, 112.5W, Edmonton, NOAAGlobalTemp Version 3.0
setwd("/Users/sshen/climstats")
setEPS() # save the .eps figure file to the working directory
postscript("fig0303.eps", height = 7, width = 10)
da1 =read.csv("data/Lat52.5_Lon-112.5.csv", header=TRUE)
jan =seq(1, 1642, by=12)
Tjan = da1[jan,]
t=1880:2016
regJan = lm(Tjan[,3] ~ t)
par(mar=c(3.6,4.5,2,1.3), mgp = c(2.3, 0.9, 0))
plot(t, Tjan[,3], type="l",
     main = "Edmonton January Surface Air Temperature Anomalies",
     xlab = "Time: 1880-2016", 
     ylab = expression("Temperature Anomaly:"~degree~C),
     ylim=c(-20,10), cex.lab=1.4, cex.axis=1.4)
m19972006 = mean(Tjan[118:137, 3]) #1997--2016 Jan mean
m18801929 = mean(Tjan[1:50, 3]) #1880--1929 Jan mean
EM = 1.96*sd(Tjan[1:50, 3])/sqrt(50)
lines(t, rep(0,length(t)), lty = 2)
lines(t[118:137], rep(m19972006, 20), 
      col = 'blue', lwd = 3, lty = 1)
lines(t[1:50], rep(m18801929, 50), 
      col = 'darkgreen', lwd = 3, lty = 1)
lines(t[1:50], rep(m18801929 - EM, 50), 
      col = 'darkgreen', lwd = 1.5, lty = 3)
lines(t[1:50], rep(m18801929 + EM, 50), 
      col = 'darkgreen', lwd = 1.5, lty = 3)
abline(regJan, col='red', lwd = 2)
text(1920, 9, 
     expression("Linear trend: 3.78"~degree~C~"per century"), 
     col="red", cex=1.4) 
text(2005, -17, 
     paste('1997-2016', '\nmean = ', 
           round({m19972006}, digits = 2)),
     col = 'blue', cex = 1.4)
text(1905, -17, 
     paste('1880-1929', '\nmean = ', 
           round({m18801929}, digits = 2)),
     col = 'darkgreen', cex = 1.4)
dev.off()


#
#Plot Fig. 3.4: R code
setwd("/Users/sshen/climstats")
setEPS() # save the .eps figure file to the working directory
postscript("fig0304.eps", height = 7, width = 10)
x = seq(-3,6, len=1000)
par(mar=c(0.5,0,2.5,0.0))
plot(x, dnorm(x,0,1), type="l",
     lwd=2, bty='n',
     main='Probability Density Functions of 
Null and Alternative Hypotheses for Inference',
     xlab='', ylab='',
     xaxt="n",yaxt="n",
     cex.lab=1.2, ylim=c(-0.05,0.6)) 
lines(x, dnorm(x,2.5,1), 
      lwd=2, lty=2,
      type="l", col='blue')
segments(-3,0, 6,0)
#lines(c(-3,6),c(0,0))
polygon(c(2.0,seq(2.0, 4, length=100), 4),
        c(0, dnorm(seq(2, 4, length=100)), 0.0),col='pink')
arrows( 2.6,0.08, 2.25,0.015, length=0.2, angle=8, 
        lwd=2, col='red')
text(3.5,0.09,expression(alpha*": Significance level"), 
     col='red', cex=1.4)
polygon(c(-1,seq(-1, 2, length=100), 2),
        c(0, dnorm(seq(-1, 2, length=100), 2.5,1), 0),
        col='lightblue')
lines(x, dnorm(x,0,1), type="l") 
text(1.5,0.05,expression(beta), col='blue', cex=1.5)
segments(2,-0.05,2,0.6, col='red')
points(2,0, col='red', pch=16)
text(1.3,-0.06, expression("Critical value "*x[c]), 
     cex=1.4, col='red')
segments(0,0, 0,0.5, lty=3, lwd=1.5)
segments(2.5,0, 2.5,0.5, lty=3, lwd=1.5, col='blue')
lines(x, dnorm(x,0,1), type="l") 
arrows( 3.0,0.038, 2.7,0.005, length=0.2, angle=8, 
        lwd=2, col='blue')
text(3.2,0.05,"p-value", col='blue', cex=1.5)
points(2.5,0, col='blue', pch=16)
text(-1.3,0.55, "Probability density 
function of the  
H0 distribution", cex=1.4)
text(4,0.55, "Probability density 
function of the test statistic 
when H1 is true", cex=1.4, col='blue')
text(3,-0.03, expression("Statistic "*x[s]), 
     cex=1.4, col='blue')
arrows( 2.0,0.45, 6,0.45, length=0.2, angle=10, 
        lwd=1.5, col='blue', code=3)
text(4,0.42, expression("Accept"~H[1]), 
     cex=1.4, col='blue')
arrows( -2,0.45,2.0,0.45,  length=0.2, angle=10, 
        lwd=1.5, code=3)
text(1,0.42, expression("Accept"~H[0]), cex=1.4)
text(-0.5,0.09, expression(1- alpha *": Confidence level"), 
     col='grey40', cex=1.4)
text(3,0.17, expression(1-beta*': Power'), 
     col='darkgreen', cex=1.4)
arrows(0,0.49, 2.5,0.49, length=0.2, angle=20, 
       lwd=1.5, col='maroon', code=3)
text(1.3,0.52, "Difference",  col='maroon', cex=1.4)
text(0,-0.04,expression(mu[0]),  cex=1.6)
dev.off()


# Edmonton 1997-2016 hypothesis testing example
setwd("/Users/sshen/climstats")
da1 =read.csv("data/Lat52.5_Lon-112.5.csv", header=TRUE)
jan =seq(1, dim(da1)[1], by=12) #Jan indices
Tjan = da1[jan,] #Jan data matrix
da3=Tjan[118:137, 3] #1997--2016 data string
xbar = mean(da3); s = sd(da3); EM = 1.96*s/sqrt(20)
round(c(xbar, s, EM, xbar - EM, xbar + EM), digits = 2)
#[1] 1.53 2.94 1.29 0.24 2.83
mean(da3)/(sd(da3)/sqrt(20))
#[1] 2.331112 #t-statistic xs
1 - pt(2.331112, 19)
#[1] 0.01545624  less than the significance level 0.05


#
#Plot Fig. 3.5: R code
#AR(1) process and its simulation
setwd("/Users/sshen/climstats")
setEPS() # save the figure to an .eps file 
postscript("fig0305.eps", height = 8, width = 10)
par(mar=c(4.0,4.5,2,1.0))
par(mfcol = c(2, 2))

lam=0.9; alf=0.25; y0=4 #AR1 process parameters
#install.packages('forecast')
library(forecast) #Load the forecast package
#set.seed(1234)
n <- 1000 #Generate an AR1 process of length n 
x <- rep(0,n)
w <- rnorm(n)
x[1] <- y0
# loop to create time series x
for (t in 2:n) x[t] <- lam * x[t-1] + alf * w[t]
par(mar=c(4.5,4.5,2,0.5))
plot(201:400, x[201:400],type='l', 
     xlab="Time", ylab="x", 
     main=expression("AR(1) Time Series: " * rho *"=0.9"),
     cex.lab=1.8, cex.axis=1.8, cex.main=1.6)
text(15,3.5, "(a)", cex=1.8)

#Calculate the auto-correlation
M <- 10
rho <- rep(0,M)
for (m in 1:M){
  rho[m]=cor(x[1:(n-m)],x[(1+m):n])
}
par(mar=c(4.5,4.5,2,0.5))
plot(rho, type='p', ylim=c(0,1), 
     xlab="Time lag", 
     ylab="Auto-correlation",
main=expression("Lagged auto-correlation: " * rho *"=0.9"),
     lwd=1.8, cex.lab=1.8, cex.axis=1.8, cex.main=1.6)
text(2,0.95, "(c)", cex=1.8)

rhotheory <- rep(0,M) #Theoretical auto-correlation
for (m in 1:M){rhotheory[m]=lam^m}
points(rhotheory, col="blue", pch=3, lwd=3)

#AR1 process for rho = 0.6
lam=0.6; alf=0.25; y0=4
n <- 1000; x <- rep(0,n); w <- rnorm(n); x[1] <- y0

# loop to create time series x
for (t in 2:n) x[t] <- lam * x[t-1] + alf * w[t]
par(mar=c(4.5,4.5,2,0.5))
plot(201:400, x[201:400],type='l', 
     xlab="Time", ylab="x", 
main=expression("AR(1) Time Series: " * rho *"=0.6"),
     cex.lab=1.8, cex.axis=1.8, cex.main=1.6)
text(15,3.5, "(b)", cex=1.8)

#Calculate the auto-correlation
M <- 10
rho <- rep(0,M)
for (m in 1:M){
  rho[m]=cor(x[1:(n-m)], x[(1+m):n])
}
par(mar=c(4.5,4.5,2,0.5))
plot(rho, type='p', ylim=c(0,1), 
     xlab="Time lag", 
     ylab="Auto-correlation",
main=expression("Lagged auto-correlation: " * rho *"=0.6"),
     lwd=1.8, cex.lab=1.8, cex.axis=1.8, cex.main=1.6)
text(2,0.95, "(d)", cex=1.8)

rhotheory <- rep(0,M) #Theoretical auto-correlation
for (m in 1:M){rhotheory[m]=lam^m}
points(rhotheory, col="blue", pch=3, lwd=3)
dev.off()
# Back to the original graphics device
par(mfrow = c(1, 1))



#
#R plot Fig. 3.6: NOAAGlobalTemp V5 1880-2019
setwd("/Users/sshen/climstats")
da1 =read.table("data/NOAAGlobalTempAnn2019.txt", 
                header=FALSE) #read data
dim(da1) 
#[1] 140   6 #140 years of anomalies data
da1[1:2,1:5] #column 1: year; column 2: data
#    V1        V2       V3       V4       V5
#1 1880 -0.432972 0.009199 0.000856 0.000850
#2 1881 -0.399448 0.009231 0.000856 0.000877
da1[139:140,1:5]
#      V1       V2       V3      V4    V5
#139 2018 0.511763 0.005803 8.6e-05 2e-06
#140 2019 0.633489 0.005803 8.6e-05 2e-06
t=da1[,1]; Tann = da1[,2]
regAnn = lm(Tann ~ t)
setEPS() # save the figure to an .eps file 
postscript("fig0306.eps", height = 7, width = 10)
par(mar=c(4.0,4.5,2,1.0))
plot(t, Tann, type="l", lwd=2.5,
main = "NOAA Global Average Annual Mean SAT Anomalies",
     xlab = "Time", ylab = "Temperature Anomalies [deg C]",
     cex.lab=1.4, cex.axis=1.4)
abline(lm(Tann ~t), col='red', lwd=1.3)
lines(t, rep(0, 140), col='blue')
lines(t[41:70], rep(mean(Tann[41:70]), 30), 
      col='green', lwd=4) # 1920-1949 mean
lines(t[71:100], rep(mean(Tann[71:100]), 30), 
      col='green', lwd=4) #1950-1979 mean
lines(t[101:130], rep(mean(Tann[101:130]), 30), 
      col='green', lwd=4) #1980-2010 mean
lm(Tann ~t)
#(Intercept)            t  
#-14.741366     0.007435
text(1940, 0.5, 
     expression("Linear trend 0.74"~degree~C~"per century"),
     col='red', cex=1.4)
#0.007435 #0.7 deg C per century
dev.off()

#
#R code for the 1920-1949 NOAAGlobalTemp statistics
setwd("/Users/sshen/climstats")
da1 =read.table("data/NOAAGlobalTempAnn2019.txt", 
                header=FALSE) #read data
Ta = da1[41:70,2]
n = 30
xbar = mean(Ta)
s = sd(Ta)
r1 = cor(Ta[1:29], Ta[2:30])
neff = n*(1 - r1)/(1 + r1)
neff = 4 #[1] 3.677746 approximately equal to 4
print(paste('rho =', round(r1, digits =2), 
            'neff =', round(neff, digits =2)))
#[1] "rho = 0.78 neff = 4"
tc0 = qt(0.975, 29, lower.tail=TRUE)#dof of t is 30 - 1
tc = qt(0.975, 3, lower.tail=TRUE)#dof of t is neff -1 
CI1 = xbar - tc0*s/sqrt(n); CI2 = xbar + tc0*s/sqrt(n)
CI3 = xbar - tc*s/sqrt(neff); CI4 = xbar + tc*s/sqrt(neff)
round(c(xbar, s, r1, CI1, CI2, CI3, CI4, tc), digits = 2)
#[1] -0.38  0.16  0.78 -0.44 -0.32 -0.63 -0.13  3.18


#Old version code for the CIs not included in the book: 
#R code for the 1920-1949 NOAAGlobalTemp statistics
setwd("/Users/sshen/climstats")
da1 =read.table("data/NOAAGlobalTempAnn2019.txt", 
                header=FALSE) #read data
Ta = da1[41:70,2]
n = 30
xbar = mean(Ta)
s = sd(Ta)
r1 = cor(Ta[1:29], Ta[2:30])
neff = n*(1 - r1)/(1 + r1)
neff #[1] 3.677746 approximately equal to 4
neff = 4
CI1 = xbar - s/sqrt(n); CI2 = xbar + s/sqrt(n)
CI3 = xbar - s/sqrt(neff); CI4 = xbar + s/sqrt(neff)
print(paste('rho =', round(r1, digits =2), 
            'neff =', round(neff, digits =2)))
#[1] "rho = 0.78 neff = 4"
tc = qt(0.975, 4, lower.tail=TRUE)
round(c(xbar, s, r1, CI1, CI2, CI3, CI4, tc), digits = 2)
#[1] -0.38  0.16  0.78 -0.41 -0.35 -0.46 -0.30  2.78

#dof of t is n_eff - 1 =3
tc0 = qt(0.975, 29, lower.tail=TRUE)
tc = qt(0.975, 3, lower.tail=TRUE)
CI1 = xbar - tc0*s/sqrt(n); CI2 = xbar + tc0*s/sqrt(n)
CI3 = xbar - tc*s/sqrt(neff); CI4 = xbar + tc*s/sqrt(neff)
round(c(xbar, s, r1, CI1, CI2, CI3, CI4, tc), digits = 2)
#[1] -0.38  0.16  0.78 -0.44 -0.32 -0.63 -0.13  3.18
##Done with the CI computing. The old CI code ends here. 

#
# R code for the 1950-1979 and 1980-2009 statistics
#  The 1950-1979 NOAAGlobalTemp statistics
Ta = da1[71:100,2] #1950-1979
n = 30
xbar = mean(Ta)
s = sd(Ta)
r1 = cor(Ta[1:29], Ta[2:30])
neff = n*(1 - r1)/(1 + r1)
neff #[1] 19.02543 approximately equal to 19
neff = 19
round(c(xbar, s, r1, neff), digits = 2)
#[1] -0.29  0.11  0.22 19.00

# The 1980-2009 NOAAGlobalTemp statistics
Ta = da1[101:130,2]#1980-2009
n = 30
xbar = mean(Ta)
s = sd(Ta)
r1 = cor(Ta[1:29], Ta[2:30])
neff = n*(1 - r1)/(1 + r1)
neff #[1] 4.322418 approximately equal to 4
neff = 4
round(c(xbar, s, r1, neff), digits = 2)
#[1] 0.12 0.16 0.75 4.00

#t-statistic for 1950-1979 and 1980-2009 difference
ts = (-0.29 - 0.12)/sqrt(0.11^2/19 + 0.16^2/4)
ts  #[1] -4.887592


#
#Chi-square test for Dodge City, Kansas, USA
((9-15)^2)/15+ ((6-7)^2)/7 + ((16-9)^2)/9
#7.987302 #This is the Chi-statistic
1 - pchisq(7.987302, df=2)  #Compute the tail probability
#[1] 0.01843229 #p-value
1 - pchisq(5.99, df=2) #Compute the tail probability
#[1] 0.05003663 # Thus, xc = 5.99

#
#R: Omaha monthly precipitation and Gamma
#Read the Omaha monthly precipitation data
setwd("/Users/sshen/climstats")
Omaha=read.csv("data/OmahaP.csv", header=TRUE)
dim(Omaha)
#[1] 864   7  : Jan 1948-Dec 2019: 864 months, 72 years*12
daP = matrix(Omaha[,7], ncol=12, byrow=TRUE)
y = daP[,6] #Omaha June precipitation data 
#Fit the data
#install.packages('fitdistrplus')
library(fitdistrplus)
omahaPfit = fitdist(y, distr = "gamma", method = "mle")
summary(omahaPfit)
#        estimate  Std. Error
#shape 1.51760921 0.229382819
#rate  0.01889428 0.003365757


#Plot the figure: Fig. 3.7
setEPS() # save the .eps figure 
postscript("fig0307.eps", height = 5.6, width = 8)
par(mar=c(4.2,4.2,2.5,4.5))
hist(y, breaks= seq(0,300,by=25),
     xlim=c(0,300), ylim=c(0,20),
     main="Histogram and Its Fitted Gamma Distribution 
     for Omaha June Precip: 1948-2019", 
     xlab="Precipitation [mm]", cex.lab=1.4, cex.axis=1.4)
par(new=TRUE)
density = function(y) dgamma(y, shape=1.5176, rate=0.0189)
plot(y, density(y), col="blue",
     pch=20, cex=0.5, ylim=c(0,0.01),
     axes=FALSE, bty="n", xlab="", ylab="")
axis(4, cex.axis=1.4, col='blue', col.axis='blue')
mtext("Gamma Density", cex=1.4,
      side = 4, line = 3, col="blue")
text(140, 0.009, cex=1.4, col="blue",
     "Gamma: Shape=1.5176, Rate=0.0189")
dev.off()


#
#R code: Chi-square test for the goodness of fit: Omaha prcp
setwd("/Users/sshen/climstats")
Omaha=read.csv("data/OmahaP.csv", header=TRUE)
dim(Omaha)
#[1] 864   7  : Jan 1948-Dec 2019: 864 months, 72 years*12
daP = matrix(Omaha[,7], ncol=12, byrow=TRUE)
y = daP[,6] #Omaha June precipitation data 
n = 72 #Total number of observations
m = 12 #12 bins for the histogram in [0, 300] mm
p1 = pgamma(seq(0,300, by=300/m), 
            shape=1.5176, rate=0.0189)
p1[m+1] = 1
p2 = p1[2:(m+1)]-p1[1:m]
y # The 72 years of Omaha June precipitation
cuts = cut(y, breaks = seq(0,300,by=300/m))
#The cut function in R assigns values into bins
Oi <- c(t(table(cuts))) #Extract the cut results
Ei = round(p2*n, digits=1) #Theoretical results
rbind(Oi, Ei)
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#Oi 9 17.0 16.0  7.0  8.0  5.0  5.0  1.0  2.0   1.0   1.0  0.0
#Ei13 15.6 12.8  9.5  6.8  4.7  3.2  2.1  1.4   0.9   0.6  1.2
sum(((Oi - Ei)^2)/Ei)
#[1] 6.350832 #Chi-square statistic
1 - pchisq(19.68, df=11) #Compute the tail probability
#[1] 0.04992718 # Thus, xc = 19.68
1 - pchisq(6.3508, df=11) #Compute the tail probability
#[1] 0.8489629 #p-value

#
#K-S test for a set of generated data from N(0,1): R code
x = rnorm(60)
ks.test(x, "pnorm", mean=0, sd=1)
#D = 0.10421, p-value = 0.4997 
#The large p-value implies no significant difference

#K-S test of uniformly distributed data vs N(0,1) population
u = runif(60)
ks.test(u, "pnorm", mean=0, sd=1)
#D = 0.50368, p-value = 1.366e-14
#The small p-value implies significant difference

#
#Plot Fig. 3.8: K-S test and R code
setEPS() # save the .eps figure 
postscript("fig0308.eps", height = 5.6, width = 8)
par(mar=c(4.2,4.5,2.5,4.5))
setwd("/Users/sshen/climstats")
da1 =read.csv("data/EdmontonT.csv", header=TRUE)
x=da1[,3]
m1 = mean(x)
s1 = sd(x)
xa = (x- m1)/s1
ks.test(xa, "pnorm", mean=0, sd=1)
#Dn = 0.074186, p-value = 2.829e-08 => significant difference
plot(ecdf(xa), pch =3,
     xlim=c(-5,5),ylim=c(0,1),
  main="Cumulative Distributions: Data vs Model", 
  xlab=expression(paste('Temperature Anomalies [',degree,'C]')), 
  ylab=expression(paste(F[obs],': Percentile/100')),
  cex.lab=1.4, cex.axis=1.4)
par(new=TRUE)
x=seq(-5,5, by=0.1)
lines(x,pnorm(x), col='blue')
axis(4, cex.axis=1.4, col='blue', 
     col.axis='blue')
mtext(expression(paste(F[exp],': CDF of N(0,1)')), 
      cex=1.4, side = 4, line = 3, col="blue")
text(2, 0.05, cex=1.4, col="red",
     expression(paste('K-S test: ', D[n], 
                     '= 0.0742, p-value' %~~% '0' )))
text(2, 0.22, cex=1.4, col="red",
     expression(paste(D[n], '= max|', 
                      F[obs] -F[exp], '|')))
m=46
segments(x[m],pnorm(x[m]), 
         x[m],pnorm(x[m])- 0.0742,
         lwd=2, col='red')
dev.off()


#
# R: K-S test for Omaha June precip vs Gamma 
Omaha=read.csv("data/OmahaP.csv", header=TRUE)
daP = matrix(Omaha[,7], ncol=12, byrow=TRUE)
y = daP[,6] #June precipitation 1948-2019
#install.packages('fitdistrplus')
library(fitdistrplus)
omahaPfit = fitdist(y, distr = "gamma", method = "mle")
#shape 1.51760921, rate  0.01889428
ks.test(y, "pgamma", shape = 1.51760921, rate= 0.01889428)
#D = 0.066249, p-value = 0.8893 #y is the Omaha data string
#You may verify the K-S statistic using another command
#install.packages('fitdistrplus')
library(fitdistrplus)
gofstat(omahaPfit) 
#Kolmogorov-Smirnov statistic  0.06624946 



#
#Plot Fig. 3.9: Sensitive to outliers and R code
setEPS() # save the .eps figure 
postscript("fig0309.eps", height = 5.6, width = 8)
setwd("/Users/sshen/climstats")
par(mar=c(4,4,0.5,0.5))
x = c(0.2*runif(50),1)
y = c(0.2*runif(50),1)
plot(x,y, pch=19, cex=0.5,
     cex.axis =1.6, cex.lab = 1.6)
dev.off()

#t-statistic
n=51
r = cor(x, y)
t=r*(sqrt(n-2))/sqrt(1-r^2)
t
#[1] 10.19999
qt(0.975, df=49)
#[1] 2.009575 #This is critical t value
1 - pt(10.19999, df=49)
#[1] 5.195844e-14 # a very small p-value

#
#
# R code for Kendall tau test
#install.packages("Kendall") 
library(Kendall)
x = c(0.2*runif(50),1)
y = c(0.2*runif(50),1)
Kendall(x,y) 
#tau = 0.114, 2-sided pvalue =0.24216

#
#
# R code for Mann-Kendall test: Edmonton data
setwd("/Users/sshen/climstats")
#Read Edmonton data from the gridded NOAAGlobalTemp
da1 =read.csv("data/EdmontonT.csv", header=TRUE)
x=da1[,3]
m1 = mean(x)
s1 = sd(x)
xa = (x- m1)/s1 #standardized anomalies
#install.packages('Kendall')
library(Kendall)
summary(MannKendall(xa))
#Score =  206254 , Var(Score) = 492349056
#tau = 0.153, 2-sided pvalue =< 2.22e-16


################################################################
#
# Chapter 4: Regression Models and Methods 
#
################################################################

#Plot Fig. 4.1: Colorado temperature lapse rate: R code
x= c(
1671.5, 1635.6, 2097.0, 1295.4, 1822.7, 2396.9, 2763.0, 1284.7,
1525.2, 1328.6, 1378.9, 2323.8, 2757.8, 1033.3, 1105.5, 1185.7,
2343.9, 1764.5, 1271.0, 2347.3, 2094.0, 2643.2, 1837.9, 1121.7)
y= c(
22.064, 23.591, 18.464, 23.995, 20.645, 17.175, 13.582, 24.635,
22.178, 24.002, 23.952, 16.613, 13.588, 25.645, 25.625, 25.828,
17.626, 22.433, 24.539, 17.364, 17.327, 15.413, 22.174, 24.549)
setEPS() # save the .eps figure 
postscript("fig0401.eps",  width = 8)
par(mar=c(4.5,4.5,2.5,0.5))
plot(x,y, 
     xlab="Elevation [m]",
     ylab=expression("Temperature ["~degree~"C]"),
  main="Colorado Elevation and July Tmean: 1981-2010 Average",
     cex.lab=1.5, cex.axis=1.5, cex.main =1.2)
reg=lm(y~x)
reg
#(Intercept)            x  
# 33.476216    -0.006982   #-7.0 degC/1km
summary(reg)
#R-squared:  0.9631
abline(reg,lwd=3)
text(2100, 25.5, 
expression("Temperature lapse rate: 7.0"~degree~"C/1.0km"), 
     cex=1.5)
text(2350, 24, "y = 33.48 - 0.0070 x", cex=1.5)
text(2350, 22.5,"R-squared = 0.96", cex=1.5)
dev.off()

#R code for the Colorado TLR regression analysis
lm(y ~ x)
#(Intercept)            x  
#33.476216    -0.006982 
reg = lm(y ~ x)
round(reg$residuals, digits = 5)
#       1        2        3        4        5        6 
#0.25792  1.53427 -0.37129 -0.43697 -0.10542  0.43358 
#7        8        9       10       11       12 
#-0.60335  0.12833 -0.64953 -0.19817  0.10302 -0.63880 
#13       14       15       16       17       18 
#-0.63366 -0.61692 -0.13283  0.63012  0.51454  1.27623 
#19       20       21       22       23       24 
#-0.06333  0.27628 -1.52923  0.39122  1.52971 -1.09572 

mean(reg$residuals)
#[1] 1.62043e-17

xa = x - mean(x)
sum(xa*reg$residuals)
#[1] -2.83773e-13

sum((reg$residuals)^2)/(length(y) -2)
#[1] 0.6096193


#
#Plot Fig. 4.2: R code
setEPS() 
postscript("fig0402.eps",  height = 5, width = 8)
par(mar=c(0.0,0.5,0.0,0.5))
plot(0,0, xlim=c(0,5.2), ylim=c(0,2.2),
     axes = FALSE, xlab="", ylab="")
arrows(0,0,4,0, angle=5, code=2, lwd=3, length=0.5)
arrows(4,0,4,2, angle=5, code=2, lwd=3, length=0.5)
arrows(0,0,4,2, angle=5, code=2, lwd=3, length=0.5)
arrows(5,0,4,2, angle=7, code=2, lwd=2, lty=3, length=0.5)
arrows(0,0,5,0, angle=7, code=2, lwd=2, lty=3, length=0.5)
arrows(3,0,4,2, angle=7, code=2, lwd=2, lty=3, length=0.5)
arrows(0,0,3,0, angle=7, code=2, lwd=2, lty=3, length=0.5)
segments(3.9,0, 3.9, 0.1)
segments(3.9, 0.1, 4.0, 0.1)
text(2,0.2, expression(hat(b)~bold(x)[a]), cex=2)
text(2,1.2, expression(bold(y)[a]), cex=2)
text(4.1,1, expression(bold(e)), cex=2)
text(3.8,0.6, expression(paste("Shortest ",bold(e))), 
     cex=1.5, srt=90)
text(3.4,1.1, expression(paste("Longer ",bold(e))), 
     cex=1.5, srt=71)
text(4.6,1.1, expression(paste("Longer ",bold(e))), 
     cex=1.5, srt=-71)
dev.off()

#
#
#R code for estimating regression slope b
#
#Method 1: Using vector projection
xa = x - mean(x)  #Anomaly the x data vector
nxa = sqrt(sum(xa^2)) #Norm of the anomaly data vector
ya = y - mean(y)
nya=sqrt(sum(ya^2))
sum(ya*(xa/nxa))/nxa #Compute b
#[1] -0.006981885  #This is an estimate for b

#Method 2:  Using correlation: R code
corxy=cor(xa, ya) #Compute the correlation between xa and ya
corxy
#[1] -0.9813858 #Very high correlation
corxy*nya/nxa #Compute b
#[1] -0.006981885 #This is an estimate for b

#
#R code for computing MV 
var(reg$fitted.values)
#[1] 15.22721 
#Or another way
yhat = reg$fitted.values
var(yhat)
#[1] 15.22721 
#Or still another way
n = 24
sum((yhat - mean(yhat))^2)/(n-1)
#[1] 15.22721 

#R code for computing YV 
sum((y - mean(y))^2)/(n-1)
# [1] 15.81033  
#Or another way
var(y)
#[1] 15.81033

#R code for computing R-squared value 
var(reg$fitted.values)/var(y)
#[1] 0.9631181  #This is the R-squared value

cor(x,y)
#[1] -0.9813858
(cor(x,y))^2
#[1] 0.9631181 #This is the R-squared value


#
#
qt(c(.025, .975), df=22)
#[1] -2.073873  2.073873

summary(reg)

-0.0069818 - 2.073873 * 0.0002913
#[1] -0.007585919
-0.0069818 + 2.073873 * 0.0002913
#[1] -0.006377681

33.4762157 - 2.073873*0.5460279
#[1] 32.34382
33.4762157 + 2.073873*0.5460279
#[1] 34.60861

(-0.0069818-(-0.0073))/0.0002913
#[1] 1.092345


#R plot Fig. 4.3: Confidence interval of a regression model
setwd("/Users/sshen/climstats")
#Confidence interval of the linear model
x1 = seq(max(x), min(x),len=100)
n = 24
xbar = mean(x)
reg = lm(y ~ x)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modTLR = 33.476216 + -0.006982*x1
xbar = mean(x)
Sxx = sum((x-xbar)^2)
CIupperModel= modTLR + qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIlowerModel= modTLR - qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIupperResponse= modTLR + qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)
CIlowerResponse= modTLR - qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)

setEPS() #Plot the figure and save the file
postscript("fig0403.eps", height = 8, width = 8)
par(mar=c(4.5,4.5,2.0,0.5))
plot(x,y, 
     ylim=c(10,30), xlim=c(1000,3000),
     xlab="Elevation [m]",
     ylab=bquote("Temperature ["~degree~"C]"),
     main="Colorado Elevation and July Tmean: 1981-2010 Average",
     cex.lab=1.5, cex.axis=1.5)
lines(x1,CIupperModel,type="l",col='red')
lines(x1,CIlowerModel,type="l",col='red')
lines(x1,CIupperResponse,type="l",col='blue')
lines(x1,CIlowerResponse,type="l",col='blue')
abline(reg,lwd=3)
text(2280, 26, 
bquote("Temperature lapse rate: 7.0"~degree~"C/km"), 
cex=1.5)
text(2350, 27.5,"R-squared = 0.96", cex=1.5)
text(2350, 29, "y= 33.48 - 0.0070 x", cex=1.5)
text(1600, 15,"Blue lines: CI of July Tmean RV", 
     col="blue", cex=1.5)
text(1600, 13.5,"Red lines: CI of the fitted model", 
     col="red", cex=1.5)
dev.off()


#
#Plot Fig. 4.4: R code
reg = lm(y~x)
setEPS() #Plot the figure and save the file
postscript("fig0404.eps", height = 4.5, width = 8)
par(mar=c(4.5,4.5,2.0,0.5))
plot(x, reg$residuals, pch=5,
     ylim=c(-2,2), xlim=c(1000,2800),
     xlab="Elevation [m]",
     ylab=bquote("Residual Temp ["~degree~"C]"),
     main="Residuals of the Colorado 1981-2010 July Tmean vs. Elevation",
     cex.lab=1.5, cex.axis=1.5, cex.main = 1.2)
dev.off()

#
#Plot Fig. 4.5: R code
reg = lm(y ~ x)
setEPS() #Plot the figure and save the file
postscript("fig0405.eps", height = 6, width = 6)
par(mar=c(4.5,4.5,2.0,0.5))
qqnorm(reg$residuals, pch=5,
       main="QQ-Normal Plot for the Colorado TLR Residuals",
       cex.lab = 1.4, cex.axis = 1.4)
qqline(reg$residuals, lty=2)
dev.off()

#
#R code for the DW-test for independence
#install.packages("lmtest")
library(lmtest)
ElevTemp=cbind(x,y, 1:24)
#Sort the data for ascending elevation
ElevTemp=ElevTemp[order(x),]
reg1=lm(ElevTemp[,2] ~ ElevTemp[,1])
dwtest(reg1)
#DW = 2.3072, p-value = 0.7062


#
#R code for the Mann-Kendall test
#install.packages("trend")
library(trend)
ElevTemp=cbind(x, y, 1:24)
#Sort the data for ascending elevation
ElevTemp=ElevTemp[order(x),]
reg1=lm(ElevTemp[,2] ~ ElevTemp[,1])
ElevTemp[,3]=reg1$residuals
mk.test(ElevTemp[,3])
#data:  ElevTemp[, 3]
#z = 0.47128, n = 24, p-value = 0.6374
mk.test(ElevTemp[,2])
#z = -5.9779, n = 24, p-value = 2.261e-09

#
#
lat=c(39.9919, 38.4600, 39.2203, 38.8236, 39.2425, 37.6742,
      39.6261, 38.4775, 40.6147, 40.2600, 39.1653, 38.5258,
      37.7717, 38.0494, 38.0936, 38.0636, 37.1742, 38.4858,
      38.0392, 38.0858, 40.4883, 37.9492, 37.1786, 40.0583)
lon=c(
-105.2667, -105.2256, -105.2783, -102.3486, -107.9631, -106.3247,
-106.0353, -102.7808, -105.1314, -103.8156, -108.7331, -106.9675,
-107.1097, -102.1236, -102.6306, -103.2153, -105.9392, -107.8792,
-103.6933, -106.1444, -106.8233, -107.8733, -104.4869, -102.2189)

#R code for the TLR multivariate linear regression
elev = x; temp = y #The x and y data were entered earlier
dat = cbind(lat, lon, elev, temp)
datdf = data.frame(dat)
datdf[1:2,] #Show the data of the first two stations
#      lat       lon   elev     temp
# 39.9919 -105.2667 1671.5 22.064
# 38.4600 -105.2256 1635.6 23.591

#Multivariate linear regression
reg=lm(temp ~ lat + lon + elev, data = datdf) 
summary(reg)  #Display the regression results 
#                  Estimate   Std. Error   t value  Pr(>|t|)    
#(Intercept) 36.4399561  9.4355746   3.862 0.000971 ***
#  lat         -0.4925051  0.1320096  -3.731 0.001319 ** 
#  lon         -0.1630799  0.0889159  -1.834 0.081564 .  
#  elev        -0.0075693  0.0003298 -22.953 7.67e-16 ***
#Residual standard error: 0.6176 on 20 degrees of freedom
#Multiple R-squared:  0.979

round(reg$coefficients, digits=5)
colnames(datdf) <- c('x1', 'x2', 'x3', 'y')
reg=lm(y ~ x1 + x2 + x3, data = datdf)
reg

#
# Plot Fig. 4.6 and make regression diagnostics: R code
setwd("/Users/sshen/climstats")
dtmean<-read.table(
  "data/aravg.ann.land_ocean.90S.90N.v5.0.0.201909.txt", 
  header=F)
dim(dtmean)
#[1] 140   6
x = dtmean[1:139,1]
y = dtmean[1:139,2]
reg =  lm(y ~ x) #linear regression
reg
#(Intercept)       yrtime  
#-14.574841     0.007348 

#Confidence interval of the linear model
xbar = mean(x)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x
xbar = mean(x)
Sxx = sum((x-xbar)^2)
n = length(y)
CIupperModel= modT + 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModel= modT - 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponse= modT + 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponse= modT - 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

CIupperModelr= modT + 
  qt(.975, df=5)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModelr= modT - 
  qt(.975, df=5)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponser= modT + 
  qt(.975, df=5)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponser= modT - 
  qt(.975, df=5)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

setEPS() #Plot the figure and save the file
postscript("fig0406.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x, y,  ylim = c(-1.5, 1),
     type="o", xaxt="n", yaxt="n",
     cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Temperature ["*degree*"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
axis(side = 2, at = c(-1.0, 0, 1.0), cex.axis = 1.4)
abline(reg, col="black", lwd=3)
lines(x,CIupperModel,type="l",col='red')
lines(x,CIlowerModel,type="l",col='red')
lines(x,CIupperResponse,type="l",col='blue')
lines(x,CIlowerResponse,type="l",col='blue')

lines(x,CIupperModelr,type="l", lty = 3, col='red')
lines(x,CIlowerModelr,type="l", lty = 3, col='red')
lines(x,CIupperResponser,type="l",lty = 3, col='blue')
lines(x,CIlowerResponser,type="l",lty = 3, col='blue')

text(1940, 0.5, 
     bquote("Linear trend: 0.7348"*degree*"C per century"), 
     col="black",cex=1.4)
text(1880, 0.9, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x, reg$residuals, ylim = c(-0.6,0.6),
     pch=5, cex.lab=1.4, cex.axis=1.4,
     yaxt = 'n', xlab="Year", 
     ylab=bquote("Residuals ["*degree*"C]"))
axis(side = 2, at = c(-0.3, 0, 0.3), cex.axis = 1.4)
text(1880, 0.5, "(b)", cex=1.4)
dev.off()

#
#
#Kolmogorov-Smirnov (KS) test for normality
library(fitdistrplus)
resi_mean = mean(reg$residuals)
resi_sd = sd(reg$residuals)
test_norm = rnorm(length(reg$residuals), 
                  mean = 0, sd = 1)
testvar = (reg$residuals - resi_mean)/resi_sd
ks.test(testvar, test_norm)
#D = 0.057554, p-value = 0.9754
#The normality assumption is accepted

#Diagnostics on independence and normality
# Durbin-Watson (DW) test for independence
dwtest(reg)
#DW = 0.45235, p-value < 2.2e-16
#The independence assumption is rejected

#degrees of freedom and critical t values
rho1 = acf(y)[[1]][2] #Auto-correlation function
rho1 #[1] 0.9270817
edof = (length(y) - 2)*(1 - rho1)/(1 + rho1)
edof #[1] 5.183904 effective degrees of freedom
qt(.975, df=137) #[1] 1.977431 critical t value
qt(.975, df=5) #[1] 2.570582 critical t value

#
#Plot Fig. 4.7: R code
#Polynomial fitting by multiple linear  regression
x1=x
x2=x1^2
x3=x1^3
dat3=data.frame(cbind(x1,x2,x3,y))
reg3 = lm(y ~ x1 + x2 + x3, data=dat3)
# simply use
# reg3 = lm(y ~ x + I(x^2) + I(x^3))
setEPS() #Plot the figure and save the file
postscript("fig0407.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x, y,  type="o", xaxt="n",
     cex.lab=1.4, cex.axis=1.4, xlab="Year", 
     ylab=bquote("Temperature ["~degree~"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
lines(x, predict(reg3), col="black", lwd=3)
reg3
#(Intercept)           x1           x2           x3  
#-1.426e+03    2.333e+00   -1.271e-03    2.308e-07  
text(1940, 0.3, 
     "The third order polynomial fit",
     col="black",cex=1.4)
text(1880, 0.58, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x1, reg3$residuals, 
     pch=5, cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Residuals ["~degree~"C]"))
text(1880, 0.32, "(b)", cex=1.4)
dev.off()


######Below is for an earlier Fig. 4.6 in the book draft 
# Plot Fig. 4.6 and make regression diagnostics: R code
setwd("/Users/sshen/climstats")
dtmean<-read.table(
  "data/aravg.ann.land_ocean.90S.90N.v5.0.0.201909.txt", 
                   header=F)
dim(dtmean)
#[1] 140   6
x = x1 = dtmean[1:139,1]
y = dtmean[1:139,2]
reg =  lm(y ~ x1) #linear regression
reg
#(Intercept)       yrtime  
#-14.574841     0.007348 

#Confidence interval of the linear model
xbar = mean(x1)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x1
xbar = mean(x)
Sxx = sum((x-xbar)^2)
n = length(y)
CIupperModel= modT + 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIlowerModel= modT - 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIupperResponse= modT + 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)
CIlowerResponse= modT - 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)

setEPS() #Plot the figure and save the file
postscript("fig0406.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x1, y,  ylim = c(-1.5, 1),
     type="o", xaxt="n", yaxt="n",
     cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Temperature ["~degree~"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
axis(side = 2, at = c(-1.0, 0, 1.0), cex.axis = 1.4)
abline(reg, col="black", lwd=3)
lines(x1,CIupperModel,type="l",col='red')
lines(x1,CIlowerModel,type="l",col='red')
lines(x1,CIupperResponse,type="l",col='blue')
lines(x1,CIlowerResponse,type="l",col='blue')
text(1940, 0.5, 
 bquote("Linear trend: 0.7348"~degree~"C per century"), 
     col="black",cex=1.4)
text(1880, 0.9, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x1, reg$residuals, ylim = c(-0.6,0.6),
     pch=5, cex.lab=1.4, cex.axis=1.4,
     yaxt = 'n', xlab="Year", 
     ylab=bquote("Residuals ["~degree~"C]"))
axis(side = 2, at = c(-0.3, 0, 0.3), cex.axis = 1.4)
text(1880, 0.5, "(b)", cex=1.4)
dev.off()

#Kolmogorov-Smirnov (KS) test for normality
ks.test(reg$residuals,test_norm)
#D = 0.086331, p-value = 0.6782
#The normality assumption is accepted

#Diagnostics on independence and normality
# Durbin-Watson (DW) test for independence
dwtest(reg)
#DW = 0.45235, p-value < 2.2e-16
#The independence assumption is rejected

rho1 = acf(y)[[1]][2] #Auto-correlation function
rho1 #[1] 0.9270817
edof = (length(y) - 2)*(1 - rho1)/(1 + rho1)
edof # [1] 5.183904
qt(.975, df=137) #[1] 1.977431
qt(.975, df=5) #[1] 2.570582


################## New Fig. 4.6

# Plot Fig. 4.6 and make regression diagnostics: R code
setwd("/Users/sshen/climstats")
dtmean<-read.table(
  "data/aravg.ann.land_ocean.90S.90N.v5.0.0.201909.txt", 
  header=F)
dim(dtmean)
#[1] 140   6
x = dtmean[1:139,1]
y = dtmean[1:139,2]
reg =  lm(y ~ x) #linear regression
reg
#(Intercept)       yrtime  
#-14.574841     0.007348 

#Confidence interval of the linear model
xbar = mean(x)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x
xbar = mean(x)
Sxx = sum((x-xbar)^2)
n = length(y)
CIupperModel= modT + 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModel= modT - 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponse= modT + 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponse= modT - 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

setEPS() #Plot the figure and save the file
postscript("fig0406.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x, y,  ylim = c(-1.5, 1),
     type="o", xaxt="n", yaxt="n",
     cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Temperature ["~degree~"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
axis(side = 2, at = c(-1.0, 0, 1.0), cex.axis = 1.4)
abline(reg, col="black", lwd=3)
lines(x,CIupperModel,type="l",col='red')
lines(x,CIlowerModel,type="l",col='red')
lines(x,CIupperResponse,type="l",col='blue')
lines(x,CIlowerResponse,type="l",col='blue')
text(1940, 0.5, 
     bquote("Linear trend: 0.7348"~degree~"C per century"), 
     col="black",cex=1.4)
text(1880, 0.9, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x, reg$residuals, ylim = c(-0.6,0.6),
     pch=5, cex.lab=1.4, cex.axis=1.4,
     yaxt = 'n', xlab="Year", 
     ylab=bquote("Residuals ["~degree~"C]"))
axis(side = 2, at = c(-0.3, 0, 0.3), cex.axis = 1.4)
text(1880, 0.5, "(b)", cex=1.4)
dev.off()

#Kolmogorov-Smirnov (KS) test for normality
ks.test(reg$residuals,test_norm)
#D = 0.086331, p-value = 0.6782
#The normality assumption is accepted

#Diagnostics on independence and normality
# Durbin-Watson (DW) test for independence
dwtest(reg)
#DW = 0.45235, p-value < 2.2e-16
#The independence assumption is rejected

rho1 = acf(y)[[1]][2] #Auto-correlation function
rho1 #[1] 0.9270817
edof = (length(y) - 2)*(1 - rho1)/(1 + rho1)
edof # [1] 5.183904
qt(.975, df=137) #[1] 1.977431
qt(.975, df=5) #[1] 2.570582

### New ### Fig. 4.6

# Plot Fig. 4.6 and make regression diagnostics: R code
setwd("/Users/sshen/climstats")
dtmean<-read.table(
  "data/aravg.ann.land_ocean.90S.90N.v5.0.0.201909.txt", 
  header=F)
dim(dtmean)
#[1] 140   6
x = dtmean[1:139,1]
y = dtmean[1:139,2]
reg =  lm(y ~ x) #linear regression
reg
#(Intercept)       yrtime  
#-14.574841     0.007348 

#Confidence interval of the linear model
xbar = mean(x)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x
xbar = mean(x)
Sxx = sum((x-xbar)^2)
n = length(y)
CIupperModel= modT + 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModel= modT - 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponse= modT + 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponse= modT - 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

CIupperModelr= modT + 
  qt(.975, df=5)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModelr= modT - 
  qt(.975, df=5)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponser= modT + 
  qt(.975, df=5)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponser= modT - 
  qt(.975, df=5)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

setEPS() #Plot figure Fig. 4.6 and save the file
postscript("fig0406.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x, y,  ylim = c(-1.5, 1),
     type="o", xaxt="n", yaxt="n",
     cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Temperature ["~degree~"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
axis(side = 2, at = c(-1.0, 0, 1.0), cex.axis = 1.4)
abline(reg, col="black", lwd=3)
lines(x,CIupperModel,type="l",col='red')
lines(x,CIlowerModel,type="l",col='red')
lines(x,CIupperResponse,type="l",col='blue')
lines(x,CIlowerResponse,type="l",col='blue')

lines(x,CIupperModelr,type="l", lty = 3, col='red')
lines(x,CIlowerModelr,type="l", lty = 3, col='red')
lines(x,CIupperResponser,type="l",lty = 3, col='blue')
lines(x,CIlowerResponser,type="l",lty = 3, col='blue')

text(1940, 0.5, 
     bquote("Linear trend: 0.7348"~degree~"C per century"), 
     col="black",cex=1.4)
text(1880, 0.9, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x, reg$residuals, ylim = c(-0.6,0.6),
     pch=5, cex.lab=1.4, cex.axis=1.4,
     yaxt = 'n', xlab="Year", 
     ylab=bquote("Residuals ["~degree~"C]"))
axis(side = 2, at = c(-0.3, 0, 0.3), cex.axis = 1.4)
text(1880, 0.5, "(b)", cex=1.4)
dev.off()

#Kolmogorov-Smirnov (KS) test for normality
ks.test(reg$residuals, test_norm)
#D = 0.086331, p-value = 0.6782
#The normality assumption is accepted

#Diagnostics on independence and normality
# Durbin-Watson (DW) test for independence
dwtest(reg)
#DW = 0.45235, p-value < 2.2e-16
#The independence assumption is rejected

rho1 = acf(y)[[1]][2] #Auto-correlation function
rho1 #[1] 0.9270817
edof = (length(y) - 2)*(1 - rho1)/(1 + rho1)
edof # [1] 5.183904
qt(.975, df=137) #[1] 1.977431
qt(.975, df=5) #[1] 2.570582


yautoc = acf(y) #Auto-correlation function
yautoc$acf[2]
#[1] 0.9270817

cor(y[1:138], y[2:139])
#[1] 0.9459649

length(y)
#[1] 139

#Confidence interval of the linear model
x1 = x
xbar = mean(x1)
reg8018 = lm(y ~ x1)
SSE = sum((reg8018$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x1
xbar = mean(x)
Sxx = sum((x-xbar)^2)
CIupperModel= modT + qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIlowerModel= modT - qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIupperResponse= modT + qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)
CIlowerResponse= modT - qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)

plot(x1, y, 
     xlab="Elevation [m]",
     ylab=bquote("Temperature ["~degree~"C]"),
     main="Colorado Elevation and July Tmean: 1981-2010 Average",
     cex.lab=1.5, cex.axis=1.5)
lines(x1,CIupperModel,type="l",col='red')
lines(x1,CIlowerModel,type="l",col='red')
lines(x1,CIupperResponse,type="l",col='blue')
lines(x1,CIlowerResponse,type="l",col='blue')


#Linear regression diagnostics: Check the assumptions
#Normality test by Q-Q plot
par(mfrow=c(1,1))
par(mar=c(4.5,4.5,2.5,0.7))
qqnorm(reg8018$residuals, pch=5,
       ylim=c(-0.4,0.4),xlim=c(-3,3),
       main="QQ-Normal Plot for the NOAAGlobalTemp: 1880-2018",
       cex.lab=1.4, cex.axis=1.4)
qqline(reg8018$residuals, lty=2, col = 'red')


#Kolmogorov-Smirnov (KS) test for normality
resi_sd=sd(reg8018$residuals)
resi_mean=mean(reg8018$residuals)
test_norm = rnorm(length(x1), mean = resi_mean,
                  sd=resi_sd)
ks.test(reg8018$residuals,test_norm)
#D = 0.086331, p-value = 0.6782
#Conclusion: Normal distribution is not rejected. 

#Check independence by Durbin-Watson (DW) test 
dwtest(reg8018)
#DW = 0.45235, p-value < 2.2e-16
#Conclusion: There is a significant serial correlation. 
 
yautoc = acf(y)
yautoc$acf[2]

cor(y,y)

#rm(list=ls()) #R forgets all the defined variables 


################################################################
#
# Chapter 5: Matrices for Climate Data
#
################################################################

#R code: Computational examples of matrices
A = matrix(c(1,0,0,4,3, 2), nrow = 3, byrow = TRUE)
B = matrix(c(0,1,-1,2), nrow = 2) #form a matrix by columns
C = A%*%B #matrix multiplication
C
#[1,]    0   -1
#[2,]    4    8
#[3,]    2    1
t(C) # transpose matrix of C
#[1,]    0    4    2
#[2,]   -1    8    1

A = matrix(c(1, -1, 1, 2), nrow =2, byrow = TRUE)
solve(A) #compute the inverse of A
#[1,]  0.6666667 0.3333333
#[2,] -0.3333333 0.3333333
A%*%solve(A) #verify the inverse of A
#[1,] 1.000000e+00    0
#[2,] 1.110223e-16    1

#Solve linear equations
A = matrix(c(30, 40, 1, 1), nrow =2, byrow = TRUE)
b = c(1000, 30)
solve(A,b)
#[1] 20 10
solve(A)%*%b #Another way to solve the equations
det(A) #compute the determinant
#[1] -10

library(Matrix)
rankMatrix(A) #Find the rank of a matrix
#[1] 2 #rank(A) = 2

#Orthogonal matrices
p = sqrt(2)/2
Q = matrix(c(p,-p,p,p), nrow=2) 
Q #is an orthogonal matrix 
#           [,1]      [,2]
#[1,]  0.7071068 0.7071068
#[2,] -0.7071068 0.7071068
Q%*%t(Q) #verify O as an orthogonal matrix
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
det(Q) #The determinant of an orthogonal matrix is 1 or -1
#[1] 1


#Eigenvectors and eigenvalues
A = matrix(c(1,2,2,1), nrow = 2)
eigen(A)
#$values
#[1]  3 -1
#$vectors
#[,1]       [,2]
#[1,] 0.7071068 -0.7071068
#[2,] 0.7071068  0.7071068


#
#
#Plot Fig. 5.2: R code
setwd('/Users/sshen/climstats')
setEPS() #Plot the figure and save the file
postscript("fig0502.eps", width = 6)
par(mar=c(4.5,4.5,2.0,0.5))
plot(9,9,
     main = 'An eigenvector vs a non-eigenvector',
     cex.axis = 1.4, cex.lab = 1.4,
     xlim = c(0,3), ylim=c(0,3),
     xlab = bquote(x[1]), ylab = bquote(x[2]))
arrows(0,0, 1,0, length = 0.25, 
       angle = 8, lwd = 5, col = 'blue')
arrows(0,0, 1,2, length = 0.3, 
       angle = 8, lwd = 2, col = 'blue',  lty = 3)
arrows(0,0, 1,1, length = 0.25, 
       angle = 8, lwd = 5, col='red') 
arrows(0,0, 3,3, length = 0.3, 
       angle = 8, lwd = 2, col='red', lty = 3)
text(1.4,0.1, 'Non-eigenvector u', cex =1.4, col = 'blue')
text(1.0,2.1, 'Au', cex =1.4, col = 'blue')
text(1.5,0.9, 'Eigenvector v', cex =1.4, col = 'red')
text(2.8, 2.95, 'Av', cex =1.4, col = 'red')
dev.off()

#
# Verify diagonalization and decomposition: R code
C = matrix(c(2,1,1,2), nrow = 2)
eigen(C)
#$values
#[1] 3 1
#$vectors
#  [,1]       [,2]
#[1,] 0.7071068 -0.7071068
#[2,] 0.7071068  0.7071068
Q = eigen(C)$vectors
D = t(Q)%*%C%*%Q #Matrix diagonalization
D
#[1,]    3    0
#[2,]    0    1
Q%*%D%*%t(Q) #Matrix decomposition
#[1,]    2    1
#[2,]    1    2
D[1,1]*Q[,1]%*%t(Q[,1]) + D[2,2]*Q[,2]%*%t(Q[,2])
#[1,]    2    1
#[2,]    1    2

#
#
#Plot Fig. 5.3: R code 
setwd('/Users/sshen/climstats')
setEPS() #Plot the figure and save the file
postscript("fig0503.eps", width = 11)
par(mar=c(0,0,0,0))
plot(200, axes = FALSE,
     xlab = "", ylab = "",
     xlim = c(-3,28), ylim = c(-3,16))
text(13,15.5, cex=2.0,
     bquote("SVD:" ~ A==UDV^t~ 
     "when n > m (top panel) or n < m (bottom panel)"))
#Space-time data matrix A when n>m
segments(x0 = c(0,0,3,3),
         y0 = c(6,12,12,6) +1,
         x1 = c(0,3,3,0),
         y1 = c(12,12,6,6) +1, 
         col = c('blue','red','blue','red'),lwd =3)
segments(x0 = c(0.5,1.0),
         y0 = c(6,6)+1,
         x1 = c(0.5,1.0),
         y1 = c(12,12)+1,
         lwd =1.3, lty = 3)
text(-.8, 9+1, 'n', srt=90, col ='blue', cex = 1.4)
text(1.5, 12.8+1, 'm', col = 'red',  cex = 1.4)
text(2.0, 9+1, '...',  cex = 1.4)
text(2, 5+1, bquote(A[n%*%m]),  cex = 2.5)
text(5, 9+1, '=',  cex = 3)
#Spatial matrix U
segments(x0 = c(7,7,10,10),
         y0 = c(6,12,12,6)+1,
         x1 = c(7,10,10,7),
         y1 = c(12,12,6,6)+1, 
         col = c('blue','blue','blue','blue'), lwd =3)
segments(x0 = c(7.5,8),
         y0 = c(6,6)+1,
         x1 = c(7.5,8),
         y1 = c(12,12)+1,
         lwd =1.3, lty = 3, col = 'blue')
text(6.2, 9+1, 'n', srt=90, col ='blue', cex = 1.4)
text(8.5, 12.8+1, 'm', col = 'red',  cex = 1.4)
text(9, 9+1, '...',  cex = 1.4, col='blue')
text(8.7, 5.0+1, bquote(U[n%*%m]),  cex = 2.5, col= 'blue')
#Singular value diagonal matrix D
segments(x0 = c(12,12,15,15),
         y0 = c(9,12,12,9)+1,
         x1 = c(12,15,15,12),
         y1 = c(12,12,9,9)+1, 
         col = c('brown','brown','brown','brown'), lwd =3)
segments(x0 = 12, y0 = 12+1, x1 = 15, y1 = 9+1, lty=3,
         col = c('brown'), lwd =1.3)#diagonal line
text(11.2, 10.5+1, 'm', srt=90, col ='red', cex = 1.4)
text(13.5, 12.8+1, 'm', col = 'red',  cex = 1.4)
text(14.1, 11.3+1, '0', col = 'brown',  cex = 1.4)
text(12.9, 10.0+1, '0', col = 'brown',  cex = 1.4)
text(13.9, 8.0+1, bquote(D[m%*%m]),  cex = 2.5, col='brown')
#Temporal matrix V
segments(x0 = c(17,17,20,20),
         y0 = c(9,12,12,9)+1,
         x1 = c(17,20,20,17),
         y1 = c(12,12,9,9)+1, 
         col = c('red','red','red','red'), lwd =3)
segments(x0 = c(17,17),
         y0 = c(11.5,10.8)+1,
         x1 = c(20,20),
         y1 = c(11.5,10.8)+1, 
         col = c('red','red'), lty=3, lwd =1.3)
text(16.2, 10.5+1, 'm', srt=90, col ='red', cex = 1.4)
text(18.5, 12.5+1, 'm', col = 'red',  cex = 1.4)
text(19.5, 8+1, bquote((V^t)[m%*%m]),  cex = 2.5, col='red')
text(18.5, 10+1, '...',  col='red', srt=90, cex =1.4)
# Space-time data matrix B when n < m
segments(x0 = c(0,0,6,6),
         y0 = c(0,3,3,0),
         x1 = c(0,6,6,0),
         y1 = c(3,3,0,0), 
         col = c('blue','red','blue','red'), lwd =3)
segments(x0 = c(1,2,5),
         y0 = c(0,0,0),
         x1 = c(1,2,5),
         y1 = c(3,3,3),
         lwd =1.3, lty = 3)
text(-0.8, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(3, 3.8, 'm', col = 'red',  cex = 1.4)
text(3.5, 1.5, '...',  cex = 1.4)
text(3, -1.5, bquote(A[n%*%m]),  cex = 2.5)
text(8, 1.5, '=',  cex = 3)
#Spatial matrix U
segments(x0 = c(11,11,14,14),
         y0 = c(0,3,3,0),
         x1 = c(11,14,14,11),
         y1 = c(3,3,0,0), 
         col = c('blue','blue','blue','blue'), lwd =3)
segments(x0 = c(11.5,12.2),
         y0 = c(0,0),
         x1 = c(11.5,12.2),
         y1 = c(3,3),
         lwd =1.3, lty = 3, col = 'blue')
text(10.2, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(12.5, 3.8, 'n', col = 'blue',  cex = 1.4)
text(13.2, 1.5, '...',  cex = 1.4, col='blue')
text(12.5, -1.5, bquote(U[n%*%n]),  cex = 2.5, col= 'blue')
#Singular value diagonal matrix D
segments(x0 = c(16,16,19,19),
         y0 = c(0,3,3,0),
         x1 = c(16,19,19,16),
         y1 = c(3,3,0,0), 
         col = c('brown','brown','brown','brown'), lwd =3)
segments(x0 = 16, y0 = 3, x1 = 19, y1 = 0, lty=3,
         col = c('brown'), lwd =1.3)#diagonal line
text(15.2, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(17.5, 3.8, 'n', col = 'blue',  cex = 1.4)
text(18.1, 2.3, '0', col = 'brown',  cex = 1.4)
text(16.9, 1.0, '0', col = 'brown',  cex = 1.4)
text(17.5, -1.5, bquote(D[n%*%n]),  cex = 2.5, col='brown')
#Temporal matrix V
segments(x0 = c(21,21,27,27),
         y0 = c(0,3,3,0),
         x1 = c(21,27,27,21),
         y1 = c(3,3,0,0), 
         col = c('red','red','red','red'),
         lwd =3)
segments(x0 = c(21,21),
         y0 = c(2.5,1.8),
         x1 = c(27,27),
         y1 = c(2.5,1.8), 
         col = c('red','red'), lty=3, lwd =1.3)
text(20.2, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(24, 3.8, 'm', col = 'red',  cex = 1.4)
text(24, -1.5, bquote((V^t)[n%*%m]),  cex = 2.5, col='red')
text(24, 1, '...',  col='red', srt=90, cex =1.4)
dev.off()


#
#SVD example for a 2-by-3 matrix: R code
A=matrix(c(-1,1,0,2,-2,3),nrow=2)
A #Show the 2-by-3 matrix
#     [,1] [,2] [,3]
#[1,]   -1    0   -2
#[2,]    1    2    3
svdA=svd(A) #Compute the SVD of A and put the results in svdA
svdA #Show SVD results: d, U, and V
round(svdA$d, digits=2) #Show only the singular values
#[1] 4.22 1.09
round(svdA$u, digits=2) #Show only matrix U
#      [,1] [,2]
#[1,] -0.48 0.88
#[2,]  0.88 0.48
round(svdA$v, digits=2)#Show only matrix V
#     [,1]  [,2]
#[1,] 0.32 -0.37
#[2,] 0.42  0.88
#[3,] 0.85 -0.29
sqrt(eigen(A%*%t(A))$values)
#[1] 4.221571 1.085514

#Data reconstruction by singular vectors: R code
round(svdA$d[1]*svdA$u[,1]%*%t(svdA$v[,1]), 
      digits=1)
#     [,1] [,2] [,3]
#[1,] -0.7 -0.8 -1.7
#[2,]  1.2  1.5  3.2

round(svdA$d[1]*svdA$u[,1]%*%t(svdA$v[,1]) + 
        svdA$d[2]*svdA$u[,2]%*%t(svdA$v[,2]), 
      digits =2)
#     [,1] [,2] [,3]
#[1,]   -1    0   -2
#[2,]    1    2    3

#
#
#Weighted SOI from standardized SLP anomalies: R code
setwd("/Users/sshen/climmath")
Pda<-read.table("data/PSTANDdarwin.txt", header=F)
dim(Pda) 
#[1] 65 13 #Monthly Darwin data from 1951-2015
pdaDec<-Pda[,13] #Darwin Dec standardized SLP anomalies data
Pta<-read.table("data/PSTANDtahiti.txt", header=F)
ptaDec=Pta[,13] #Tahiti Dec standardized SLP anomalies
ptada1 = cbind(pdaDec, ptaDec) #space-time data matrix

#Space-time data format
ptada = t(ptada1[59:65,]) #2009-2015 data
colnames(ptada)<-2009:2015
rownames(ptada)<-c("Darwin", "Tahiti")
ptada #6 year of data for two stations
#       2009 2010 2011 2012 2013 2014 2015
#Darwin  0.5 -2.3 -2.2  0.3  0.3  0.1 -0.4
#Tahiti -0.7  2.5  1.9 -0.7  0.4 -0.8 -1.3
svdptd = svd(ptada) #SVD for the 2-by-6 matrix
U=round(svdptd$u, digits=1)
U
#[1,] -0.7  0.8
#[2,]  0.8  0.7
D=round(diag(svdptd$d), digits=1)
D
#[1,]  4.7  0.0
#[2,]  0.0  1.4
V =round(svdptd$v, digits=1)
t(V)
#[1,] -0.2  0.7  0.6 -0.2  0.0 -0.1 -0.2
#[2,] -0.1 -0.1 -0.3 -0.2  0.3 -0.3 -0.8

################################################################
#
# Chapter 6: Covariance Matrices, EOFs, and PCs
#
################################################################

#
#Plot Fig. 6.1a, b, c: and Covariance map by R code 
#Covariance from NOAAGlaobalTemp data: December SAT anomalies  
setwd('/Users/sshen/climstats') 
library(ncdf4)
nc=ncdf4::nc_open("data/air.mon.anom.nc") #Read data
nc #Check metadata of the dataset
nc$dim$lon$vals # 2.5 - 357.5
nc$dim$lat$vals # -87.5 - 87.5
nc$dim$time$vals
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time<- ncvar_get(nc, "time")
library(chron)
month.day.year(29219,c(month = 1, day = 1, year = 1800))
#1880-01-01
sat<- ncvar_get(nc, "air")
dim(sat)
#[1] 72   36 1674 
#1674 months=1880-01 to 2019-06, 139 years 6 mons

Dec = seq(12, 1674, by=12)
Decsat=sat[,, Dec]
N = 72*36
P = length(Dec)
STsat = matrix(0, nrow=N, ncol=P)
for (k in 1:P){STsat[,k]=as.vector(Decsat[,,k])}
colnames(STsat)<-1880:2018
STsat[1:4,1:4]
LAT=rep(lat, each=72)
LON=rep(lon,36)
STanom=cbind(LAT, LON, STsat)
dim(STanom)
#[1] 2592  141

#Plot Fig. 6.1a: Zonal covariance matrix 
#Select only the data for the equatorial band -2.5S
n1<-which(STanom[,1]>-4&STanom[,1]<0
          &STanom[,2]>0&STanom[,2]<360)
dim(STanom)
#[1] 2592  141
length(n1)
#[1] 72 longitude grid boxes at -2.5S
P1=84
P2=133
P= P2 - P1 + 1
dat1=STanom[n1,P1:P2] #1961-2010
dim(dat1)
#[1] 72 50
#72 grid boxes, 50 years of Dec from 1961-2010. 
dat1[1:3,48:50]
Lat1=STanom[n1,1]
Lon1=STanom[n1,2]
AreaFac = sqrt(cos(Lat1*pi/180))
dat2 = dat1 - rowMeans(dat1) #Minus the mean
Banddat = AreaFac*dat2
covBand = Banddat%*%t(Banddat)
max(covBand)
#[1] 90.67199
min(covBand)
#[1] -6.191734
int=seq(-7, 92,length.out=81)
rgb.palette=colorRampPalette(c('blue', 'darkgreen',
 'green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
setEPS() #Plot the figure and save the file
postscript("fig0601a.eps", width = 7, height = 5.5)
par(mar=c(4.2,5.0,1.8,0.0))
par(cex.axis=1.8,cex.lab=1.8, cex.main=1.7)
ticks = c(60,  180, 300)
filled.contour(Lon1, Lon1, covBand, 
               color.palette=rgb.palette, levels=int,
               plot.title=title(main=
 expression("Zonal SAT Covariance: 2.5"*degree*S),
    xlab="Longitude", ylab="Longitude"),
   plot.axes = { axis(1, at= ticks); axis(2, at= ticks, las = 0)},
            key.title=title(main=expression("["*degree*"C]"^2))
      )
dev.off()

###variance as the diagonal covariance
varzonal = diag(covBand)/(AreaFac)^2
plot(Lon1, varzonal, type = 'h', lwd =2)

#Plot Fig. 6.1b: Meridional covariance matrix
#Select only the data for the meridional band 2.5E
n1<-which(STanom[,1]>-90&STanom[,1]<90
          &STanom[,2]>0&STanom[,2]<4)
P1=84 #time index for Dec 1961
P2=133 #Dec 2010
P= P2 - P1 + 1 #=50 Decembers
dat1=STanom[n1,P1:P2] #1961-2010
Lat1=STanom[n1,1]
Lon1=STanom[n1,2]
AreaFac = sqrt(cos(Lat1*pi/180)) 
dat2 = dat1 - rowMeans(dat1)
Banddat = AreaFac*dat2
covBand = Banddat%*%t(Banddat)
max(covBand, na.rm=TRUE)
#[1] 115.6266
min(covBand, na.rm=TRUE)
#[1] -33.72083
int=seq(-35,120,length.out=81)
rgb.palette=colorRampPalette(c('blue', 'darkgreen','green', 
                               'yellow','pink','red','maroon'),
                             interpolate='spline')
ticks = seq(-90, 90, by=30)
setEPS() #Plot the figure and save the file
postscript("fig0601b.eps", width = 7, height = 5.5)
par(mar=c(4.2,5.0,1.8,0.0))
par(cex.axis=1.8, cex.lab=1.8, cex.main=1.7)
filled.contour(Lat1, Lat1, covBand, 
               color.palette=rgb.palette, levels=int,
               plot.title=title(main=
      expression("SAT Meridional Covariance: 2.5"*degree*E),
                    xlab="Latitude", ylab="Latitude"),
plot.axes={axis(1, at=ticks); axis(2, at=ticks, las =0); 
           grid()},
      key.title=title(main=expression("["*degree*"C]"^2)))     
dev.off()

#plot Fig. 6.1(c)
month.day.year(time[1416],c(month = 1, day = 1, year = 1800))
#Dec 1997
mapmat= sat[,,1416]
mapmat=pmax(pmin(mapmat,5),-5)
int=seq(-5, 5,length.out=51)
rgb.palette=colorRampPalette(c('black','blue',
  'darkgreen','green', 'white','yellow','pink',
  'red','maroon'), interpolate='spline')
setEPS() #Plot the figure and save the file
postscript("fig0601c.eps", width = 7, height = 3.5)
par(mar=c(4.2,5.0,1.8,0.0))
par(cex.axis=0.9,cex.lab=0.9, cex.main=0.8)
library(maps)
filled.contour(lon, lat, mapmat, 
               color.palette=rgb.palette, levels=int,
          plot.title=title(main="December 1997 SAT Anomalies",
                          xlab="Longitude",ylab="Latitude"),
               plot.axes={axis(1); axis(2); 
                 map('world2', add=TRUE);grid()},
               key.title=title(main=expression(degree*"C")))
segments(x0=-20,y0=-2.5, x1=255, y1=-2.5, lwd = 5, lty = 2)
segments(x0=-15,y0=-90, x1=-15, y1=90, lwd = 5, lty = 2)
dev.off()

#
#
#Zonal covariance matrix 
#Select only the data for the equatorial band -2.5S
n1<-which(STanom[,1]>-4&STanom[,1]<0
          &STanom[,2]>0&STanom[,2]<360)
dim(STanom)
#[1] 2592  142
length(n1)
#[1] 72 longitude grid boxes at -2.5S
P1=84
P2=133
P= P2 - P1 + 1
dat1=STanom[n1,P1:P2] #1961-2010
dim(dat1)
#[1] 72 50
#72 grid boxes, 50 years of Dec from 1961-2010. 
dat1[1:3,47:50]
Lat1=STanom[n1,1]
Lon1=STanom[n1,2]
AreaFac = sqrt(cos(Lat1*pi/180))
dat2 = dat1 - rowMeans(dat1) #Minus the mean
Banddat = AreaFac*dat2
covBand = Banddat%*%t(Banddat)

#R plot 6.2: Scree plot 
K = 10
eigCov =eigen(covBand) 
#covBand is for equatorial zonal band in Fig 6.1(a)
lam = eigCov$values
lamK=lam[1:K]
setEPS() #Plot the figure and save the file
postscript("fig0602.eps", width = 6, height = 4)
par(mar=c(4,4,2,4), mgp=c(2.2,0.7,0))
plot(1:K, 100*lamK/sum(lam), ylim=c(0,80), type="o", 
     ylab="Percentage of Variance [%]",
     xlab="EOF Mode Number", 
     cex.lab=1.2, cex.axis = 1.1, lwd=2, 
     main="Scree Plot of the First 10 Eigenvalues")
legend(3,30, col=c("black"),lty=1, lwd=2.0,
       legend=c("Percentange Variance"),bty="n",
       text.font=2,cex=1.0, text.col="black")
par(new=TRUE)
plot(1:K,cumsum(100*lamK/sum(lam)),
     ylim = c(60,100), type="o",
     col="blue",lwd=2, axes=FALSE,
     xlab="",ylab="")
legend(3,80, col=c("blue"),lty=1,lwd=2.0,
       legend=c("Cumulative Percentage Variance"),bty="n",
       text.font=2,cex=1.0, text.col="blue")
axis(4, col="blue", col.axis="blue", mgp=c(3,0.7,0))
mtext("Cumulative Variance [%]",col="blue", 
      cex=1.2, side=4,line=2)
dev.off()

#
#
#Generate a random space-time field 
#Step 1: Generate EOFs
N <- 100 #Number of spatial points
eof  <-  function(n, x) (sin(n*x)/sqrt(pi/2))*sqrt((pi/N))
x  <-  seq(0,pi, len=N)
sum(eof(4,x)^2)
#[1] 0.99  #Verify the normalization condition
sum(eof(1,x)*eof(2,x))
#[1] 3.035766e-18  #Verify orthogonality 

#Step 2: Generate PCs 
#install.packages('mvtnorm')
library(mvtnorm) #Multivariate normal
Mode <- 5
Ms <- 1000
univar <- diag(rep(1,Mode))
zeromean <- rep(0, Mode)
rs <- rmvnorm(Ms, zeromean, univar)
pcm=matrix(0, nrow=Ms, ncol=Mode)
for(m in 1:Mode){pcm[,m] = rs[,m]*sqrt(1/Ms)}
t(pcm[,1])%*%pcm[,1]
#1.010333 #Approximately normalized
t(pcm[,1])%*%pcm[,2]
#0.008040772 #Approximately independent/orthorgonal

#Step 3: Generate an independent random field
lam=c(10, 9, 4.1, 4, 2)
sqrlam = diag(sqrt(lam))
eofm = matrix(0, nrow=N, ncol=Mode)
for(m in 1:Mode){eofm[,m]=eof(m,x)}
Yxr  <-  eofm%*%sqrlam%*%t(pcm)
dim(Yxr)
#[1] 100 1000

#
#
#Durbin-Watson (DW) test for no serial correlation
library(lmtest)
Ms = 1000
r = 1:Ms
regYxr = lm(Yxr[10,] ~ r)
dwtest(regYxr)
#DW = 2.0344, p-value = 0.696
#Implying no significant serial correlation

#
#
#Plot Fig. 6.3: R code
#The first 100 samples of the generated field
r=1:100
setEPS()
postscript("fig0603.eps", width = 6, height = 4)
par(mar=c(3.5,3.5,2,0), mgp=c(2.0,0.7,0))
rgb.palette=colorRampPalette(
  c('black','blue','green', 
    'yellow','pink','red','maroon'),
  interpolate='spline')
filled.contour(r,x, t(Yxr[,1:100]), 
         color=rgb.palette, xlab="r", ylab="x", cex.lab=1.2,
          main="A random field realization from given EOFs",
          plot.axes={axis(1, cex.axis =1.1); 
            axis(2, las = 0, cex.axis= 1.2); grid()},
          key.title={par(cex.main=0.9); title(main="Value")}
)
dev.off()

#
#
#Scree plot
Mode = 1:5
lam=c(10, 9, 4.1, 4, 2)
samp = rep("S100", 5)
sd = sqrt(2/100)*lam
sd2 = sqrt(2/1000)*lam
par(mar=c(4.5, 4.5, 1, 0.5))
plot(Mode, lam, type="l", ylim = c(0,12),
     xlab="Mode", 
     ylab=expression("Variance   "*lambda),
     cex.lab=1.4, cex.axis=1.4)
points(Mode,lam + sd, pch="-", col="red", cex=2)
points(Mode,lam - sd, pch="-",col="red", cex=2)
segments(Mode,lam + sd, Mode,lam - sd, col="red")
points(Mode+0.06,lam + sd2, pch="-", col="blue", cex=2)
points(Mode+0.06,lam - sd2, pch="-",col="blue", cex=2)
segments(Mode +0.06,lam + sd2, 
         Mode + 0.06,lam - sd2, col="blue")
text(3.5,12, 
     "Red standard error bar: 100 samples", col="red")
text(3.5,11, 
     "Blue standard error bar: 1000 samples", col="blue")

#
#
#Plot Fig. 6.4: Scree plot by R code
Mode = 1:5
lam=c(10.0, 9.0, 4.1, 4.0, 2.0)
samp = rep("S100", 5)
sd = sqrt(2/100)*lam
sd2 = sqrt(2/1000)*lam
#par(mar=c(4.5, 4.5, 1, 0.5))
setEPS()
postscript("fig0604.eps", width = 10, height = 8)
par(mar=c(3.5, 4.5, 0.2, 0.5), mgp=c(2.5, 1.0,0))
plot(Mode, lam, type="l", ylim = c(0,12),
     xlab="Mode", 
     ylab=expression("Variance   "*lambda),
     cex.lab=1.8, cex.axis=1.8)
points(Mode,lam + sd, pch="-", col="red", cex=2)
points(Mode,lam - sd, pch="-",col="red", cex=2)
segments(Mode,lam + sd, Mode,lam - sd, col="red")
points(Mode+0.06,lam + sd2, pch="-", col="blue", cex=2)
points(Mode+0.06,lam - sd2, pch="-",col="blue", cex=2)
segments(Mode +0.06,lam + sd2, Mode + 0.06,lam - sd2, 
         col="blue")
text(3.45,12, "Red standard error bar: 100 samples", 
         cex = 1.5, col="red")
text(3.5,11, "Blue standard error bar: 1000 samples", 
     cex = 1.5, col="blue")
dev.off()
#
#
#Plot Fig. 6.5: 1D EOF errors by R code
#Ms= 100 samples for North's rule of thumb
#install.packages("mvtnorm")
library(mvtnorm)
set.seed(112)
M=100 #M samples or M independent time steps
N=100 #N spatial locations
Mode=5 # 5 EOF modes to be considered

#Generate PCs
lam=c(10.0, 9.0, 4.1, 4.0, 2.0)
round(sqrt(2/M)*lam, digits = 2)
#[1] 1.41 1.27 0.58 0.57 0.28
univar =diag(rep(1,Mode)) #SD = 1
zeromean = rep(0, Mode) #mean = 0
rs <- rmvnorm(M, zeromean, univar) 
dim(rs) # 100 samples and 5 modes
#[1] 100    5
mean(rs[,1])
#[1] -0.02524037
var(rs[,1])
#[1] 0.9108917
t <- seq(0, 2*pi, len=M)
a51 <-rs[,1]*sqrt(1/M)
sum(a51^2)
#[1] 0.9026492 is the variance approximation

pcm=matrix(0, nrow=M, ncol=Mode)
for(m in 1:Mode){pcm[,m] = rs[,m]*sqrt(1/M)} 
dim(pcm) #random and independent PCs
#[1] 100   5
sum(pcm[,1]^2) #verify the normality of a PC
#[1] 1.021924

#Generate EOFs for spatial patterns
eof <- function(n, x) sqrt(2)* sin(n*x)*sqrt((1/N))
x <- seq(0, pi,len=N) #N locations within [0,1]
sum(eof(3,x)^2) #verify the normality of an EOF
#[1] 0.99
sum(eof(1,x)*eof(2,x)) #verify the EOF orthogonality
#[1] 3.035766e-18

eofm <- matrix(0, nrow=N, ncol=Mode)
for (m in 1:Mode){eofm[,m]=eof(m,x)}
dim(eofm) #eofm are the 5 modes of EOF data
#[1] 100   5 #100 spatial locations and 5 modes

#Generate the random data with given EOFs
Lam = diag(sqrt(lam)) #eigenvalue matrix
da <- eofm%*%Lam%*%t(pcm) #spectral decomposition
dim(da) #random data at 100 spatial locations
#[1]  100 100
svdda <- svd(da)
round((svdda$d[1:5])^2, digits =2)
#[1] 10.14  8.12  3.83  3.26  2.10

png(file="fig0605a.png", width=600,height=300)
k=1
sum(svdda$u[,k]^2)
#[1] 1
par(mar=c(4.5,4.7,2,0.2))
plot(x,-svdda$u[,k], type="l", ylim=c(-0.2,0.2),
     xlab="x", ylab=paste("EOF", k), 
     main="EOF Mode (Blue Curve) vs Exact Mode (Red Curve)",
     col="blue",
     cex.lab=1.4, cex.axis=1.4,cex.main=1.4)
lines(x, eof(k,x), col='red')
dev.off()
#
#
#
#Plot Fig. 6.6: R code for the EOF4 error 
k = 4 #Use the SVD result from 100 samples R = 100
plot(x,svdda$u[,k], type="l", ylim=c(-0.33,0.33),
     xlab="x", ylab="EOFs [dimensionless]", 
     main="EOF4 error (black) vs Exact EOF3 (Orange)",
     col="blue",
     cex.lab=1.4, cex.axis=1.4, cex.main=1.4)
legend(0.2,0.37, bty = "n", cex=1.4, text.col = 'blue',
       lwd=1.2,legend="Sample EOF4",col="blue")
lines(x, eof(k,x), col='red')
legend(0.2,0.41, bty = "n", cex=1.4, text.col = 'red',
       lwd=1.2,legend="True EOF4",col="red")
lines(x,svdda$u[,k] - eof(k,x), lwd=2, col='black'  )
legend(0.2,0.33, bty = "n", cex=1.4, text.col = 'black',
       lwd=2,legend="Sample EOF4 - True EOF4",col="black")
lines(x, -eof(3,x), col='orange')
legend(0.2,0.29, bty = "n", cex=1.4, text.col = 'orange',
       lwd=1.2,legend="True EOF3",col="orange")

#
#
#Plot Fig. 6.7: R code for 2D sample EOFs
library(mvtnorm)
dmvnorm(x=c(0,0))
M=100
N=100
Mode=5

#Generate PCs
lam=c(10.0, 9.0, 4.1, 4.0, 2.0)
univar =diag(rep(1,Mode))
zeromean = rep(0, Mode)
rs <- rmvnorm(M, zeromean, univar)
dim(rs)
#[1] 100    5
t <- seq(0, len=M)
a51 <-rs[,1]*sqrt(1/M)
pcm=matrix(0, nrow=M, ncol=Mode)
for(m in 1:Mode){pcm[,m] = rs[,m]*sqrt(1/M)} 
dim(pcm)
#[1] 100   5

#Generate true EOFs
eof <- function(n, x, y){(outer(sin(n*x),sin(n*y)))*(2/N)}
#eof <- function(n, x, y){outer(sin(n*x),sin(n*y))}
x = y <- seq(0,pi,len=100)
sum((eof(2,x, y))^2) #verify the normality
#[1] 0.9801

#Plot true EOF1 2D
png(file="fig0607a.png", width=200,height=200)
par(mar=c(2,2,0.5,0.5))
contour(x,y, eof(1,x,y))
text(1.5,1.5, 'EOF1', cex=2)
dev.off()

eofm <- matrix(0, nrow=N^2, ncol=Mode)
for (m in 1:Mode){eofm[,m]=c(eof(n=m,x,y))}
dim(eofm)
#[1] 10000   5 #10000 = 100*100
t(eofm)%*%eofm 
#approximately equal to I5 an identify matrix

#Generate the random data with given EOFs
Lam = diag(sqrt(lam))
da <- eofm%*%Lam%*%t(pcm)
dim(da)
#[1] 10000   100
svdda <- svd(da)

#Plot ssample EOF1-5 2D: R=100
png(file="fig0607n.png", width=200,height=200)
par(mar=c(2,2,0.5,0.5))
k=5
contour(x,y, matrix(svdda$u[,k], ncol=100), 
        cex.lab=1.4, cex.main=1.4)
text(1.5,1.5, 'EOF5', cex=2)
dev.off()

################################################################
#
# Chapter 7: Introduction to Time Series
#
################################################################

#R code for Fig. 7.1: Mauna Loa CO2 March 1958-July 2020
setwd("/Users/sshen/climstats")
co2m = read.table("data/co2m.txt", header = TRUE)
dim(co2m)
# [1] 749   7
co2m[1:3,]
#year mon     date average interpolated  trend days
#1 1958   3 1958.208  315.71       315.71 314.62   -1
#2 1958   4 1958.292  317.45       317.45 315.29   -1
#3 1958   5 1958.375  317.50       317.50 314.71   -1
mon = co2m[,3]
co2 = co2m[,5]
setEPS() # save the .eps figure file to the working directory
postscript("fig0701.eps", height = 8, width = 10)
par(mar=c(2.2,4.5,2,0.5))
plot(mon, co2, type="l", col="red",
     main = 
       expression(paste("Monthly Atmospheric ", 
                        CO[2]," at Mauna Loa Observatory")),
     xlab ="", 
     ylab="Parts Per Million [ppm]",
     cex.axis =1.5, cex.lab=1.5, cex.main= 1.6)
text(1975, 410, "Scripps Institution of Oceanography", 
     cex=1.5)
text(1975, 400, "NOAA Global Monitoring Laboratory", 
     cex=1.5)
lines(mon, co2m[,6]) #plot the trend data
dev.off()


#
#
#
#R Plot Fig. 7.2: Time series decomposition 
co2.ts = ts(co2, start=c(1958,3), end=c(2020,7), 
            frequency =12)
co2.ts #Display time series with month and year
#        Jan    Feb    Mar    Apr    May    Jun    Jul    Aug    
#1958               315.71 317.45 317.50 317.10 315.86 314.93 
#1959 315.62 316.38 316.71 317.72 318.29 318.15 316.54 314.80 
#1960 316.43 316.97 317.58 319.02 320.03 319.59 318.18 315.91 

#Decompose a time series into components of trend, 
#seasonal cycle, and random residual
co2.decompose <- decompose(co2.ts)
#Plot the time series and its three components 
plot(co2.decompose, xlab ="")
#Change the title of the plot
my_plot.decomposed.ts = function(x, title="", ...) {
  xx <- x$x
  if (is.null(xx)) 
    xx <- with(x, if (type == "additive") 
      random + trend + seasonal
      else random * trend * seasonal)
  plot(cbind(observed = xx, trend = x$trend, 
             seasonal = x$seasonal, random = x$random), 
       main=title, ...)
}
par(mar=c(2,4.5,2.5,0.2))
my_plot.decomposed.ts(co2.decompose, 
                      expression(paste("Monthly Atmospheric ", 
                                       CO[2]," [ppm] at Mauna Loa Observatory")),
                      xlab ="", cex.lab=1.5, cex.axis=1.8)

plot(co2.decompose$trend, type = 'l')
lines(mon, co2m[,6], col = 'red')

plot(co2.decompose$trend - co2m[,6], type = 'l')

#Trend component
summary(co2.decompose$trend, na.rm=TRUE)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#315.4   330.1   352.9   355.2   377.9   412.8      12 
plot(co2.decompose$trend, type = 'l')

#Seasonal component
round(summary(co2.decompose$seasonal, na.rm=TRUE), 
      digits = 3)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-3.256  -1.484   0.678   0.013   2.318   3.031 
plot(co2.decompose$seasonal, type = 'l')
plot(co2.decompose$seasonal[1:50], type = 'l')

#Random component  
round(summary(co2.decompose$random, na.rm=TRUE), 
      digits = 4)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-0.9628 -0.1817  0.0034 -0.0009  0.1818  1.2659      12 
which (co2.decompose$random > 1.0)
#[1] 186 698
co2m[c(186, 698),]
#year mon     date average interpolated  trend days
#186 1973   8 1973.625  329.31       329.31 330.42   -1
#698 2016   4 2016.292  407.45       407.45 404.60   25
sd(co2.decompose$random, na.rm=TRUE)
#[1] 0.2953659
plot(co2.decompose$random, type = 'l')

###
min(co2.decompose$trend - co2m[,6], na.rm=TRUE)
###older version to be deleted
setwd("/Users/sshen/climstats")
co2m = read.table("data/co2m.txt", header = TRUE)
dim(co2m)
# [1] 749   7
co2m[1:3,]
#year mon     date average interpolated  trend days
#1 1958   3 1958.208  315.71       315.71 314.62   -1
#2 1958   4 1958.292  317.45       317.45 315.29   -1
#3 1958   5 1958.375  317.50       317.50 314.71   -1
mon = co2m[,3]
co2 = co2m[,5]
##### test
co2_12 = c(1:2, co2, 8:12)
length(co2_12)/12
#[1] 63
mon[35: (35 + 12*50 -1)] #Jan 1961-Dec 2010
co2mat = matrix(co2_12, ncol =12, byrow = TRUE)
clim = matrix(0, nrow =length(mon), ncol = 12)
clim = colMeans(co2mat)
plot(clim)
co2trend = 1:length(mon)
for (i in 1: length(mon)){
  co2trend[i] = co2[i]-clim[co2m[i,2]] + mean(clim)
}
plot(co2trend, type = 'l')
lines(co2m[,6], type = 'l', col ='red')
plot(co2trend - co2m[,6], type = 'l')
plot(co2m[,6], type = 'l')
####

#Plot the nonlinear trend line
#install.packages('forecast')
library(forecast)
co2.ts = ts(co2, start=c(1958,3), end=c(2020,7), 
            frequency =12)
co2.decompose <- decompose(co2.ts)
plot(co2.decompose$trend,  
      ylab = 'CO2 [ppm]', xlab = 'Time [mon]')
#
#
#R plot Fig. 7.3: Time series forecast for CO2
#by ETS(Error, Trend, and Seasonal) smoothing
library(forecast)
co2forecasts <- ets(co2.ts,model="AAA")
#ETS model AAA means exponential smoothing with 
#season and trend
#Forecast 48 months to the future
co2forecasts <- forecast(co2forecasts, h=48)
plot(co2forecasts,
     ylab="Parts per million [ppm]",
     main = "Observed (red) and ETS Forecast (blue)",
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.2,
     lwd = 2)
lines(co2.ts, col='red')
#
#
#
#R plot Fig. 7.4: Daily Tmin at ST PAUL Station, Minnesota 
#1941-1950 MINNEAPOLIS/ST PAUL
#GHCND:USW00014927  44.8831N  93.2289W  265.8Elev  
setwd("/Users/sshen/climstats")
tda = read.table("data/StPaulStn.txt", header = F)
dim(tda)
#[1] 7589   13
#Col12 = Tmin, col9 = PRCP, col8 = Date
t1 = tda[,8] #date
tmin = tda[, 12] #Tmin data
t1[1462]
#[1] 19410101
t1[5112]
#[1] 19501231
da = tmin[1462:5112]
ta = t1[1462:5112]
dav = as.vector(t(da))
Tmin.ts = ts(dav, start=c(1941,1,1), 
             end=c(1950,12,31), 
             frequency =365)
#ETS time series decomposition
Tmin.decompose <- decompose(Tmin.ts)
plot(Tmin.decompose)
#Change the title of the ETS decomposition figure
my_plot.decomposed.ts = function(x, title="", ...) {
  xx <- x$x
  if (is.null(xx)) 
    xx <- with(x, if (type == "additive") 
      random + trend + seasonal
      else random * trend * seasonal)
  plot(cbind(observed = xx, trend = x$trend, 
             seasonal = x$seasonal, random = x$random), 
       main=title, ...)
}
par(mar=c(2,4.5,2.5,0.2))
my_plot.decomposed.ts(Tmin.decompose, 
 title = "St. Paul (Minnesota, USA) Daily Tmin (deg C): 1941-1950",
               xlab ="", cex.lab=1.5, cex.axis=1.8)

#Check the minimum noise day:
n1 = which(Tmin.decompose$random < -20)
tda[(n1 + 1641 - 3): (n1 + 1641 + 3), 8:13]
#4268 19480907  0 -9999 23.3 13.3 -9999
#PRCP = 0 [mm], Tmin = 13.3 [deg C]

#
#
#R plot Fig. 7.5: Generate white noise
set.seed(119) # set seed to ensure the same simulation result
wa <- rnorm(500, mean = 0, sd = 1)
par(mar=c(4.5,4.5,2.5, 0.3))
plot(wa, type = 'l', 
     xlab = 'Time', ylab = expression(W[t]),
     main = 'A Simulated Time Series of White Noise ',
     cex.lab=1.5, cex.axis=1.5)

#
#
#R plot Fig. 7.6: Histogram and ACF of white noise
par(mfrow=c(1,2))
par(mar=c(4.5,4.5,2.5, 0))
hist(wa, freq=F, breaks=16, 
     xlim =c(-4,4), ylim=c(0,0.4),
     xlab = expression(W[t]),
     main='Histogram of a White Noise Time Series',
     cex.lab=1.5, cex.axis=1.5) 
x=seq(-4,4, by=0.1)
lines(x,dnorm(x, mean=0, sd=1), col='blue', 
      type = 'l', lwd=1.5)
par(mar=c(4.5,4.5,3, 0.3))
acf(wa, main='Auto-correlation of White Noise',
    cex.lab=1.5, cex.axis=1.5)
dev.off()
#
#
#R plot Fig. 7.7: Random walk time series
#(a) Five realizations with different sigma values
n = 1000 #Total number of time steps
m = 5 #Number of time series realizations
a = 0.05 #Drift delta = 0.05
b = c(0.1, 0.4, 0.7, 1.0, 1.5) #SD sigma
#Generate the random walk time series data
x = matrix(rep(0, m*n), nrow=m)
for(i in 1:m){
  w = rnorm(n, mean =0, sd = b[i])
  for(j in 1:(n-1)){
    x[i,j+1] = a + x[i,j] + w[j]
  }
}
#Plot the five time series realizations
par(mfrow=c(1,2))
par(mar=c(4.5,4.5, 2.5, 0.5))
plot(x[1,], type='l', ylim=c(-20,100),
     xlab ="Time steps: t", ylab = expression(X[t]),
     main = expression('Five realizations of random walks:'
                       ~ delta ~'= 0.05'),
     cex.lab=1.6, cex.axis=1.5, cex.main =1.6)
lines(x[2,], type='l', col='blue')
lines(x[3,], type='l', col='red')
lines(x[4,], type='l', col='orange')
lines(x[5,], type='l', col='brown')
legend(-100, 110, 
       legend=c(expression(sigma ~ '= 0.1'), 
                expression(sigma ~ '= 0.4'),
                expression(sigma ~ '= 0.7'),
                expression(sigma ~ '= 1.0'),
                expression(sigma ~ '= 1.5')), 
       col=c('black','blue','red','orange','brown'), 
       x.intersp = 0.2, y.intersp = 0.4, 
       seg.len = 0.6, lty =1, bty ='n', cex = 1.3)
text(20,-20, "(a)", cex =1.4)

#(b) 100 realizations with fixed parameters 
#Random walk to show variance increasing with time
n = 1000 #Total number of time steps
m = 100 #Number of time series realizations
a = 0.0 #Drift delta = 0
b = rep(1, m) #SD sigma is the same
#Generate the random walk time series data
x = matrix(rep(0, m*n), nrow=m)
for(i in 1:m){
  w = rnorm(n, mean =0, sd = b[i])
  for(j in 1:(n-1)){
    x[i,j+1] = a + x[i,j] + w[j]
  }
}
#Plot the series realizations
par(mar=c(4.5,4.5, 2.5, 0.8))
plot(x[1,], type='l', ylim=c(-100,100),
     xlab ="Time steps: t", ylab = expression(X[t]),
     main = expression('100 realizations of random walks:'
                       ~ delta ~'= 0,'~~ sigma ~'= 1'),
     cex.lab=1.6, cex.axis=1.5, cex.main =1.6)
for(i in 2:m){
  lines(x[i,], type='l')
}
library(matrixStats)
y = colSds(x)
lines(y,type='l', lwd=2, col='red')
lines(-y,type='l', lwd=2, col='red') 
z = sqrt(1:n)
lines(z,type='l', lwd=2, col='blue')
lines(-z,type='l', lwd=2, col='blue')
legend(-150, 120, 
    legend=c('Standard deviation of the simulations SD(t)',
  expression('Theorectical formula: SD(t)'%prop% sqrt(t))),
       col=c('red','blue'), cex = 1.3,
       x.intersp = 0.2, y.intersp = 0.3, 
       seg.len = 0.4, lty=1, bty='n', lwd =2)
text(20,-100, "(b)", cex =1.4)
dev.off() #go back to R's default figure setting

#R plot Fig. 7.8: Moving average time series
n = 121
w = rnorm(n)
x1 = x2 = x3 = rep(0, n)
for (t in 5:n){
  x1[t] = 0.5*w[t] + 0.5*w[t-1]
  x2[t] = 0.9*w[t] + 0.1*w[t-1]
  x3[t] = (1/5)*(w[t] + w[t-1] + w[t-2] + w[t-3] + w[t-4])
}
plot.ts(x1[5:n], ylim= c(-2,3),
        main="Realizations of moving average time series",
        xlab ="Time steps", ylab = expression(X[t]),
        cex.lab=1.5, cex.axis=1.5, lwd=2)
lines(x2[5:n], col='blue')
lines(x3[5:n], col='red', lwd=2)
legend(5, 3.2, bty='n', lty=1, y.intersp=0.7,
       legend=(c("MA(1): a = 0.5, b = 0.5",
                 "MA(1): a = 0.9, b = 0.1",
                 "MA(5): Uniform weight = 1/5")),
       col=c('black','blue','red'), 
       lwd = c(2,1,2))

#R plot Fig. 7.9: Autoregressive time series AR(1)
set.seed(791)
n = 121 #Number of time steps
m = 4 #Number of realizations
lam = 0.8 #Decay parameter
x = matrix(rep(4, n*m), nrow=m) #x0 = 4
#Simulate the time series data
for(k in 1:m){
  for(i in 1:(n-1)){
    x[k,i+1] = x[k,i]*lam + 
      rnorm(1, mean=0, sd=0.25)
  }
}
#Plot the realizations and their mean
plot.ts(x[1,], type = 'l', ylim=c(-2,4),
        main="Realizations of an AR(1) time series and their mean",
        xlab ="Time steps", ylab = expression(X[t]),
        cex.lab=1.5, cex.axis=1.5,
        col='purple')
lines(x[2,], col='blue')
lines(x[3,], col='red')
lines(x[4,], col='orange')
lines(colMeans(x),  lwd=3)

#R plot Fig. 7.10: ACF of AR(1) 
setwd('/Users/sshen/climstats')
n = 481 #Number of time steps
m = 2 #Number of realizations
lam = c(0.9, 0.6) #Decay parameter
x = matrix(rep(4, n*m), nrow=m) #x0 = 4
#Simulate the time series data
for(k in 1:m){
  for(i in 1:(n-1)){
    x[k,i+1] = x[k,i]*lam[k] + 
      rnorm(1, mean=0, sd=0.25)
  }
}
#Plot the auto-correlation function
setEPS() #Automatically saves the .eps file
postscript("fig0710.eps", width=10, height=5)
par(mfrow=c(1,2))
par(mar=c(4.5, 4.5, 3, 1))
acf(x[1,], lag.max = 36,
    main = 'Auto-correlation function of AR(1)',
    xlab='Time lag',
    cex.lab=1.5, cex.axis=1.5)
text(20, 0.8, bquote('Decay parameter'~lambda == 0.9),
     cex=1.5)
par(mar=c(4.5,4.5,3, 0.3))
acf(x[2,], lag.max = 36, 
    main = 'Auto-correlation function of AR(1)',
    xlab='Time lag', col='red',
    cex.lab=1.5, cex.axis=1.5)
text(20, 0.8, expression('Decay parameter'~lambda == 0.6),
     col='red', cex=1.5)
dev.off()

#R plot Fig. 7.11: ARIMA model fitting
setwd("/Users/sshen/climstats")
co2m = read.table("data/co2m.txt", header = TRUE)
mon = co2m[,3]
co2 = co2m[,5]
co2.ts = ts(co2, start=c(1958,3), end=c(2020,7), 
            frequency =12) 
#Then fit an AR(1) model
co2.AR1 <- arima(co2.ts, order = c(1,0,0))
#Obtain the AR(1) model fit data
AR1_fit <- co2.ts - residuals(co2.AR1) 
#Fit an MA(1) model
co2.MA1 <- arima(co2.ts, order = c(0,0,1))
#Obtain the MA(1) model fit data
MA1_fit <- co2.ts - residuals(co2.MA1) 

setEPS() #Automatically saves the .eps file
postscript("fig0711.eps", width=8, height=6)
#Plot the CO2 time series
par(mar=c(3,4.5,2.5, 0.5))
plot(mon, co2, ylim=c(310,420),
     type='l', lwd=2, col = 'red',
     main = bquote("Monthly Atmospheric "~ 
                     CO[2]~" at Mauna Loa Observatory"),
     xlab ="", 
     ylab="Parts Per Million [ppm]",
     cex.main=1.5,
     cex.lab =1.5, cex.axis=1.5) 
#Plot the AR(1) model data on the CO2 data
points(AR1_fit, type = "l", lwd=1, 
       col = 'black', lty = 2)
points(MA1_fit, type = "l", lwd=1, 
       col = 'blue', lty = 2)
legend(1960, 420, lty=c(1, 2, 2),
       col=c('red','black', 'blue'),
       legend=c("Observed data", 
                "AR(1) model fitting", 
                "MA(1) model fitting"),
       text.col = c('red','black', 'blue'),
       x.intersp = 0.2, y.intersp = 1.4, 
       seg.len = 0.8, cex=1.5, bty = 'n')
dev.off()


################################################################
#
# Chapter 8: Spectral Analysis and Filtering of Time Series
#
################################################################

#
setwd("~/climstats")
# R plot Fig. 8.1: Waves of different amplitudes, 
# periods and phases
setEPS()
postscript("fig0801.eps", height=6, width=10)
par(mar = c(4.5, 4.5, 2.5, 0.5))
par(mfrow=c(2,2))
t = seq(0, 2, len = 1000)
y1 = sin(2*pi*t) #A=1, tau=1, phi=0
y2 = sin(2*pi*t/0.5) #A=1, tau=0.5, phi=0
y3 = 0.5*sin(2*pi*t  - pi/2) #A=0.5, tau=1, phi=0
y4 = 0.5*sin(2*pi*t/0.5 + pi/6) #A=0.5, tau=1, phi=pi/6
plot(t, y1, 
     type = 'l', lwd = 2,
     xlab = " ", ylab = "T(t)",
     cex.lab =1.5, cex.axis = 1.5,
     cex.main = 1.5,
     main=expression(paste(
       'Function T(t): A=1, ', tau, '=1, ', phi,'=0')))
plot(t, y2, 
     type = 'l', lwd = 2,
     xlab = " ", ylab = "T(t)",
     cex.lab =1.5, cex.axis = 1.5,
     cex.main = 1.5, col = 'red',
     main=expression(paste(
       'Function T(t): A=1, ', tau, '=0.5, ', phi,'=0')))
plot(t, y3, 
     type = 'l', lwd = 2,
     xlab = "Time t", ylab = "T(t)",
     cex.lab =1.5, cex.axis = 1.5,
     cex.main = 1.5, col = 'blue',
     main=expression(paste(
       'Function T(t): A=0.5, ', tau, '=1, ', phi,'=', - pi/2)))
plot(t, y4, 
     type = 'l', lwd = 2,
     xlab = "Time t", ylab = "T(t)",
     cex.lab =1.5, cex.axis = 1.5,
     cex.main = 1.5, col = 'purple',
     main=expression(paste(
       'Function T(t): A=0.5, ', tau, '=0.5, ', phi,'=', pi/6)))
dev.off()

#
#
#R plot Fig. 8.2: Wave superposition
setEPS()
postscript("fig0802.eps", height=6, width=10)
par(mar = c(4.5, 4.8, 2.5, 0.5))
plot(t, y1 + y2 + 2*y3 + y4, 
     type = 'l', lwd = 4,
     xlab = "Time t", ylab = "T(t)",
     cex.lab =1.5, cex.axis = 1.5,
     cex.main = 1.5, col = 'blue',
     main='Superposition of several harmonics')
dev.off()

#
#
#R code for the discrete sine transform (DST)
M = 5
time = 1:M - 1/2
freq = 1:M - 1/2
time_freq = outer(time, freq)
#Construct a sine transform matrix
Phi = sqrt(2/M)*sin((pi/M)*time_freq)
#Verify Phi is an orthogonal matrix
round((t(Phi)%*%Phi), digits = 2)
#The output is an identity matrix

ts = time^2  #Given time series data
ts
#[1]  0.25  2.25  6.25 12.25 20.25
ts_dst = t(Phi)%*%ts #DST transform
Recon = Phi%*%ts_dst #inverse DST 
t(Recon) #Verify Recon = ts
#[1,] 0.25 2.25 6.25 12.25 20.25

#
#
#R plot Fig. 8.3: Polar expression of a complex number
setwd("~/climstats")
setEPS()
postscript("fig0803.eps", height=5.5, width=5.5)
par(mar = c(0,0,0,0))
r=10
bb=1.5*r
t=seq(0, 2*pi, length=200)
x=r*cos(t)
y=r*sin(t)
plot(x,y,type="l", lwd=3, asp=1, 
     xlim=c(-bb + 3, bb),ylim=c(-bb + 3, bb),
     xaxt="n",yaxt="n",ann=FALSE,bty="n")
aa=1.4*r
x1=r*cos(pi/3)
y1=r*sin(pi/3)
arrows(c(-aa + 2, 0), c(0, -aa + 2), c(aa,0), c(0, aa), 
       length=0.1, code=2, lwd=2)
arrows(0,0,0.98*x1,0.98*y1, 
       length=0.15,code=2, lwd=1, angle=15)
t2=seq(0,pi/3,length=10)
x2=0.22*r*cos(t2)
y2=0.22*r*sin(t2)
lines(x2,y2, type="l")
points(x1,y1,pch=19,cex=1)
segments(x1,0, x1,y1)
text(1.1*r,0.1*r, "1", cex=1.3)
text(-0.1*r, 1.1*r, "i", cex=1.5)
text(1.2*x1,1.1*y1, "(x,y)", cex=1.3)
text(0.3*r,-0.1*r, expression(paste(cos,theta)), 
     cex=1.3)
text(1.35*x1,0.5*y1, expression(paste(sin,theta)), 
     cex=1.3)
text(1.35*r,-0.1*r, "Real Axis", cex=1.3)
text(0.5*r, 1.35*r, "Imaginary Axis", cex=1.3)
text(-0.1*r, -0.1*r, "0", cex=1.5)
text(0.3*r*cos(pi/6),0.3*r*sin(pi/6), 
     expression(paste(theta)), cex=1.3)
text(1.9*x1,1.3*y1, 
expression(paste(e^{i*theta},"=", "cos",theta,"+ i sin",theta)), 
     cex=1.5)
dev.off()


#
# R plot Fig. 8.4: The unitary DFT matrix
M = 200
i  = complex(real = 0, imaginary = 1)
time_freq = outer(0:(M-1), 0:(M-1))
U = exp(i*2*pi*time_freq/M) / sqrt(M)
Ure = Re(U) #Real part of U
Uim = Im(U) #Imaginary part of U

setEPS()
postscript("fig0804a.eps", height=8.1, width=10)
par(mar=c(4.2, 5.5, 1.5, 0))
#Plot the real part
filled.contour(0:(M-1), 0:(M-1), Ure,
               color.palette = heat.colors,
               #     xlab = 't', ylab ='k',
               plot.axes={
                 axis(1,cex.axis=1.8)
                 axis(2,cex.axis=1.8)},
               plot.title={
  title(main = '(a) Real Part of the DFT Unitary Matrix',
                       xlab="t", cex.lab=2, cex.main = 1.5)
                 mtext("k",2,cex=2,line=4,las=1)
               },
               key.axes = axis(4, cex.axis = 2)
)
dev.off()
#Plot the imaginary part
setEPS()
postscript("fig0804b.eps", height=8.1, width=10)
par(mar=c(4.2, 5.5, 1.5, 0))
#Plot the real part
filled.contour(0:(M-1), 0:(M-1), Uim,
               color.palette = rainbow,
               #     xlab = 't', ylab ='k',
               plot.axes={
                 axis(1,cex.axis=1.8)
                 axis(2,cex.axis=1.8)},
               plot.title={
  title(main = '(b) Imaginary Part of the DFT Unitary Matrix',
                       xlab="t", cex.lab=2, cex.main = 1.5)
                 mtext("k",2,cex=2,line=4,las=1)
               },
               key.axes = axis(4, cex.axis = 2)
)
dev.off()



setEPS()
postscript("fig0804b2.eps", height=8.5, width=10)
par(mar=c(4, 4.5, 1.5, 0))
filled.contour(0:(M-1), 0:(M-1), Uim,
               color.palette = rainbow,
               xlab = 't', ylab ='k',
               cex.lab = 1.5, cex.axis = 1.8,
               main= 'Imaginary Part of the DFT Unitary Matrix')
dev.off()

#
#
# R code for DFT and iDFT
M = 9
ts = (1:M)^(1/2) #X is ts here
round(ts, digits = 2)
#[1]  1.00 1.41 1.73 2.00 2.24 ...
i  = complex(real = 0, imaginary = 1)
time_freq = outer(0:(M-1), 0:(M-1))
U = exp(i*2*pi*time_freq/M) / sqrt(M)
ts_dft = t(Conj(U))%*%ts #DFT of ts
ts_rec = U%*%ts_dft #Inverse DFT for reconstruction
#Verify the reconstruction
round(t(ts_rec), digits = 2) 
#[1,] 1+0i 1.41+0i 1.73+0i 2+0i 2.24+0i ...

#
#
#R plot Fig. 8.5: Re and Im of the first four harmonics in DFT
M = 200
time = 1:200
i  = complex(real = 0, imaginary = 1)
time_freq = outer(0:(M-1), 0:(M-1))
U = exp(i*2*pi*time_freq/M) / sqrt(M)
Ure = Re(U) #Real part of U
Uim = Im(U) #Imaginary part of U
setEPS()
postscript("fig0805.eps", height=6, width=8)
layout(matrix(c(1,2,3,4,5,6,7,8), 
              nrow = 4, ncol = 2),
       heights = c(0.92, 0.7, 0.7, 1.16, 
                   0.92, 0.7, 0.7, 1.16),
       widths  = c(4, 3.3) # Widths of the 2 columns
) 
par(mar=c(0,5,3,0)) #Zero space between (a) and (b)
plot(time, Ure[4,], pch =16, cex =0.3, xaxt="n", 
     yaxt="n", xlab="", ylab="k=3",
     cex.axis = 1.6,  cex.lab = 1.6,  
     main ='Real part of DFT harmonics')
par(mar=c(0,5,0,0))
plot(time,Ure[3,],pch =16, cex =0.3, xaxt="n", 
     yaxt="n", xlab="", ylab="k=2",
     cex.axis = 1.6,  cex.lab = 1.6)
par(mar=c(0,5,0,0))
plot(time,Ure[2,], pch =16, cex =0.3, xaxt="n", 
     yaxt="n", ylim = c(-0.1, 0.1),
     xlab="", ylab="k=1",
     cex.axis = 1.6, cex.lab = 1.6)
par(mar=c(6,5,0,0))
plot(time,Ure[1,], pch =16, cex =0.3, 
     xaxt="n", yaxt="n",
     ylim = c(-0.1, 0.1),
     xlab="t", ylab="k=0",
     cex.axis = 1.6,  cex.lab = 1.6)
axis(1, at = c(0, 50, 100, 150), cex.axis=1.6)
axis(2, at = c(-0.1, 0, 0.1), cex.axis=1.6)

par(mar=c(0,0,3,1)) #Zero space between (a) and (b)
plot(time,Uim[4,], pch = 16, cex =0.3, xaxt="n", 
     yaxt="n", xlab="", ylab="",
     cex.axis = 1.6,  cex.lab = 1.6,  
     main ='Imaginary part of DFT harmonics')
par(mar=c(0,0,0,1))
plot(time,Uim[3,], pch = 16, cex = 0.3, xaxt="n", 
     yaxt="n", xlab="", ylab="",
     cex.axis = 1.6,  cex.lab = 1.6)
par(mar=c(0,0,0,1))
plot(time,Uim[2,], pch = 16, cex = 0.3, xaxt="n", 
     yaxt="n", ylim = c(-0.1, 0.1),
     xlab="", ylab="",
     cex.axis = 1.6, cex.lab = 1.6)
par(mar=c(6,0,0,1))
plot(time,Uim[1,], pch = 16, cex = 0.3, 
     yaxt="n", ylim = c(-0.1, 0.1),
     xlab="t", ylab="",
     cex.axis = 1.6, cex.lab = 1.6)
dev.off()

#
#
#R plot of Figs. 8.6 and 8.7
library(ncdf4)
nc=ncdf4::nc_open("/Users/sshen/climstats/data/air.mon.mean.nc")
Lon <- ncvar_get(nc, "lon")
Lat1 <- ncvar_get(nc, "lat")
Time <- ncvar_get(nc, "time")
library(chron)
month.day.year(1297320/24,
               c(month = 1, day = 1, year = 1800)) 
#1948-01-01
NcepT <- ncvar_get(nc, "air")
dim(NcepT)
#[1] 144 73 878, 
#i.e., 878 months=1948-01 to 2021-02, 73 years 2 mons
#Tokyo (35.67N, 139.65E) monthly temperature data 2011-2020
Lat1[23]
#[1] 35oN
Lon[57]
#[1] 140
m1 = month.day.year(Time/24,
                    c(month = 1, day = 1, year = 1800)) 
m1$year[757]
#[1] 2011
m1$month[757]
#[1] 1  i.e., January
TokyoT = NcepT[56,29, 757:876] #2011-2020
M = length(TokyoT)
t1 = seq(2011, 2020, len = M)
#Plot Fig. 8.6: Tokyo temperature 2011-2020
plot(t1, TokyoT, 
     type = 'l', lwd=1.5,
     xlab = "Time [mon]", 
     ylab = 'Temperature [deg C]',
     main = 'NCEP Monthly Tokyo Temperature: 2011-2020',
     cex.lab=1.4, cex.axis=1.4)
#Compute FFT
TokyoT_FFT = fft(TokyoT-mean(TokyoT))

#R plot Fig. 8.7: Peoriodogram of Tokyo temperature
setEPS()
postscript("fig0807.eps", height=8, width=8)
layout(matrix(c(1,2,3,4), 
              nrow = 2, ncol = 2),
       heights = c(1, 1, 1, 1),
       widths  = c(1, 1) # Widths of the 2 columns
) 
par(mar=c(4.5, 5, 2, 1))
kk = 0:59
plot(kk, Mod(TokyoT_FFT)[1:60]^2,
     type = 'l', lwd=2,
     xlab = 'k', ylab = 'Spectral Power',
     cex.lab = 1.5, cex.axis = 1.5,
     main = 'Periodogram of Mon. Tokyo Temp')
text(50,21000, '(a)', cex = 2)

f_mon = kk/M
plot(f_mon, Mod(TokyoT_FFT)[1:60]^2,
     type = 'l', lwd=2, 
     xlab = 'Cycles per month', ylab = 'Spectral Power',
     cex.lab = 1.5, cex.axis = 1.5,
     main = 'Periodogram in terms of month')
text(0.45,21000, '(c)', cex = 2)
#axis(1, at = c(0.08, 0.17, 0.33, 0.50), cex.axis=1.6)

f_year = 12*kk/M
plot(f_year, Mod(TokyoT_FFT)[1:60]^2,
     type = 'l', lwd=2,
     xlab = 'Cycles per year', ylab = 'Spectral Power',
     cex.lab = 1.5, cex.axis = 1.5,
     main = 'Periodogram in terms of year')
text(5.5,21000, '(b)', cex = 2)

tau = 120/kk[0:60]
plot(tau, Mod(TokyoT_FFT)[1:60]^2,
     log = 'x', xaxt="n",
     type = 'l', lwd=2,
     xlab = 'Period in month (in log scale)', 
     ylab = 'Spectral Power',
     cex.lab = 1.5, cex.axis = 1.5,
     main = 'Periodogram in terms of period')
text(90,21000, '(d)', cex = 2)
axis(1, at = c(3, 6, 12, 24, 48, 96), cex.axis=1.6)

dev.off()

#
#
#R code to verify Parseval's identity
M = 8
X = rnorm(M) #Time series data
DFT_X = fft(X)/sqrt(M) #Compute DFT using FFT
t(X)%*%X - sum(Mod(DFT_X)^2)
#[1,] 2.220446e-15 #Approximately zero


#
#
### R plot Fig. 8.8: Fourier series over [-1,1]
i  = complex(real = 0, imaginary = 1)
T = 2
t = seq(-T/2,T/2, len = 401)
#Define the original function x(t)
xt <- ( t >= -1 & t < 0) * (-4) +
  ( t <= 1 & t > 0) * (4) 
#Plot the function x(t)
setEPS()
postscript("fig0808.eps", height=5, width=8)
par(mar = c(4, 4.5, 2, 0.5))
plot(t, xt, type = 'l', 
     ylim = c(-5,5),
     xlab='t', ylab='x(t)',
main = 'Approximate a function by a finite sum of series',
     cex.lab = 1.5, cex.axis =1.4, cex.main =1.4,
     lwd = 5, col = 'red')

#Plot the partial sum of Fourier series from -K to K
J = c(3, 10, 100)
Fcol = c('brown', 'blue','black')
for (j in 1:3){
  k = -J[j]:J[j]
  RK= colSums(8/(i*pi*(2*k-1))*exp(i*pi*outer(2*k-1, t)))
  lines(t, Re(RK), type = 'l', 
        col = Fcol[j])
}
legend(-1.05, 5.1, 
       legend = c('Function x(t)', 'Sum of 7 terms', 
                  'Sum of 21 terms', 'Sum of 201 terms'),
       col = c('red', 'brown', 'blue', 'black'), 
       lty = c(1,1,1,1), lwd = c(5, 1,1,1),
       cex = 1.3, bty = 'n')
dev.off()

#R plot Fig. 8.9 for Exercise 8.4
setwd('~/climstats')
setEPS() #Automatically saves the .eps file
postscript("fig0809.eps", height=5, width=7)
par(mar = c(4.5, 4, 2.5, 0.2))
#set.seed(101)
M = 51
t = seq(0, 10, len = M)
ys = 10*sin(t)
yn = rnorm(M, 0, 3)
yd = ys + yn
plot(t, yd, type = 'o', lwd = 2,
     ylim = c(-20, 20), 
     xlab = 't', ylab = 'y',
     main = 'Data, signal, and noise for a DST filter',
     cex.lab = 1.4, cex.axis = 1.4) 
legend(0, -16, 'Data = Signal + Noise', 
       lty = 1, bty = 'n', lwd = 2, cex = 1.4)
lines(t, ys, col = 'blue', lwd = 4)
legend(0, -10, 'Signal', cex = 1.4,
       lty = 1, bty = 'n', lwd = 4, col = 'blue')
lines(t, yn, col = 'brown')
legend(0, -13, 'Noise', cex = 1.4,
       lty = 1, bty = 'n', col = 'brown')
dev.off()

################################################################
#
# Chapter 9: Basics of Machine Learning
#
################################################################

#
#tWCSS calculation for N = 3 and K = 2
N = 3; K =2
mydata <- matrix(c(1, 1, 2, 1, 3, 3.5), 
                 nrow = N, byrow = TRUE)
x1 = mydata[1, ]
x2 = mydata[2, ]
x3 = mydata[3, ]

#Case C1 = (P1, P2)
c1 = (mydata[1, ] + mydata[2, ])/2
c2 = mydata[3, ]
tWCSS = norm(x1 - c1, type = '2')^2 + 
  norm(x2 - c1, type = '2')^2 + 
  norm(x3 - c2, type = '2')^2
tWCSS
#[1] 0.5

#Case C1 = (P1, P3)
c1 = (mydata[1, ] + mydata[3, ])/2
c2 = mydata[2, ]
norm(x1 - c1, type = '2')^2 + 
  norm(x3 - c1, type = '2')^2 + 
  norm(x2 - c2, type = '2')^2
#[1] 5.125

#Case C1 = (P2, P3)
c1 = (mydata[2, ] + mydata[3, ])/2
c2 = mydata[1, ]
norm(x2 - c1, type = '2')^2 + 
  norm(x3 - c1, type = '2')^2 + 
  norm(x1 - c2, type = '2')^2
#[1] 3.625

#The case C1 = (P1, P2) can be quickly found by 
kmeans(mydata, 2) 
#Clustering vector:
#[1] 1 1 2 #points P1, P2 in C1
#
#
#
# R plot Fig. 9.1: K-means for N = 3 and K = 2
setwd("~/climstats")
N = 3 #The number of data points
K = 2 #Assume K clusters
mydata = matrix(c(1, 1, 2, 1, 3, 3.5), 
                nrow = N, byrow = TRUE)
Kclusters = kmeans(mydata, K) 
Kclusters #gives the K-means results, 
#e.g., cluster centers and WCSS 
#Cluster means:
#[,1] [,2]
#1  1.5  1.0
#2  3.0  3.5
#Within cluster sum of squares by cluster:
#  [1] 0.5 0.0
Kclusters$centers
par(mar = c(4,4,2.5,0.5))
plot(mydata[,1], mydata[,2], lwd = 2,
     xlim =c(0, 4), ylim = c(0, 4),
     xlab = 'x', ylab = 'y', col = c(2, 2, 4),
     main = 'K-means clustering for 
     three points and two clusters',
     cex.lab = 1.4, cex.axis = 1.4)
points(Kclusters$centers[,1], Kclusters$centers[,2],
       col = c(2, 4), pch = 4)
text(1.5, 0.8, bquote(C[1]), col = 'red', cex = 1.4)
text(3.2, 3.5, bquote(C[2]), col = 'skyblue', cex = 1.4)
text(1, 1.2, bquote(P[1]), cex = 1.4, col = 'red')
text(2, 1.2, bquote(P[2]), cex = 1.4, col = 'red')
text(3, 3.3, bquote(P[3]), cex = 1.4, col = 'skyblue')

#
#R for Fig. 9.2: K-means clustering for 2001 daily weather 
#data at Miami International Airport, Station ID USW00012839
setwd("~/climstats")
dat = read.csv("data/MiamiIntlAirport2001_2020.csv", 
               header=TRUE)
dim(dat)
#[1] 7305   29
tmin = dat[,'TMIN']
wdf2 = dat[,'WDF2']
# plot the scatter diagram Tmin vs WDF2
setEPS() #Plot the data of 150 observations
postscript("fig0902a.eps",  width=5, height=5)
par(mar=c(4.5, 4.5, 2, 4.5))
plot(tmin[2:366], wdf2[2:366], 
     pch =16, cex = 0.5,
     xlab = 'Tmin [deg C]',
     ylab = 'Wind Direction [deg]', grid())
title('(a) 2001 Daily Miami Tmin vs WDF2', cex.main = 0.9, line = 1)
axis(4, at = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
     lab = c('N', 'NE', 'E', 'SE', 'S', 'SW',  'W', 'NW', 'N'))
mtext('Wind Direction', side = 4, line =3)
dev.off()
#K-means clustering 
K = 2 #assuming K = 2, i.e., 2 clusters
mydata = cbind(tmin[2:366], wdf2[2:366])
fit = kmeans(mydata, K) # K-means clustering
#Output the coordinates of the cluster centers
fit$centers 
#1 18.38608 278.8608
#2 21.93357 103.9161
fit$tot.withinss # total WCSS
#[1] 457844.9 for # the value may vary for each run

#Visualize the clusters by kmeans.ani()
mycluster <- data.frame(mydata, fit$cluster)
names(mycluster)<-c('Tmin [deg C]', 
                    'Wind Direction [deg]',
                    'Cluster')
library(animation)
par(mar = c(4.5, 4.5, 2, 4.5))
kmeans.ani(mycluster, centers = K, pch=1:K, col=1:K,
           hints = '')
title(main=
        "(b) K-means Clusters for Daily Tmin vs WDF2", 
      cex.main = 0.8)
axis(4, at = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
     lab = c('N', 'NE', 'E', 'SE', 'S', 'SW',  'W', 'NW', 'N'))
mtext('Wind Direction', side = 4, line =3)



#
#R plot Fig. 9.3: tWCSS(K) and pWCSS(K)
setwd("~/climstats")
dat = read.csv("data/MiamiIntlAirport2001_2020.csv", 
               header=TRUE)
dim(dat)
#[1] 7305   29
tmin = dat[,'TMIN']
tmax = dat[,'TMAX']
wdf2 = dat[,'WDF2']
mydata = cbind(tmin[2:366], wdf2[2:366])
twcss = c()
for(K in 1:8){
  mydata=cbind(tmax[2:366], wdf2[2:366])
  twcss[K] = kmeans(mydata, K)$tot.withinss 
}
twcss
par(mar = c(4.5, 6, 2, 0.5))
par(mfrow=c(1,2))
plot(twcss/100000, type = 'o', lwd = 2,
     xlab = 'K', ylab = bquote('tWCSS [ x' ~  10^5 ~ ']'),
     main = '(a) The elbow principle from tWCSS scores',
     cex.lab = 1.5, cex.axis = 1.5)
points(2, twcss[2]/100000, pch =16, cex = 3, col = 'blue')
text(4, 5, 'Elbow at K = 2', cex = 1.5, col = 'blue')
#compute percentage of variance explained
pWCSS = 100*(twcss[1]- twcss)/twcss[1] 
plot(pWCSS, type = 'o', lwd = 2,
     xlab = 'K', ylab = 'pWCSS [percent]',
     main = '(b) The knee principle from pWCSS variance',
     cex.lab = 1.5, cex.axis = 1.5)
points(2, pWCSS[2], pch =16, cex = 3, col = 'blue')
text(4, 80, 'Knee at K = 2', cex = 1.5, col = 'blue')
dev.off()


for(K in 1:8){
  mydata=cbind(tmax[2:366], wdf2[2:366])
  clusterK <- kmeans(mydata, K) # 5 cluster solution
  twcss[K] = fit$tot.withinss
}
twcss
plot(twcss, type = 'o')
pvariance = 100*(twcss[1]- twcss)/twcss[1] 
plot(pvariance, type = 'o')

#R plot Fig. 9.3b: pWCSS(K) for the knee principle


######################
#scale the data because of different units
#equivalent to standardized anomalies
mydata = data.frame(cbind(tmin[2:366], wdf2[2:366]))
tminS = scale(tmin[2:366])
wdf2S = scale(wdf2[2:366])
K = 2 #assuming K = 2, i.e., 2 clusters
mydataS = data.frame(cbind(tminS, wdf2S))
clusterK = kmeans(mydataS, K) # K-means clustering
#Output the scaled coordinates of the cluster centers
clusterK$centers 
#1 -0.9165034  1.7527775
#2  0.2252159 -0.4307167
clusterK$tot.withinss # total WCSS
#[1] 377.103 for # the value may vary for each run
#Centers in non-scaled units
Centers = matrix( nrow = 2, ncol = 2) 
Centers[,1] =
  clusterK$centers[,1] * attr(tminS, 
                              "scaled:scale") + attr(tminS, "scaled:center")
Centers[,2] =
  clusterK$centers[,2] * attr(wdf2S, 
                              "scaled:scale") + attr(wdf2S, "scaled:center")
Centers
#[1,] 17.14306 282.5000
#[2,] 22.15427 107.2014

#Plot clusters using convex hull
i = 1
N = 365
mycluster = clusterK$cluster
plotdat = cbind(1:N, mydata, mycluster)
X = plotdat[which(mycluster == i), 1:3]
plot(X[,2:3], pch = 16, cex = 0.5,
     xlim = c(0, 30), ylim = c(0, 365),
     col = 'red')
grid(5, 5)
hpts <- chull(X[, 2:3])
hpts <- c(hpts, hpts[1])
lines(X[hpts, 2:3], col = 'red')
for(j in 1:length(X[,1])){
  text(X[j,2], X[j,3] + 8, paste("", X[j,1]), 
       col = 'red', cex = 0.8)
}



d1 = tminS[1:365] * attr(tminS, "scaled:scale") + attr(tminS, "scaled:center")
d1 - tmin[2:366]
#Visualize the clusters by fviz_cluster()
library(factoextra)
fviz_cluster(clusterK, data = mydataS, stand = FALSE,
             main = 'K-means Clustering for the Miami Daily Tmin vs WDF2')

#Visualize the clusters by kmeans.ani()
mycluster <- data.frame(mydata, clusterK$cluster)
names(mycluster)<-c('Daily Tmin [deg C]', 
                    'Wind Direction [deg]',
                    'Cluster')
library(animation)
par(mar = c(4.5, 4.5, 2, 0.5))
kmeans.ani(mycluster, centers = K, pch=1:K, col=1:K,
           hints = '')
title(main=
        "(b) K-means for Tmin vs WDF2")

#Visualize the clusters by kmeans.ani()
mycluster <- data.frame(mydata, fit$cluster)
names(mycluster)<-c('Daily Tmin [deg C]', 
                    'Wind Direction [deg]',
                    'Cluster')
library(animation)
par(mar = c(4.5, 4.5, 2, 0.5))
kmeans.ani(mycluster, centers = K, pch=1:K, col=1:K,
           hints = '')
title(main=
        "(b) K-means for Tmin vs WDF2")

#
#
#R plot Fig. 9.4: Convex hull for a cluster 
setwd("~/climstats")
dat = read.csv("data/MiamiIntlAirport2001_2020.csv", 
               header=TRUE)
dim(dat)
#[1] 7305   29
tmin = dat[,'TMIN']
wdf2 = dat[,'WDF2']
#K-means clustering 
K = 2 #assuming K = 2, i.e., 2 clusters
mydata = cbind(tmin[2:366], wdf2[2:366]) #2001 data
clusterK = kmeans(mydata, K) # K-means clustering
mycluster <- data.frame(mydata, clusterK$cluster)
plotdat = cbind(1:N, mycluster)

par(mar = c(4.5, 6, 2, 4.5))#set up the plot margin
i = 2 # plot Cluster 1
N = 365 #Number of data points
X = plotdat[which(mycluster[,3] == i), 1:3]
colnames(X)<-c('Day', 'Tmin [deg C]', 'Wind Direction [deg]')
plot(X[,2:3], pch = 16, cex = 0.5, col = i,
     xlim = c(0, 30), ylim = c(0, 365),
     main = 'Cluster 1 of Miami Tmin vs WDF2' )
grid(5, 5)
#chull() finds the boundary points of a convex hull
hpts = chull(X[, 2:3]) 
hpts1 = c(hpts, hpts[1]) #close the boundary
lines(X[hpts1, 2:3], col = i)
for(j in 1:length(X[,1])){
  text(X[j,2], X[j,3] + 8, paste("", X[j,1]), 
       col = i, cex = 0.8)
} #Put the data order on the cluster
axis(4, at = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
     lab = c('N', 'NE', 'E', 'SE', 'S', 'SW',  'W', 'NW', 'N'))
mtext('Wind Direction', side = 4, line =3)

#
#
#R plot Fig. 9.5: Maximum difference between two points
x = matrix(c(1, 1, 3, 3), 
           ncol = 2, byrow = TRUE)#Two points
y= c(-1, 1) #Two labels -1 and 1
#Plot the figure and save it as a .eps file
setEPS() 
postscript("fig0905.eps", height=7, width=7)
par(mar = c(4.5, 4.5, 2.0, 2.0))
plot(x, col = y + 3, pch = 19, 
     xlim = c(-2, 6), ylim = c(-2, 6),
     xlab = bquote(x[1]), ylab = bquote(x[2]),
     cex.lab = 1.5, cex.axis = 1.5, 
     main = "Maximum Difference Between Two Points")
axis(2, at = (-2):6, tck = 1, lty = 2, 
     col = "grey", labels = NA)
axis(1, at = (-2):6, tck = 1, lty = 2, 
     col = "grey", labels = NA)
segments(1, 1, 3, 3)
arrows(1, 1, 1.71, 1.71, lwd = 2,
       angle = 9, length= 0.2)
text(1.71, 1.71-0.4, quote(bold('n')), cex = 1.5)
arrows(4, 0, 4 + 0.5, 0 + 0.5, lwd = 3,
       angle = 15, length= 0.2, col = 'green' )
text(4.7, 0.5 -0.2, quote(bold('w')), 
     cex = 1.5, col = 'green')
x1 = seq(-2, 6, len = 31)
x20 = 4 - x1
lines(x1, x20, lwd = 1.5, col = 'purple')
x2m = 2 - x1
lines(x1, x2m, lty = 2, col = 2)
x2p = 6 - x1
lines(x1, x2p, lty = 2, col = 4)
text(1-0.2,1-0.5, bquote(P[1]), cex = 1.5, col = 2)
text(3+0.2,3+0.5, bquote(P[2]), cex = 1.5, col = 4)
text(1-0.2,1-0.5, bquote(P[1]), cex = 1.5, col = 2)
text(0,4.3, 'Separating Hyperplane', 
     srt = -45, cex = 1.2, col = 'purple')
text(1.5, 4.8, 'Positive Hyperplane', 
     srt = -45, cex = 1.2, col = 4)
text(-1, 3.3, 'Negative Hyperplane', 
     srt = -45, cex = 1.2, col = 2)
dev.off()


#
#
#R plot Fig. 9.6: SVM for three points
#training data x
x = matrix(c(1, 1, 2, 1, 3, 3.5), 
           ncol = 2, byrow = TRUE)
y = c(1, 1, -1) #two categories 1 and -1
plot(x, col = y + 3, pch = 19,
     xlim = c(-2, 8), ylim = c(-2, 8))
library(e1071)
dat = data.frame(x, y = as.factor(y))
svm3P = svm(y ~ ., data = dat, 
            kernel = "linear", cost = 10, 
            scale = FALSE, 
            type = 'C-classification')
svm3P #This is the trained SVM
xnew = matrix(c(0.5, 2.5, 4.5, 4), 
              ncol = 2, byrow = TRUE) #New data
predict(svm3P, xnew) #prediction using the trained SVM
# 1  2 
# 1 -1

# Find hyperplane, normal vector, and SV (wx + b = 0)
w = t(svm3P$coefs) %*% svm3P$SV 
w
#[1,] -0.2758621 -0.6896552
b = svm3P$rho
b
#[1] -2.241379
2/norm(w, type ='2') #maximum margin of separation
#[1] 2.692582

x1 = seq(0, 5, len = 31)
x2 = (b - w[1]*x1)/w[2]
x2p = (1 + b - w[1]*x1)/w[2]
x2m = (-1 + b - w[1]*x1)/w[2]
x20 = (b - w[1]*x1)/w[2]
#plot the SVM results
setEPS() 
postscript("fig0906.eps", height=7, width=7)
par(mar = c(4.5, 4.5, 2.0, 2.0))
plot(x, col = y + 3, pch = 19,
     xlim = c(0, 6), ylim = c(0, 6),
     xlab = bquote(x[1]), ylab = bquote(x[2]),
     cex.lab = 1.5, cex.axis = 1.5,
     main = 'SVM for three points labeled in two categories')
axis(2, at = (-2):8, tck = 1, lty = 2, 
     col = "grey", labels = NA)
axis(1, at = (-2):8, tck = 1, lty = 2, 
     col = "grey", labels = NA)
lines(x1, x2p, lty = 2, col = 4)
lines(x1, x2m, lty = 2, col = 2)
lines(x1, x20, lwd = 1.5, col = 'purple')
xnew = matrix(c(0.5, 2.5, 4.5, 4), 
              ncol = 2, byrow = TRUE)
points(xnew, pch = 18, cex = 2)
for(i in 1:2){
  text(xnew[i,1] + 0.5, xnew[i,2] , paste('Q',i),
       cex = 1.5, col = 6-2*i)
}
text(2.2,5.8, "Two blue points and a red point 
are training data for an SVM ", 
     cex = 1.5, col = 4)
text(3.5,4.7, "Two black diamond points 
         are to be predicted by the SVM", 
     cex = 1.5)
dev.off()

#
#
#R plot Fig. 9.7: SVM for many points
#Training data x and y
x = matrix(c(1, 6, 2, 8, 3, 7.5, 1, 8, 4, 9, 5, 9, 
             3, 7, 5, 9, 1, 5,
             5, 3, 6, 4, 7, 4,   8, 6, 9, 5, 10, 6, 
             5, 0, 6, 5, 8, 2, 2, 2, 1, 1), 
           ncol = 2, byrow = TRUE)
y= c(1, 1, 1, 1, 1, 1, 
     1, 1, 1,
     2, 2, 2, 2, 2, 2 ,
     2, 2, 2, 2, 2)

library(e1071)
dat = data.frame(x, y = as.factor(y))
svmP = svm(y ~ ., data = dat, 
           kernel = "linear", cost = 10, 
           scale = FALSE, 
           type = 'C-classification')
svmP
#Number of Support Vectors:  3
svmP$SV #SVs are #x[9,], x[17,], x[19,]
#9   1  5
#17  6  5
#19  2  2

# Find SVM parameters: w, b, SV (wx+c=0)
w = t(svmP$coefs) %*% svmP$SV 
# In essence this finds the hyper plane that separates our points
w
#[1,] -0.39996 0.53328
b <- svmP$rho
b
#[1] 1.266573
2/norm(w, type ='2')
#[1] 3.0003 is the maximum margin
x1 = seq(0, 10, len = 31)
x2 = (b - w[1]*x1)/w[2]
x2p = (1 + b - w[1]*x1)/w[2]
x2m = (-1 + b - w[1]*x1)/w[2]
x20 = (b - w[1]*x1)/w[2]
#plot the svm results
setEPS() 
postscript("fig0907.eps", height=7, width=7)
par(mar = c(4.5, 4.5, 2.0, 2.0))
plot(x, col = y + 9, pch = 19, cex =1.5,
     xlim = c(0, 10), ylim = c(0, 10),
     xlab = bquote(x[1]), ylab = bquote(x[2]),
     cex.lab = 1.5, cex.axis = 1.5,
     main = 'SVM application to many points of two labels')
axis(2, at = 0:10, tck = 1, lty = 3, 
     col = "grey", labels = NA)
axis(1, at = 0:10, tck = 1, lty = 3, 
     col = "grey", labels = NA)
lines(x1, x2p, lty = 2, col = 10)
lines(x1, x2m, lty = 2, col = 11)
lines(x1, x20, lwd = 1.5, col = 'purple')
thetasvm = atan(-w[1]/w[2])*180/pi
thetasvm
#[1] 36.8699 #36.9 deg angle of the hyperplane
#linear equations for the hyperplanes
delx = 1.4
dely = delx * (-w[1]/w[2])
text(5 + 2*delx, 6.5 + 2*dely, bquote(bold(w%.%x) - b == 0), 
     srt = thetasvm, cex = 1.5, col = 'purple')
text(5 - delx, 7.6 - dely, bquote(bold(w%.%x) - b == 1), 
     srt = thetasvm, cex = 1.5, col = 10)
text(5, 4.8, bquote(bold(w%.%x) - b == -1), 
     srt = thetasvm, cex = 1.5, col = 11)
#normal direction of the hyperplanes
arrows(2, 3.86, 2 + w[1], 4 + w[2], lwd = 2,
       angle = 15, length= 0.1, col = 'blue' )
text(2 + w[1] + 0.4, 4 + w[2], bquote(bold(w)), 
     srt = thetasvm, cex = 1.5, col = 'blue')

#new data points to be predicted
xnew = matrix(c(0.5, 2.5, 7, 2, 6, 9), 
              ncol = 2, byrow = TRUE)
points(xnew, pch = 17, cex = 2)
predict(svmP, xnew) #Prediction
#1 2 3 
#2 2 1

for(i in 1:3){
  text(xnew[i,1], xnew[i,2] - 0.4 , 
       paste('Q',i), cex = 1.5)
}
dev.off()


#
#
#R plot Fig. 9.8: R.A. Fisher data of three iris species
setwd('~/climstats')
data(iris) #read the data already embedded in R
dim(iris)
#[1] 150   5
iris[1:2,] # Check the first two rows of the data
#    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#1          5.1         3.5          1.4         0.2  setosa
#2          4.9         3.0          1.4         0.2  setosa

str(iris) # Check the structure of the data
#'data.frame':	150 obs. of  5 variables:
#$ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#$ Species     : Factor w/ 3 levels "setosa","versicolor",...

setEPS() #Plot the data of 150 observations
postscript("fig0908.eps",  width=7, height=5)
par(mar= c(4.5, 4.5, 2.5, 0.2))
plot(iris[,1], type = 'o', pch = 16, cex = 0.5, ylim = c(-1, 9), 
     xlab = 'Sorted order of the flowers for measurement', 
     ylab = 'Length or width [cm]',
     main = 'R. A. Fisher data of irish flowers', col = 1, 
     cex.lab = 1.3, cex.axis = 1.3)
lines(iris[,2], type = 'o', pch = 16, cex = 0.5, col=2)
lines(iris[,3], type = 'o', pch = 16, cex = 0.5, col=3)
lines(iris[,4], type = 'o', pch = 16, cex = 0.5, col=4)
legend(0, 9.5, legend = c('Sepal length', 'Sepal width', 
                          'Petal length', 'Petal width'), 
       col = 1:4, lty = 1, lwd = 2, bty = 'n', 
       y.intersp = 0.8, cex = 1.2)
text(25, -1, 'Setosa 1-50', cex = 1.3)
text(75, -1, 'Versicolor 51-100', cex = 1.3)
text(125, -1, 'Virginica 101 - 150', cex = 1.3)
dev.off()

#
#
#R code of the RF experiment on the Fisher iris data 
#install.packages("randomForest") 
library(randomForest)
#set.seed(8)  # run this line to get the same result
#randomly select 120 observations as training data 
train_id = sort(sample(1:150, 120, replace = FALSE))
train_data = iris[train_id, ]
dim(train_data)
#[1] 120   5
#use the remaining 30 as the new data for prediction
new_data = iris[-train_id, ]
dim(new_data)
#[1] 30  5

#train the RF model
classifyRF = randomForest(x = train_data[, 1:4],
                          y = train_data[, 5], ntree = 800)
classifyRF #output RF training result
#Type of random forest: classification
#Number of trees: 800
#No. of variables tried at each split: 2


#OOB estimate of  error rate: 4.17%
#Confusion matrix:
#  		setosa versicolor virginica class.error
#setosa         41          0         0  0.00000000
#versicolor      0         34         2  0.05555556
#virginica       0          3        40  0.06976744

plot(classifyRF, 
     main='RF model error rate for each tree') 
#plot the errors vs RF trees Fig. 9.9a
#This plots the error rate data in the 
#matrix named classifyRF$err.rate
errRate = classifyRF$err.rate
dim(errRate)
#[1] 800   4
#Fig. 9.9a is a plot for this matrix data
#Fig. 9.9a can also be plotted by the following code
tree_num = 1:800
plot(tree_num, errRate[,1], 
     type = 's', col='black',
     ylim = c(0, 0.2),
     xlab = 'Trees', ylab = 'Error rate',
     main = 'RF model error rate for each tree',
     cex.axis = 1.3, cex.lab = 1.3)
lines(tree_num, errRate[,2], lwd =1.8,
      lty = 2, type = 's', col='red')
lines(tree_num, errRate[,3], lwd =1.8,
      lty = 3, type = 's', col='green')
lines(tree_num, errRate[,4], lwd =1.8,
      lty = 4, type = 's', col='skyblue')
legend(400, 0.21, lwd = 2.5,
       legend = c('OOB','Setosa', 'Vericolor', 'Virginica'),
       col=c('black', 'red', 'green', 'skyblue'), 
       lty=1:4, cex=1.2, y.intersp = 0.6,
       text.font = 2,  box.lty=0)

classifyRF$importance #classifyRF$ has many outputs
#             MeanDecreaseGini
#Sepal.Length         8.313131
#Sepal.Width          1.507188
#Petal.Length        31.075960
#Petal.Width         38.169763
#plot the importance result Fig. 9.9b
varImpPlot(classifyRF, sort = FALSE, 
           lwd = 1.5, pch = 16,
           main = 'Importance plot of the RF model',
           cex.axis = 1.3, cex.lab = 1.3)

classifyRF$confusion

#RF prediction for the new data based on the trained trees
predict(classifyRF, newdata = new_data[,1:4])
# 2          4         10         11         12         14 
#setosa     setosa     setosa     setosa     setosa     setosa 

#It got two wrong: 120 versicolor and 135 versicolor 

#Another version of the randomForest() command
anotherRF = randomForest(Species ~ ., 
                         data = train_data, ntree = 500)

#
#
#R plot Fig. 9.10: RF regression for ozone data
library(randomForest)
airquality[1:2,] #use R's RF benchmark data "airquality"
#  Ozone Solar.R Wind Temp Month Day
#1    41     190  7.4   67     5   1
#2    36     118  8.0   72     5   2
dim(airquality)
#[1] 153   6
ozoneRFreg = randomForest(Ozone ~ ., data = airquality, 
                          mtry = 2, ntree = 500, importance = TRUE, 
                          na.action = na.roughfix)
#na.roughfix allows NA to be replaced by medians 
#to begin with when training the RF trees
ozonePred = ozoneRFreg$predicted #RF regression result
t0 = 1:153
n1 = which(airquality$Ozone > 0) #positions of data
n0 = t0[-n1] #positions of missing data
ozone_complete = ozone_filled= airquality$Ozone 
ozone_complete[n0] = ozonePred[n0] #filled by RF
ozone_filled = ozonePred #contains the RF reg result
ozone_filled[n1] <- NA #replace the n1 positions by NA 
t1 = seq(5, 10, len = 153) #determine the time May - Sept

par(mfrow = c(2, 1))
par(mar = c(3, 4.5, 2, 0.1))
plot(t1, airquality$Ozone,   
     type = 'o', pch = 16, cex = 0.5, ylim = c(0, 170), 
     xlab = '', ylab = 'Ozone [ppb]', xaxt="n",
     main = '(a) Ozone data: Observed (black) and RF filled (blue)', 
     col = 1, cex.lab = 1.3, cex.axis = 1.3) 
MaySept = c("May","Jun", "Jul", "Aug", "Sep")
axis(side=1, at=5:9, labels = MaySept, cex.axis = 1.3)
points(t1, ozone_filled, col = 'blue', 
       type = 'o', pch = 16, cex = 0.5)#RF filled data

#Plot the complete data
par(mar = c(3, 4.5, 2, 0.1))
plot(t1, ozone_complete, 
     type = 'o', pch = 16, cex = 0.5, ylim = c(0, 170), 
     xlab = '', ylab = 'Ozone [ppb]', 
     xaxt="n", col = 'brown', 
     main = '(b) RF-filled complete ozone data series', 
     cex.lab = 1.3, cex.axis = 1.3)
MaySept = c("May","Jun", "Jul", "Aug", "Sep")
axis(side=1, at=5:9, labels = MaySept, cex.axis = 1.3)


#R plot of Fig. 9.11: A tree in a random forest 
#install.packages('rpart')
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)

setwd('~/climstats')
setEPS() #Plot the data of 150 observations
postscript("fig0911.eps",  width=6, height=6)
par(mar = c(0, 2, 1, 1))
iris_tree = rpart( Species ~. , data = train_data)
rpart.plot(iris_tree, 
           main = 'An decision tree for the RF training data')
dev.off()

#
#
#R code for NN recruitment decision and Fig. 9.12
TKS = c(20,10,30,20,80,30)
CSS = c(90,20,40,50,50,80)
Recruited = c(1,0,0,0,1,1)
# Here, you will combine multiple columns or features into a single set of data
df = data.frame(TKS, CSS, Recruited)

require(neuralnet) # load 'neuralnet' library
# fit neural network
set.seed(123)
nn = neuralnet(Recruited ~ TKS + CSS, data = df, 
               hidden=5, act.fct = "logistic",
               linear.output = FALSE)
plot(nn) #Plot Fig 9.12: A neural network

TKS=c(30,51,72) #new data for decision
CSS=c(85,51,30) #new data for decision
test=data.frame(TKS,CSS)
Predict=neuralnet::compute(nn,test)
Predict$net.result #the result is probability
#[1,] 0.99014936
#[2,] 0.58160633
#[3,] 0.01309036

# Converting probabilities into decisions
ifelse(Predict$net.result > 0.5, 1, 0) #threshold = 0.5
#[1,]    1
#[2,]    1
#[3,]    0

#print bias and weights
nn$weights[[1]][[1]]
#print the last bias and weights before decision
nn$weights[[1]][[2]]
#print the random start bias and weights
nn$startweights 
#print error and other technical indices of the nn run
nn$result.matrix #results data


#
#
#R plot Fig. 9.13: Curve of a logistic function
z = seq(-2, 4, len = 101)
k = 3.2
z0 = 1
setEPS() #Automatically saves the .eps file
postscript("fig0913.eps", height=5, width=7)
par(mar = c(4.2, 4.2, 2, 0.5))
plot(z, 1/(1 + exp(-k*(z - z0))),
     type = 'l', col = 'blue', lwd =2,
     xlab = 'z', ylab = 'g(z)',
main = bquote('Logistic function for k = 3.2,'~z[0]~'= 1.0'),
     cex.lab = 1.3, cex.axis = 1.3)
dev.off()

#
#
#R NN code for the Fisher iris flower data
#Ref: https://rpubs.com/vitorhs/iris 
data(iris) #150-by-5 iris data
#attach True or False columns to iris data
iris$setosa = iris$Species == "setosa" 
iris$virginica = iris$Species == "virginica"
iris$versicolor = iris$Species == "versicolor"
p = 0.5 # assign 50% of data for training
train.idx = sample(x = nrow(iris), size = p*nrow(iris))
train = iris[train.idx,] #determine the training data
test = iris[-train.idx,] #determine the test data
dim(train) #check the train data dimension
#[1] 75  8

#training a neural network
library(neuralnet)
#use the length, width, True and False data for training 
iris.nn = neuralnet(setosa + versicolor + virginica ~ 
                      Sepal.Length + Sepal.Width + 
                      Petal.Length + Petal.Width, 
                    data = train, hidden=c(10, 10), 
                    rep = 5, err.fct = "ce", 
                    linear.output = F, lifesign = "minimal", 
                    stepmax = 1000000, threshold = 0.001)

plot(iris.nn, rep="best") #plot the neural network

#Prediction for the rest data
prediction = neuralnet::compute(iris.nn, test[,1:4])
#prediction$net.result is 75-by-3 matrix
prediction$net.result[1:3,] #print the first 3 rows
#1    1 7.882440e-13 2.589459e-42
#2    1 7.890670e-13 2.586833e-42
#6    1 7.848803e-13 2.600133e-42
#The largest number in a row indicates species

#find which column is for the max of each row
pred.idx <- apply(prediction$net.result, 1, which.max)
pred.idx[70:75] #The last 6 rows
#140 142 144 148 149 150 
#3   3   3   3   3   3 

#Assign 1 for setosa, 2 for versicolor, 3 for virginica
predicted <- c('setosa', 'versicolor', 'virginica')[pred.idx]
predicted[1:6] #The prediction result
#[1] "setosa" "setosa" "setosa" "setosa" "setosa" "setosa"

#Create confusion matrix: table(prediction,observation)
table(predicted, test$Species)
#predicted    setosa versicolor virginica
#setosa         27          0         0
#versicolor      0         19         2
#virginica       0          1        26

