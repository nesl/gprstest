setwd("C:/cygwin/home/adparker/repositories/gprstest/trunk")
source("report_util.r")
setwd("C:/cygwin/home/adparker/repositories/gprstest/trunk/data")
## Read in a bunch of data

ahome1 <- get.split.table("ahome.txt", start.chron=13643.45, end.chron = 13646.42) # Andrew Home GPRS
ahome2 <- get.split.table("ahome.txt", start.chron=13658.62, end.chron=13663.80)
ahome3 <- get.split.table("ahome.txt",start.chron=13663.80, end.chron=13664.26)
ahome4 <- get.split.table("ahome.txt",start.chron=13665.90, end.chron=13666.64)
ahome5 <- get.split.table("ahome.txt",start.chron=13666.69, end.chron=13667.56)
ahome6 <- get.split.table("ahome.txt",start.chron=13668.26, end.chron=Inf)
alaptop <- get.split.table("alaptop.txt") ## Andrew Home DSL 
censlab <- get.split.table("censlab.txt")  ## Andrew's Desk
censlab515 <- get.split.table("censlab_5_15_static.txt") ## CENS 5/15
#censlab516 <- get.split.table("censlab_5_16.txt")  # CENS Lab 5/16
censlab522 <- get.split.table("censlab_5_22.txt") ## CENS Lab 5/22
censlabwifi <- get.split.table("censlabwifi.txt") ## CENS Lab WIFI
chome <- get.split.table("chris_home_gprs.txt") ## Chris Home GPRS
lablaptopgprs <- get.split.table("lab_laptop_gprs.txt") # lab laptop gprs
labwifi1k <- get.split.table("lab_wifi_test_1k.txt") # wifi test 1k
valley515 <- get.split.table("valleydata_5_15.txt") # Valley 5/15
valley520 <- get.split.table("valley_5_20.txt") ## Valley 5/20
ww516 <- get.split.table("ww_5_16.txt") ## Westwood 5/16
ww517 <- get.split.table("ww_5_17.txt")## Westwood 5/17
ww518 <- get.split.table("ww_5_18.txt") ## Westwood 5/18



t <- rbind.tables(ahome1, ahome2)
t <- rbind.tables(t, ahome3)
t <- rbind.tables(t, ahome4)
t <- rbind.tables(t, ahome5)
t <- rbind.tables(t, ahome6)
attach(t$k100)
## Raw data
plot(time_download ~ chron)
boxplot(time_download ~ weekday, ylim = c(0,30))
boxplot(time_download ~ hour+weekday, ylim = c(0,30))
boxplot(time_download ~ hour, ylim = c(0,30))
boxplot(time_download ~ minute, ylim = c(0,30))


boxplot(resid ~ weekday, ylim = c(-5,10))
boxplot(resid ~ hour+weekday, ylim = c(-5,10))
boxplot(resid ~ hour, ylim = c(-5,10))
boxplot(resid ~ minute, ylim = c(-5,10))

hist(resid,500,xlim=c(min(resid),20))


foo <- lm(time_download ~ time_epoch)
summary(foo)
plot(foo)

plot(time_download ~ time_epoch)
abline(coef(foo),col="red",lw=5)

detach(t$k10)
## Plot stuff against signal strength (bars & dbm)
boxplot(ahome1$k100$time_download ~ ahome1$k100$signal_dbm)
boxplot(ahome1$k50$time_download ~ ahome1$k50$signal_dbm,add=T,col="red")
boxplot(ahome1$k100$time_download ~ ahome1$k100$signal_bars)
boxplot(ahome1$k50$time_download ~ ahome1$k50$signal_bars,add=T,col="red")

boxplot(signal_bars ~ hour)

plot(spline(signal_bars))
plot(spline(signal_bars) ~ hour)

boxplot(time_download/3 ~ hour, add=T,col="red")

boxplot(signal_bars ~ hour)
boxplot(resid/3 + 3 ~ hour, add=T,col="red")

## printing other stuff
plot(ahome2$k1$chron, ahome2$k1$time_download, ylim=c(0,15))
points(ahome2$k100$chron, ahome2$k100$time_download, col="red")
points(ahome2$k10$chron, ahome2$k10$time_download, col="blue")
points(ahome2$k50$chron, ahome2$k50$time_download, col="green")
points(ahome2$k1$chron, ahome2$k1$signal_dbm - 80, col="orange")

points(ahome3$k50$chron, ahome3$k50$time_download, col="purple")
points(ahome3$k10$chron, ahome3$k10$time_download, col="orange")
points(ahome3$k1$chron, ahome3$k1$time_download, col="brown")


     data(stackloss)
     rq(stack.loss ~ stack.x,.5)  #median (l1) regression  fit for the stackloss data. 
     rq(stack.loss ~ stack.x,.25)  #the 1st quartile, 
             #note that 8 of the 21 points lie exactly on this plane in 4-space! 
     rq(stack.loss ~ stack.x, tau=-1)   #this returns the full rq process
     rq(rnorm(50) ~ 1, ci=FALSE)    #ordinary sample median --no rank inversion ci
     rq(rnorm(50) ~ 1, weights=runif(50),ci=FALSE)  #weighted sample median 
     #plot of engel data and some rq lines see KB(1982) for references to data
     data(engel)
     attach(engel)
     plot(income,foodexp,xlab="Household Income",ylab="Food Expenditure",type = "n", cex=.5)
     points(income,foodexp,cex=.5,col="blue")
     taus <- c(.05,.1,.25,.75,.9,.95)
     xx <- seq(min(income),max(income),100)
     f <- coef(rq((foodexp)~(income),tau=taus))
     yy <- cbind(1,xx)%*%f
     for(i in 1:length(taus)){
             lines(xx,yy[,i],col = "gray")
             }
     abline(lm(foodexp ~ income),col="red",lty = 2)
     abline(rq(foodexp ~ income), col="blue")
     legend(3000,500,c("mean (LSE) fit", "median (LAE) fit"),
             col = c("red","blue"),lty = c(2,1))
     #Example of plotting of coefficients and their confidence bands
     plot(summary(rq(foodexp~income,tau = 1:49/50,data=engel)))
     #Example to illustrate inequality constrained fitting
     n <- 100
     p <- 5
     X <- matrix(rnorm(n*p),n,p)
     y <- .95*apply(X,1,sum)+rnorm(n)
     #constrain slope coefficients to lie between zero and one
     R <- cbind(0,rbind(diag(p),-diag(p)))
     r <- c(rep(0,p),-rep(1,p))
     rq(y~X,R=R,r=r,method="fnc")

