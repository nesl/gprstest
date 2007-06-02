setwd("C:/cygwin/home/adparker/repositories/gprstest/trunk")
source("report_util.r")
setwd("C:/cygwin/home/adparker/repositories/gprstest/trunk/data")
## Read in a bunch of data

ahome1 <- get.split.table("ahome.txt", start.chron=13643.45, end.chron = 13646.42) # Andrew Home GPRS
ahome2 <- get.split.table("ahome.txt", start.chron=13658.62, end.chron=13663.80)
ahome3 <- get.split.table("ahome.txt",start.chron=13663.80, end.chron=Inf)
alaptop <- get.split.table("alaptop.txt") ## Andrew Home DSL 
censlab <- get.split.table("censlab.txt")  ## Andrew's Desk
censlab515 <- get.split.table("censlab_5_15_static.txt") ## CENS 5/15
#censlab516 <- get.split.table("censlab_5_16.txt")  # CENS Lab 5/16
censlab522 <- get.split.table("censlab_5_22.txt") ## CENS Lab 5/22
censlabwifi <- get.split.table("censlabwifi.txt") ## CENS Lab WIFI
chome <- get.split.table("chris_home_gprs.txt") ## Chris Home GPRS
lablaptopgrps <- get.split.table("lab_laptop_gprs.txt") # lab laptop gprs
labwifi1k <- get.split.table("lab_wifi_test_1k.txt") # wifi test 1k
valley515 <- get.split.table("valleydata_5_15.txt") # Valley 5/15
valley520 <- get.split.table("valley_5_20.txt") ## Valley 5/20
ww516 <- get.split.table("ww_5_16.txt") ## Westwood 5/16
ww517 <- get.split.table("ww_5_17.txt")## Westwood 5/17
ww518 <- get.split.table("ww_5_18.txt") ## Westwood 5/18


t <- rbind.tables(ahome1, ahome2)
t <- rbind.tables(t, ahome3)
attach(t$k10)
boxplot(time_download ~ weekday, ylim = c(0,30))
boxplot(time_download ~ hour+weekday, ylim = c(0,30))
boxplot(time_download ~ hour, ylim = c(0,30))
boxplot(time_download ~ minute, ylim = c(0,30))

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
boxplot(time_download/3 ~ hour, add=T,col="red")
boxplot(signal_bars ~ hour)
boxplot(time_download/2 ~ hour, add=T,col="red")

## printing other stuff
plot(ahome2$k1$chron, ahome2$k1$time_download, ylim=c(0,15))
points(ahome2$k100$chron, ahome2$k100$time_download, col="red")
points(ahome2$k10$chron, ahome2$k10$time_download, col="blue")
points(ahome2$k50$chron, ahome2$k50$time_download, col="green")
points(ahome2$k1$chron, ahome2$k1$signal_dbm - 80, col="orange")

points(ahome3$k50$chron, ahome3$k50$time_download, col="purple")
points(ahome3$k10$chron, ahome3$k10$time_download, col="orange")
points(ahome3$k1$chron, ahome3$k1$time_download, col="brown")


