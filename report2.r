setwd("C:/cygwin/home/adparker/repositories/gprstest/trunk")
source("report_util.r")
setwd("C:/cygwin/home/adparker/repositories/gprstest/trunk/data")

censlab <- get.split.table("censlab.txt")  ## Andrew's Desk
attach(censlab$k1)

# 1k black
# 10k green
# 50k blue
# 100k red 
# signal_db "tan"

## SLIDE OUR FIRST EXPERIMENTS
start <- chron("5/14/07", "11:16:48")
end <- chron("5/14/07" , "12:16:48")
plot(time_download[chron >= start & chron < end] ~ chron[chron >= start & chron < end], xlab = "Date", ylab = "Download time (seconds)", col="black", main="1 Kilobyte Test", pch=19)

## SLIDE Multi-file test
plot(censlab$k100$time_download[chron >= start & chron < end] ~ censlab$k100$chron[chron >= start & chron < end], xlab = "Date", ylab = "Download time (seconds)", col="red", main="Multi-size test", pch=19,ylim=c(0,11))

points(censlab$k1$time_download ~ censlab$k1$chron, col = "black", pch=19)
points(censlab$k10$time_download ~ censlab$k10$chron, col = "green", pch=19)
points(censlab$k50$time_download ~ censlab$k50$chron, col = "blue", pch=19)
legend(13647.47, 10.2981, legend = c("1k", "10k","50k","100k"), fill=c("black","green","blue","red"))

## SLIDE over a long period of time, show all files
start <- chron(13651.44)
end <- chron(13653.0)
foo1 <- censlab$k1$time_download[chron >= start & chron < end] ~ censlab$k1$chron[chron >= start & chron < end]
foo10 <- censlab$k10$time_download[chron >= start & chron < end] ~ censlab$k10$chron[chron >= start & chron < end]
foo50 <- censlab$k50$time_download[chron >= start & chron < end] ~ censlab$k50$chron[chron >= start & chron < end]
foo100 <- censlab$k100$time_download[chron >= start & chron < end] ~ censlab$k100$chron[chron >= start & chron < end]
plot.all(foo1,foo10,foo50,foo100,lx=start, ly = 15, main="Linear trend in download times", ylim=c(0,15))


## SLIDE least squares fit on 1k
form <- censlab$k1$time_download[chron >= start & chron < end] ~ censlab$k1$chron[chron >= start & chron < end]
plot(form, xlab = "Date", ylab = "Download time (seconds)", col="black",
     main="Linear fit to 1k data", pch=19,ylim=c(0,12))
abline(lm(form), lw=5, col="orange", lty = 2)
abline(rq(form), lw=5, col="blue", lty = 2)
legend(13651.46, 11.3, legend = c("1k",
                                  "Least squares fit (OLS)",
                                  "Least absolute deviation fit (LAD)"),
       fill=c("black", "orange", "blue"), bg = "white")


## SLIDE show residuls of OLS going down
plot(form, xlab = "Date", col="black",
     pch=19,ylim=c(-1,10), ylab="Residuals",
     main="Residuals of the different linear fits", type="n")
points(censlab$k1$chron[chron >= start & chron < end], residuals(lm(form)),  col="orange", pch=19)
points(censlab$k1$chron[chron >= start & chron < end], residuals(rq(form)), col="blue", pch=19)
legend(13651.46, 9.3, legend = c( "Least squares fit (OLS)",
                                  "Least absolute deviation fit (LAD)"),
       fill=c("orange", "blue"), bg = "white")

## SLIDE Table of coefs of least absolute deviation fit

for (i in c(foo1, foo10, foo50, foo100)) {
  print(summary(rq(i,tau=0.1)))
}


## SLIDE Linear trend is not a network phenomenon
valleygprs <- get.split.table("valley_gprs.csv") ## Valley GPRS
foo1 <- valleygprs$k1$time_download ~ valleygprs$k1$chron
foo10 <- valleygprs$k10$time_download ~ valleygprs$k10$chron
foo50 <- valleygprs$k50$time_download ~ valleygprs$k50$chron
foo100 <- valleygprs$k100$time_download ~ valleygprs$k100$chron
plot.all(foo1,foo10,foo50,foo100,lx=chron(13666.13), ly = 10, main="Trouble-Shooting (Laptop using phone as a modem)")


## Signal strength and diurnal variation
ahome <- get.split.table("ahome.txt")
ahome1 <- get.split.table("ahome.txt", start.chron=13643.67, end.chron = 13646.42) # Andrew Home GPRS
ahome2 <- get.split.table("ahome.txt", start.chron=13658.62, end.chron=13663.80)
ahome3 <- get.split.table("ahome.txt",start.chron=13663.80, end.chron=13664.26)
ahome4 <- get.split.table("ahome.txt",start.chron=13665.90, end.chron=13666.64)
ahome5 <- get.split.table("ahome.txt",start.chron=13666.69, end.chron=13667.56)
ahome6 <- get.split.table("ahome.txt",start.chron=13668.26, end.chron=Inf)
ww516 <- get.split.table("ww_5_16.txt",fix=T) ## Westwood 5/16
ww517 <- get.split.table("ww_5_17.txt")## Westwood 5/17
ww518 <- get.split.table("ww_5_18.txt") ## Westwood 5/18
lablaptopgprs <- get.split.table("lab_laptop_gprs.txt") # lab laptop gprs

sig.foo <- ahome2$k100$signal_dbm ~ ahome2$k100$chron
dl.foo <- ahome2$k100$resid ~ ahome2$k100$chron
plot(sig.foo, col="brown", pch = 19, ylim=c(0,100),
     xlab="Date",
     main = "Measurements at Residential Site 1", 
     ylab="Residual Download Time (seconds)                         Signal Strength (-dBm)")
points(dl.foo, col="red", pch = 19)
#lines(smooth.spline(dl.foo,df=20))
legend(13658.81, 77.95,
       legend = c("Signal Strength (-dBm)", "100k Download Time (seconds)"),
       fill = c("brown", "red"),
       bg = "white")

################
t <- rbind.tables(ahome2, ahome3)
t <- rbind.tables(t, ahome4)
t <- rbind.tables(t, ahome5)
t <- rbind.tables(t, ahome6)
t <- rbind.tables(t, ahome1)
t <- rbind.tables(t,ww516)
t <- rbind.tables(t,ww517)
t <- rbind.tables(t,ww518)
t <- rbind.tables(t,lablaptopgprs)
## Boxplot resid ~ hour+weekday
dl.hour.weekday.foo <- t$k100$resid ~ t$k100$hour+ t$k100$weekday
boxplot(dl.hour.weekday.foo,
        ylim = c(-5,25),
        xlab = "Hour of the Week",
        ylab = "Residual Download Time (seconds)",
        main = "Residuals vs. Hour over the Week at Residential Site 1")
## Boxplot resid ~ hour
#t <- rbind.tables(ahome1, ahome2)
dl.hour.foo <- t$k100$resid ~ t$k100$hour
boxplot(dl.hour.foo,ylim=c(-5,25), xlab = "Hour of Day", ylab = "Residual Download Time (seconds)", main = "")

##
 

## boxplot data as countours, Hour of day trends
bpl=boxplot(dl.hour.foo,ylim=c(-5,25), xlab = "Hour of Day", ylab = "Residual Download Time (seconds)", main = "")
plot(bpl$stats[3,], pch=19, ylim=c(-9.0, 11), main="Residual Download Times", xlab="Hour of the Day", ,ylab="Residual (seconds)", type="o")

plot(bpl$stats[3,], pch=19, ylim=c(-8.0, 11), main="Residual Download Times (100Kb file)", xlab="Hour of the Day", ylab="Residual (seconds)", type="o", xaxt="n")
axis(1,seq(1,24,1), as.character(seq(0,23,1)))
lines(bpl$stats[4,])
lines(bpl$stats[2,])
lines(bpl$stats[5,], col="gray")
lines(bpl$stats[1,], col="gray")
lines(bpl$stats[3,], col="red")
lines(bpl$conf[1,], col="blue")
lines(bpl$conf[2,], col="blue")
lines(bpl$stats[3,], col="red", lw="2")
points(bpl$stats[3,], pch=19)

legend(.5, 10.5, legend = c("Largest Non-outlier (< Median + 1.5*IQR)",
                                  "   3rd Quartile",
                                  "      Upper 95% Confidence Interval",
						"         Median",
						"      Lower 95% Confidence Interval",
						"   1st Quartile",
					"Smallest Non-outlier (> Median - 1.5*IQR)"),
       fill=c("gray", "black", "blue", "red", "blue", "black", "gray"), bg = "white")
