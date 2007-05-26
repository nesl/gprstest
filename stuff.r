setwd("C:/cygwin/home/adparker/repositories/gprstest/trunk/data")
files <- list.files()
d <- data.frame()
for (fname in files) {
  d <- rbind(d, read.table(fname, header=TRUE, sep=","))
}
d1k <- d[d$file_size_bytes == 1024,]
d10k <- d[d$file_size_bytes == 10240,]
d50k <- d[d$file_size_bytes == 51200,]
d100k <- d[d$file_size_bytes == 102400,]
plot(d1k$time_epoch, d1k$time_download)
plot(d10k$time_epoch, d10k$time_download)

plot(d50k$time_epoch[d50k$time_download < 20,], d50k$time_download[d50k$time_download < 20,])
plot(d100k$time_epoch, d100k$time_download)


points(d1k$time_epoch, d1k$time_download, col="red")


##
plot(d1k$time_epoch, d1k$time_download)
foo <- d50k[d50k$time_download < 20,]
points(foo$time_epoch, foo$time_download, col="red")
points(d100k$time_epoch, d100k$time_download, col="blue")
points(d10k$time_epoch, d10k$time_download, col="green")

###
cwt1 <- read.table("censwifi1.txt", header=TRUE, sep=",")
cwt2 <- read.table("censwifi2.txt", header=TRUE, sep=",")
cwt <- rbind(cwt1, cwt2)


plot(cwt$time_epoch, cwt$time_download)
points(cwt1$time_epoch, cwt1$time_download, col="red")


cwt1k <- cwt[cwt$file_size_bytes == 1024,]
cwt10k <- cwt[cwt$file_size_bytes == 10240,]
cwt50k <- cwt[cwt$file_size_bytes == 51200,]
cwt100k <- cwt[cwt$file_size_bytes == 102400,]


plot(cwt1k$time_epoch, cwt1k$time_download, col="red")
points(cwt100k$time_epoch, cwt100k$time_download, col="blue")
points(cwt50k$time_epoch, cwt50k$time_download, col="blue")
points(cwt10k$time_epoch, cwt10k$time_download, col="green")


lines(cwt1k$time_epoch, cwt1k$time_download, col="red", xlim=c(cwt1k$time_epoch[1], cwt1k$time_epoch[1] + 3600))


xlim.start <- 1179510255+2000
xlim.end <- xlim.start + 80000 
xlim <-  c(xlim.start, xlim.end)
ylim <- c(0,3)
foo <- d1k
plot(foo$time_epoch, foo$time_download, col="red", xlim = xlim, ylim = ylim)
lines(foo$time_epoch, foo$time_download, col="red", xlim = xlim, ylim = ylim)

hist(foo$time_download,100)

## plot signal strength
plot(foo$time_epoch, foo$time_signal_strength, col="red", xlim = xlim)


### home data

home1 <- read.table("home.txt", header=TRUE, sep=",")
home2 <- read.table("home2.txt", header=TRUE, sep=",")
home <- rbind(home1, home2)

home1k <- home[home$file_size_bytes == 1024,]
home10k <- home[home$file_size_bytes == 10240,]
home50k <- home[home$file_size_bytes == 51200,]
home100k <- home[home$file_size_bytes == 102400,]

plot(home1k$time_epoch, home1k$time_download, col="black", ylim=c(0,20))
points(home100k$time_epoch, home100k$time_download, col="blue")
points(home50k$time_epoch, home50k$time_download, col="red")
points(home10k$time_epoch, home10k$time_download, col="green")
points(home$time_epoch, home$signal_bars, col="yellow")

plot.new()
plot(home$time_epoch, home$signal_dbm/10, col="blue")
points(home50k$time_epoch, home50k$time_download, col="red", ylim=c(0,15))


### valley 5_20

valley1 <- read.table("valley1_5_20.txt", header=TRUE, sep=",")
valley2 <- read.table("valley2_5_20.txt", header=TRUE, sep=",")
valley <- rbind(valley1, valley2)

valley1k <- valley[valley$file_size_bytes == 1024,]
valley10k <- valley[valley$file_size_bytes == 10240,]
valley50k <- valley[valley$file_size_bytes == 51200,]
valley100k <- valley[valley$file_size_bytes == 102400,]

plot(valley1k$time_epoch, valley1k$time_download, col="black", ylim=c(0,20))
points(valley100k$time_epoch, valley100k$time_download, col="blue")
points(valley50k$time_epoch, valley50k$time_download, col="red")
points(valley10k$time_epoch, valley10k$time_download, col="green")
points(valley$time_epoch, valley$signal_dbm/10, col="yellow")

plot.new()
plot(valley$time_epoch, valley$signal_dbm/10, col="blue")
points(valley50k$time_epoch, valley50k$time_download, col="red", ylim=c(0,15))

mean(valley50k$time_download)
var(valley50k$time_download)

### west wood 5 16

ww516 <- read.table("ww1_5_16.txt", header=TRUE, sep=",")


ww5161k <- ww516[ww516$file_size_bytes == 1024,]
ww51610k <- ww516[ww516$file_size_bytes == 10240,]
ww51650k <- ww516[ww516$file_size_bytes == 51200,]
ww516100k <- ww516[ww516$file_size_bytes == 102400,]

plot(ww5161k$time_epoch, ww5161k$time_download, col="black", ylim=c(0,20))
points(ww516100k$time_epoch, ww516100k$time_download, col="blue")
points(ww51650k$time_epoch, ww51650k$time_download, col="red")
points(ww51610k$time_epoch, ww51610k$time_download, col="green")
points(ww516$time_epoch, ww516$signal_dbm/10, col="yellow")

plot.new()
plot(ww516$time_epoch, ww516$signal_dbm/10, col="blue")
points(ww51650k$time_epoch, ww51650k$time_download, col="red", ylim=c(0,15))


### west wood 5 17

ww517 <- read.table("ww1_5_17.txt", header=TRUE, sep=",")


ww5171k <- ww517[ww517$file_size_bytes == 1024,]
ww51710k <- ww517[ww517$file_size_bytes == 10240,]
ww51750k <- ww517[ww517$file_size_bytes == 51200,]
ww517100k <- ww517[ww517$file_size_bytes == 102400,]

plot(ww5171k$time_epoch, ww5171k$time_download, col="black", ylim=c(0,20))
points(ww517100k$time_epoch, ww517100k$time_download, col="blue")
points(ww51750k$time_epoch, ww51750k$time_download, col="red")
points(ww51710k$time_epoch, ww51710k$time_download, col="green")
points(ww517$time_epoch, ww517$signal_dbm/10, col="yellow")

plot.new()
plot(ww517$time_epoch, ww517$signal_dbm/10, col="blue")
points(ww51750k$time_epoch, ww51750k$time_download, col="red", ylim=c(0,15))

### west wood 5 18

ww518 <- read.table("ww1_5_18.txt", header=TRUE, sep=",")


ww5181k <- ww518[ww518$file_size_bytes == 1024,]
ww51810k <- ww518[ww518$file_size_bytes == 10240,]
ww51850k <- ww518[ww518$file_size_bytes == 51200,]
ww518100k <- ww518[ww518$file_size_bytes == 102400,]

plot(ww5181k$time_epoch, ww5181k$time_download, col="black", ylim=c(0,20))
points(ww518100k$time_epoch, ww518100k$time_download, col="blue")
points(ww51850k$time_epoch, ww51850k$time_download, col="red")
points(ww51810k$time_epoch, ww51810k$time_download, col="green")
points(ww518$time_epoch, ww518$signal_dbm/10, col="yellow")

plot.new()
plot(ww518$time_epoch, ww518$signal_dbm/10, col="blue")
points(ww51850k$time_epoch, ww51850k$time_download, col="red", ylim=c(0,15))

hist(ww50k$time_download, 100)

hist(ww51650k$time_download,100,ylim=c(0,100), xlim=c(0,15))
hist(ww51750k$time_download,100,ylim=c(0,100), xlim=c(0,15))
hist(ww51850k$time_download,100,ylim=c(0,100), xlim=c(0,15))


mean(ww51650k$time_download)
mean(ww51750k$time_download)
mean(ww51850k$time_download)


sd(ww51650k$time_download,100)
sd(ww51750k$time_download,100)
sd(ww51850k$time_download,100)

