
setwd("C:/Documents and Settings/cm/Desktop")

ch_gprs1 = read.table("home_gprs1.txt", header = TRUE, sep=",")
ch_gprs2 = read.table("home_gprs2.txt", header = TRUE, sep=",")
ch_gprs3 = read.table("home_gprs3.txt", header = TRUE, sep=",")
ch_gprs4 = read.table("home_gprs4.txt", header = TRUE, sep=",")

ch_gprs_a = rbind(ch_gprs1, ch_gprs2, ch_gprs3, ch_gprs4)

ch_gprs_a = read.table("lab_laptop_gprs.txt", header = TRUE, sep=",")

#look at a limited time range
ch_gprs_ab = ch_gprs_a[ch_gprs_a$time_epoch > 1180300000,]
ch_gprs_ab = ch_gprs_ab[ch_gprs_ab$time_epoch < 1180380000,]
ch_gprs = ch_gprs_ab

ch_gprs = ch_gprs_a

ch1k = ch_gprs[ch_gprs$file_size_bytes == 1024,]
ch10k = ch_gprs[ch_gprs$file_size_bytes == 10240,]
ch50k = ch_gprs[ch_gprs$file_size_bytes == 51200,]
ch100k = ch_gprs[ch_gprs$file_size_bytes == 102400,]

plot(ch100k$time_epoch, ch100k$time_download, col="black", ylim=c(0,25))
points(ch50k$time_epoch, ch50k$time_download, col="blue")
points(ch10k$time_epoch, ch10k$time_download, col="green")
points(ch1k$time_epoch, ch1k$time_download, col="red")

#must use standardized breaks so that they can all appear together
minrange=0
maxrange=25
sbreaks=seq(minrange,maxrange,by=.2)

#but hist will choke if there is data outside that range
q1k=ch1k[ch1k$time_download < maxrange,]
q10k=ch10k[ch10k$time_download < maxrange,]
q50k=ch50k[ch50k$time_download < maxrange,]
q100k=ch100k[ch100k$time_download < maxrange,]

z1= hist(q1k$time_download, breaks=sbreaks, include.lowest=TRUE)
z2= hist(q10k$time_download, breaks=sbreaks, include.lowest=TRUE)
z3= hist(q50k$time_download, breaks=sbreaks, include.lowest=TRUE)
z4= hist(q100k$time_download, breaks=sbreaks, include.lowest=TRUE)

#"histogram" with everything at once
library(gplots)
x1=z1$counts
x2=z2$counts
x3=z3$counts
x4=z4$counts

#make everything the same length
max_len = max(length(x1), length(x2), length(x3), length(x4))
x1a=append(x1, vector(mode="double", max_len-length(x1)))
x2a=append(x2, vector(mode="double", max_len-length(x2)))
x3a=append(x3, vector(mode="double", max_len-length(x3)))
x4a=append(x4, vector(mode="double", max_len-length(x4)))

x=rbind(x1a, x2a, x3a, x4a)

barplot2(x)


################date and time objects
date.and.time = matrix(unlist(strsplit(as.character(ch100k$time_date)," ")), ncol = 2, byrow=TRUE)
chrons = chron(date.and.time[,1], date.and.time[,2], format=c(dates="y-m-d", times="h:m:s"))

plot(chrons, ch100k$time_download, col="black", ylim=c(0,25), xaxt='n')
axis(1,ticks,as.character(chron(ticks, out.format=c('m/d/y', 'h:m:s'))))


