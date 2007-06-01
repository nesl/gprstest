setwd("C:/cygwin/home/adparker/repositories/gprstest/trunk/data")
library(chron)

get.table <- function(table.name) {
  table <- read.table(table.name,header=TRUE,sep=",")
  date.and.time = matrix(unlist(strsplit(as.character(table$time_date)," ")), ncol = 2, byrow=TRUE)
  chrons = chron(date.and.time[,1], date.and.time[,2],  format=c(dates="y-m-d", times="h:m:s"))
  table <- transform(table, chron=chrons)
  return(table)
}

split.table <- function(t, start.chron = 0, end.chron = Inf) {
  t <- t[t$chron >= start.chron && t$chron <= end.chron]
  t1k <- t[t$file_size_bytes == 10240,]
  t10k <- t[t$file_size_bytes == 10240,]
  t50k <- t[t$file_size_bytes == 51200,]
  t100k <- t[t$file_size_bytes == 102400,]
  return(list(t1k=t1k,t10k=t10k,t50k=t50k,t100k=t100k))

}
## Read in Andrew Home Data
df <- get.table("ahome.txt")
ahome1 <- split.table(df, 13643.45, 13646.42)
ahome2 <- split.table(df, 13658.62, 13664.36)

## Read in CENS wifi
df <- get.table("censwifi.txt")
censwifi <- split.table(df)

## Read in Andrew Home LAPTOP Data
df <- get.table("alaptop.txt")
alaptop <- split.table(df)

## Read in Valley 5/20
df <- get.table("valley_5_20.txt")
valley520 <- split.table(df)

## Read in Westwood 5/16
df <- get.table("ww_5_16.txt")
ww516 <- split.table(df)

## Read in Westwood 5/17
df <- get.table("ww_5_17.txt")
ww517 <- split.table(df)

## Read in Westwood 5/18
df <- get.table("ww_5_18.txt")
ww518 <- split.table(df)




plot(ahome1$t1k$chron, ahome1$t1k$time_download)

attach(ahome1)
plot(t1k$chron, t1k$time_download)
detach(ahome1)
