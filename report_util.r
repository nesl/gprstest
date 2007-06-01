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

