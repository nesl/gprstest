library(chron)

get.table <- function(table.name) {
  table <- read.table(table.name,header=TRUE,sep=",")
  date.and.time <-
    matrix(unlist(strsplit(as.character(table$time_date)," ")),
           ncol = 2, byrow=TRUE)
  chrons <- chron(date.and.time[,1],
                  date.and.time[,2],
                  format=c(dates="y-m-d", times="h:m:s"))
  days <- cut(chrons,"day")
  weekday <- weekdays(chrons)
  hour <- hours(chrons)
  minute <- minutes(chrons)
  second <- seconds(chrons)
  
  table <- transform(table,
                     chron=chrons,
                     day=days,
                     weekday = weekday,
                     hour = hour,
                     minute = minute,
                     second = second)
  return(table)
}

split.table <- function(table, start.chron = 0, end.chron = Inf) {
  t <- table[start.chron <= table$chron & table$chron < end.chron,]
  k1 <- t[t$file_size_bytes == 1024,]
  k10 <- t[t$file_size_bytes == 10240,]
  k50 <- t[t$file_size_bytes == 51200,]
  k100 <- t[t$file_size_bytes == 102400,]
  return(list(k1=k1,k10=k10,k50=k50,k100=k100))

}

get.split.table <- function(table.name,
                            start.chron = 0,
                            end.chron = Inf) {
  t <- get.table(table.name)
  tt <- split.table(t, start.chron = start.chron, end.chron = end.chron)
  return (tt)
}

rbind.tables <- function(t1, t2) {
  
  k1 <- rbind(t1$k1, t2$k1)
  k10 <- rbind(t$k10, t2$k10)
  k50 <- rbind(t1$k50, t2$k50)
  k100 <- rbind(t1$k100, t2$k100)
  return(list(k1=k1,k10=k10,k50=k50,k100=k100))
}
