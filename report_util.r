library(chron)
library(quantreg)

get.table <- function(table.name) {
  table <- read.table(table.name, header = TRUE,sep = ",")
  date.and.time <-
    matrix(unlist(strsplit(as.character(table$time_date), " ")),
           ncol = 2, byrow = TRUE)
  chrons <- chron(date.and.time[,1],
                  date.and.time[,2],
                  format = c(dates = "y-m-d",
                             times = "h:m:s"))
  days <- cut(chrons,"day")
  weekday <- weekdays(chrons)
  hour <- hours(chrons)
  minute <- minutes(chrons)
  second <- seconds(chrons)
  table <- transform(table,
                     chron = chrons,
                     day = days,
                     weekday = weekday,
                     hour = hour,
                     minute = minute,
                     second = second)
  return(table)
}

split.table <- function(table, start.chron = 0, end.chron = Inf) {
  t <- table[start.chron <= table$chron & table$chron < end.chron,]
  k1 <- add.resid(t[t$file_size_bytes == 1024,])
  k10 <- add.resid(t[t$file_size_bytes == 10240,])
  k50 <- add.resid(t[t$file_size_bytes == 51200,])
  k100 <- add.resid(t[t$file_size_bytes == 102400,])
  return(list(k1=k1,k10=k10,k50=k50,k100=k100))

}

add.resid <- function(table) {
  resids <- resid(rq(table$time_download ~ table$chron))
  table <- transform(table, resid = resids)
  return (table)
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


plot.all <- function(foo1, foo10, foo50, foo100, lx, ly, main) {
  plot(foo100, xlab = "Date", ylab = "Download time (seconds)", col="red", main=main, pch=19,ylim=c(0,11))
points(foo50, col = "blue", pch=19)
points(foo10, col = "green", pch=19)
points(foo1, col = "black", pch=19)
  legend(lx, ly, legend = rev(c("1k", "10k","50k","100k")), fill=rev(c("black","green","blue","red")), bg="white")
}
