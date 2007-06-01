setwd("C:/cygwin/home/adparker/repositories/gprstest/trunk/data")

## Read in a bunch of data
df <- get.table("ahome.txt") # Andrew Home GPRS
ahome1 <- split.table(df, 13643.45, 13646.42)
ahome2 <- split.table(df, 13658.62, 13664.36)
alaptop <- split.table(get.table("alaptop.txt")) ## Andrew Home DSL 
censlab <- split.table(get.table("censlab.txt"))  ## Andrew's Desk
## CENS Lab 5/15 static
## two hours in late afternoon under static and optimal conditions
censlab515 <- split.table(get.table("censlab_5_15_static.txt"))
censlab516 <- split.table(get.table("censlab_5_16.txt"))  # CENS Lab 5/16
censlab522 <- split.table(get.table("censlab_5_22.txt")) ## CENS Lab 5/22
censlabwifi <- split.table(get.table("censlabwifi.txt")) ## CENS Lab WIFI
chome <- split.table(get.table("chris_home_gprs.txt")) ## Chris Home GPRS
lablaptopgrps <- split.table(get.table("lab_laptop_gprs.txt")) # lab laptop gprs
labwifi1k <- split.table(get.table("lab_wifi_test_1k.txt")) # wifi test 1k
valley515 <- split.table(get.table("valleydata_5_15.txt")) # Valley 5/15
valley520 <- split.table(get.table("valley_5_20.txt")) ## Valley 5/20
ww516 <- split.table(get.table("ww_5_16.txt")) ## Westwood 5/16
ww517 <- split.table(get.table("ww_5_17.txt"))## Westwood 5/17
ww518 <- split.table(get.table("ww_5_18.txt")) ## Westwood 5/18

plot(ahome1$t1k$chron, ahome1$t1k$time_download)

