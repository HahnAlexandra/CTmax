##########################################################
##                    Temperature                       ##
##                   Alexandra Hahn                     ##
##########################################################

Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "English")
setwd("C:/Users/A.Hahn/Documents/RStuff/masterarbeit/temperature")

library(ggplot2)
library(plyr)   
library(dplyr)

####heaters####

temp_r <- read.csv("~/RStuff/masterarbeit/temperature/temp_running.csv", sep = ";")

i = 1

lowout <- c()
highout <- c()
for(i in 1:max(temp_r$trial, na.rm=T)){
  temp1 <-temp_r[which(temp_r$trial == i),]
  temp1$difference <- c(0,diff(temp1$temperature))#adapt 
  low1 <- temp1[which(temp1$temperature < 22.5),]
  high1 <- temp1[which(temp1$temperature > 22.5),]
  lowout[i]<- mean(low1$difference)
  highout[i]<- mean(high1$difference)
  
}


temp_r$time_running <- as.time(temp_r$time_running)

hist(highout, "heating rate per 30 seconds")
abline(v = mean(highout),                       # Add line for mean
       col = "red",
       lwd = 3)
text(x = 0.05,                   
     y = 12,
     paste("Mean =", 0.057),
     col = "red",
     cex = 1)
abline(v = median(highout),                    # Add line for median
       col = "blue",
       lwd = 3)
text(x = 0.05,                 
     y = 11,
     paste("Median =", 0.058),
     col = "blue",
     cex = 1)

hist(lowout, xlab = "heating rate per 30 seconds")
abline(v = mean(lowout),                       # Add line for mean
       col = "red",
       lwd = 3)
text(x = 0.075,                   
     y = 25,
     paste("Mean =",0.105),
     col = "red",
     cex = 1)
abline(v = median(lowout),                    # Add line for median
       col = "blue",
       lwd = 3)
text(x = 0.075,                 
     y = 23,
     paste("Median =",0.106),
     col = "blue",
     cex = 1)


ggplot(temp_r, aes(x = time_running, y = temperature))+
  geom_point(size = 0.05)+
  theme_classic()+
  geom_hline(yintercept = 22.5, col = "#D5968F", size = 1.2)+
  scale_x_discrete(breaks = c("00:00:00", "02:00:00"))

difference <- c(0,diff(temp_r$temperature))

ggplot(temp_r, aes(x = temperature, y = difference))+
  geom_point(size = 0.05)+
  theme_classic()+
  geom_vline(xintercept = 22.5, col = "#D5968F", size = 1.2)+
  scale_y_continuous(limits = c(0,0.18))+
  scale_x_continuous(limits = c(5,37), n.breaks = 10)+
  geom_smooth()


####polarfuchs#####

library(wesanderson)

polarfuchs <- read.csv("~/RStuff/masterarbeit/polarfuchs/master.csv")
polarfuchs$Date <- paste(polarfuchs$Year, polarfuchs$Month, polarfuchs$Day, sep = "-")
polarfuchs$Date <- as.Date(polarfuchs$Date)


ggplot(polarfuchs, aes(x = Date, y = Pressure..db, color = Temp..degC))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradientn(colors = wes_palette("Zissou1", type = "continuous"))+
  theme_classic()+
  labs(title = "Water temperature - Polarfuchs",
    y = "Depth [m]", x = "Date", color = "Temperature [°C]")
 

p_exp <- read.csv("~/RStuff/masterarbeit/polarfuchs/exp/master.csv")
p_exp$Date <- paste(p_exp$Year, p_exp$Month, p_exp$Day, sep = "-")

p_exp$Date <- as.Date(p_exp$Date) 
  

t <- ggplot(p_exp, aes(x = Date, y = Pressure..db, color = Temp..degC))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradientn(colors = wes_palette("Zissou1", type = "continuous"))+
  theme_classic()+
  labs(title = "Water temperature at sampling dates",
       y = "Depth [m]", x = "Month", color = "Temp [°C]")


##salinity

s <- ggplot(p_exp, aes(x = Date, y = Pressure..db, color = SALIN..ppt))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradientn(colors = wes_palette("Zissou1", type = "continuous"))+
  theme_classic()+
  labs(title = "Salinity at sampling dates",
       y = "Depth [m]", x = "Month", color = "Salinity [ppt]")

library(gridExtra)
grid.arrange(t,s, ncol = 1, nrow = 2)


#temperature values
max<-aggregate (Temp..degC~Date, p_exp, FUN = max)
mean <- aggregate(Temp..degC~Date, p_exp, FUN = mean)
median <- aggregate(Temp..degC~Date, p_exp, FUN = median)


#salinity values

sal_maximum <- aggregate(SALIN..ppt~Date, p_exp, FUN = max)
sal_min <- aggregate(SALIN..ppt~Date, p_exp, FUN = min)
sal_mean <- aggregate(SALIN..ppt~Date, p_exp, FUN = mean)
sal_median <- aggregate(SALIN..ppt~Date, p_exp, FUN = median)


col_names <- c("Date", "Temp_max", "Temp_mean", "Temp_median", "Sal_max", "Sal_min", "Sal_mean")
table <- data.frame(matrix(ncol =7, nrow = 5))
colnames(table) <- col_names

table$Date <- c("4-6", "5-16", "6-13", "6-27", "7-19" )

#monthly temp

kimocc <- read.csv("~/RStuff/masterarbeit/temperature/KIMOCC.csv", header = TRUE)
#combine date and time
kimocc$DT <- paste(kimocc$Date, kimocc$Time, sep = " ")

kimocc$DT <- as.POSIXct(kimocc$DT)
kimocc$Date <- as.Date(kimocc$Date)
plot(x = kimocc$DT, y = kimocc$T..IPTS.90)

#install.packages("ggplotFL", repos="http://flr-project.org/R")

library(ggplotFL)
a <- ggplot(kimocc,aes(Date, T..IPTS.90)) +
  geom_flquantiles(probs=c(0.025, 0.50, 0.975), fill="red", alpha=0.25) + 
  scale_x_date(date_labels = "%b") +
  theme_classic() +
  xlab("") +
  ylab("Temperature")

a

#subset from March to July
final <-kimocc[kimocc$Date >= "2022-3-21" & kimocc$Date <= "2022-07-31",]
f <- ggplot(final,aes(Date, T..IPTS.90)) +
  geom_flquantiles(probs=c(0.025, 0.50, 0.975), fill="red", alpha=0.25) + 
  scale_x_date(date_labels = "%b") +
  theme_classic() +
  xlab("") +
  ylab("Temperature")

#add sampling dates
f + geom_vline(aes(xintercept = as.numeric(as.Date("2022-04-06"))), col = "black", linetype = 3)+
    geom_vline(aes(xintercept = as.numeric(as.Date("2022-05-16"))), col = "black", linetype = 3)+
    geom_vline(aes(xintercept = as.numeric(as.Date("2022-06-27"))), col = "black", linetype = 3)+
    geom_vline(aes(xintercept = as.numeric(as.Date("2022-07-19"))), col = "black", linetype = 3)






