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


hist(highout, xlab = "heating rate per 30 seconds")
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

polarfuchs <- read.csv("~/RStuff/masterarbeit/temperature/polarfuchs/master.csv")
polarfuchs$Date <- paste(polarfuchs$Year, polarfuchs$Month, polarfuchs$Day, sep = "-")
polarfuchs$Date <- as.Date(polarfuchs$Date)


ggplot(polarfuchs, aes(x = Date, y = Pressure..db, color = Temp..degC))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradientn(colors = wes_palette("Zissou1", type = "continuous"))+
  theme_classic()+
  labs(title = "Water temperature - Polarfuchs",
    y = "Depth [m]", x = "Date", color = "Temperature [°C]")
 

p_exp <- read.csv("~/RStuff/masterarbeit/temperature/polarfuchs/exp/master.csv")
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



####kimocc###
kimocc <- read.csv("~/RStuff/masterarbeit/temperature/KIMOCC.csv", header = TRUE)
#combine date and time
kimocc$DT <- paste(kimocc$Date, kimocc$Time, sep = " ")

kimocc$DT <- as.POSIXct(kimocc$DT)
kimocc$Date <- as.Date(kimocc$Date)

#install.packages("ggplotFL", repos="http://flr-project.org/R")

library(ggplotFL)
a <- ggplot(kimocc,aes(Date, T..IPTS.90)) +
  geom_flquantiles(probs=c(0.025, 0.50, 0.975), fill="red", alpha=0.25) + 
  scale_x_date(date_labels = "%b") +
  theme_light(base_size = 14) +
  xlab("") +
  ylab("SST in °C")

a

#subset from March to July
final <-kimocc[kimocc$Date >= "2022-3-21" & kimocc$Date <= "2022-07-31",]
f <- ggplot(final,aes(Date, T..IPTS.90)) +
  geom_flquantiles(probs=c(0.025, 0.50, 0.975), fill="red", alpha=0.25) + 
  theme_light(base_size = 14)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab("") +
  ylab("SST in °C")
f
#add sampling dates
f + geom_vline(aes(xintercept = as.numeric(as.Date("2022-04-06"))), col = "black", linetype = 3, size = 1)+
    geom_vline(aes(xintercept = as.numeric(as.Date("2022-05-16"))), col = "black", linetype = 3, size = 1)+
    geom_vline(aes(xintercept = as.numeric(as.Date("2022-06-27"))), col = "black", linetype = 3, size = 1)+
    geom_vline(aes(xintercept = as.numeric(as.Date("2022-07-19"))), col = "black", linetype = 3, size = 1)+
    geom_vline(aes(xintercept = as.numeric(as.Date("2022-06-13"))), col = "darkgrey", linetype = 3, size = 1)

rects <- data.frame(
  name = c('col1', 'col2', 'col3', 'col4', 'col5'),
  start = c("2022-03-23", "2022-05-02","2022-05-30", "2022-06-13", "2022-07-05"),
  end = c("2022-04-06", "2022-05-16", "2022-06-13", "2022-06-27", "2022-07-19")
)
rects$start <- as.Date(rects$start)
rects$end <- as.Date(rects$end)

curve <- f + geom_rect(data = rects, inherit.aes=FALSE, mapping=aes(xmin = start, xmax = end,
                                                       ymin = -Inf, ymax = Inf, fill = name), alpha = 0.35)+
  scale_fill_manual(values = alpha(c("#3B9AB2", "#D5C660", "#E9C624", "#EC7B00", "#F21A00")))+
  theme(legend.position = "none")

x11()
grid.arrange(curve, box, ncol = 2, nrow = 1)# box from CTmax_plots.R

#####table stuff####
#daily average
mean(final$T..IPTS.90[which(final$Date == "2022-04-06")] )#5.81
mean(final$T..IPTS.90[which(final$Date == "2022-05-16")] )#12.66
mean(final$T..IPTS.90[which(final$Date == "2022-06-27")] )#18.35
mean(final$T..IPTS.90[which(final$Date == "2022-07-19")] )#19.16
mean(final$T..IPTS.90[which(final$Date == "2022-06-13")] )#12.37

#1-week average

mean(final$T..IPTS.90[which(final$Date >= "2022-03-30" & final$Date <= "2022-04-06")])#6.09
mean(final$T..IPTS.90[which(final$Date >= "2022-05-09" & final$Date <= "2022-05-16")])#11.29
mean(final$T..IPTS.90[which(final$Date >= "2022-06-20" & final$Date <= "2022-06-27")])#17.78
mean(final$T..IPTS.90[which(final$Date >= "2022-07-10" & final$Date <= "2022-07-19")])#18.50
mean(final$T..IPTS.90[which(final$Date >= "2022-06-06" & final$Date <= "2022-06-13")])#11.99

#2-week average
mean(final$T..IPTS.90[which(final$Date >= "2022-03-23" & final$Date <= "2022-04-06")])#6.36
mean(final$T..IPTS.90[which(final$Date >= "2022-05-02" & final$Date <= "2022-05-16")])#11.44
mean(final$T..IPTS.90[which(final$Date >= "2022-06-13" & final$Date <= "2022-06-27")])#16.55
mean(final$T..IPTS.90[which(final$Date >= "2022-07-03" & final$Date <= "2022-07-19")])#18.11
mean(final$T..IPTS.90[which(final$Date >= "2022-05-30" & final$Date <= "2022-06-13")])#12.86
