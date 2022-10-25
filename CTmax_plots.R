##########################################################
##                       CTmax                          ##
##                   Alexandra Hahn                     ##
##########################################################

setwd("C:/Users/A.Hahn/Documents/RStuff/masterarbeit")
Sys.setenv(LANG = "en")

library(ggplot2)
library(dplyr)
library(wesanderson)

assays <- read.csv("~/RStuff/masterarbeit/assays.csv", sep=";", header = TRUE)
assays <- assays[-c(1, 59, 136, 246), ]#remove dead or inactive
assays <-  assays[!(is.na(assays$ï..collection)),]

#Run for prep 
{

# Adding columns based on tank side and mean length:
assays<- assays %>%
  mutate(tank_side = case_when(
    grepl("R", assays$ID) ~ "right",
    grepl("L", assays$ID) ~ "left"
  ))

assays$length <- (assays$length1+assays$length2+assays$length3)/3               #new column mean length in µm

# set as factor
assays$ï..collection <- factor(assays$ï..collection, levels =c("1", "2", "3", "4", "5"))
assays$treatment <- factor(assays$treatment, levels = c("wild", "cold", "warm"))
assays$generation <- factor(assays$generation, levels = c("parental", "f1"))
assays$sex_confirmed <- factor(assays$sex_confirmed, levels = c("f", "m"))
assays$tank_side <- factor(assays$tank_side, levels = c("left", "right"))
assays$position <- factor(assays$position, levels = c("side", "top"))

#subsets
females <- assays[which(assays$sex_confirmed == "f"),]
wild <- assays[which(assays$treatment == "wild"),]
poster <- assays[which(assays$ï..collection != "3"),]
poster2 <- poster[which(poster$ï..collection != "4"),]
data <- poster[which(poster$Ctmax < 32),]
data$mean2 <- as.factor(data$X2.week_mean)

} 

#####plots#####

#CTmax - all collections
ggplot(poster, aes(x=date_sampling, y=Ctmax, fill=treatment))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = position_jitterdodge(jitter.width = 0.2))+
  theme_light(base_size = 14)+
  scale_x_discrete(labels=c("2022-04-06" = "April", "2022-05-16" = " May", "2022-06-27" = " June" , "2022-07-19" = "July"))+
  scale_y_continuous(limits = c(23,36), n.breaks = 5)+
  xlab("Sampling time")+ ylab("Critical thermal maximum in °C")+
  theme(legend.position = "right")+
  scale_fill_manual(values = c("#96B48E","#CFD4EB","#D5968F"), name = "treatment")

ggplot(data, aes(x = mean2, y = Ctmax, fill = X2.week_mean))+
  geom_boxplot(outlier.shape = NA)+
  theme_light(base_size = 14)+
  scale_fill_gradientn(colors = wes_palette("Zissou1", type = "continuous"))+
  scale_x_discrete(labels =c("6.36" = "6.36 °C", "6.36" = "6.36 °C", "10.8" = "10.8 °C","11.44" = "11.44 °C","16.55" = "16.55 °C","17.9" = "17.9 °C" ,"18.11" = "18.11 °C"))+
  xlab("")+ ylab("Critical thermal maximum in °C")+
  labs(fill = "Developmental temperature in °C")+
  theme(legend.position = "bottom")
  
#males and females - thermal tolerance
ggplot(poster,aes(x=treatment, y=Ctmax, fill=sex_confirmed )) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width=0.2))+
  facet_wrap(~date_sampling)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme_light(base_size = 14)+
  theme(legend.position = "right")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")


group <- interaction(assays$temperature,assays$sex_confirmed)#with old temp measurements
p <- ggplot(assays,aes(x=temperature, 
                       y=Ctmax,
                       color=sex_confirmed,
                       fill = sex_confirmed,
                       group = group)) +
  theme_light(base_size = 14)+
  ylab("Critical thermal maximum in °C")+
  xlab("Treatment temperature in °C")+
  theme(legend.position = "right")+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  geom_point(aes(shape = generation), size = 2.75)
#"#FF8275","#8BA4FF" for darker colours
p 
p + geom_smooth(method = "lm", col ="grey", aes(group = sex_confirmed))

group_mean <- interaction(assays$daily_mean,assays$sex_confirmed)#with daily mean kimocc
p_mean <- ggplot(assays,aes(x=daily_mean, 
                       y=Ctmax,
                       color=sex_confirmed,
                       fill = sex_confirmed,
                       group = group)) +
  theme_light(base_size = 14)+
  ylab("Critical thermal maximum in °C")+
  xlab("Treatment temperature in °C")+
  theme(legend.position = "right")+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  geom_point(aes(shape = generation), size = 2.75)
#"#FF8275","#8BA4FF" for darker colours
p_mean 
pm <- p_mean + geom_smooth(method = "lm", col ="grey", aes(group = sex_confirmed))

group_1w <- interaction(assays$X1.week_mean,assays$sex_confirmed)#with 1-week mean
p_1w <- ggplot(assays,aes(x=X1.week_mean, 
                            y=Ctmax,
                            color=sex_confirmed,
                            fill = sex_confirmed,
                            group = group)) +
  theme_light(base_size = 14)+
  ylab("Critical thermal maximum in °C")+
  xlab("Treatment temperature in °C")+
  theme(legend.position = "right")+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  geom_point(aes(shape = generation), size = 2.75)

p_1w
p1 <- p_1w + geom_smooth(method = "lm", col ="grey", aes(group = sex_confirmed))

group_2w <- interaction(assays$X2.week_mean,assays$sex_confirmed)#with 2-week mean
p_2w <- ggplot(assays,aes(x=X2.week_mean, 
                          y=Ctmax,
                          color=sex_confirmed,
                          fill = sex_confirmed,
                          group = group)) +
  theme_light(base_size = 14)+
  ylab("Critical thermal maximum in °C")+
  xlab("Treatment temperature in °C")+
  theme(legend.position = "right")+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  geom_point(aes(shape = generation), size = 2.75)

p_2w
p2 <- p_2w + geom_smooth(method = "lm", col ="grey", aes(group = sex_confirmed))

library(gridExtra)
grid.arrange(pm, p1, p2)

#males and females - length
ggplot(assays, aes(y = length, x = Ctmax, col = sex_confirmed))+
  geom_point(aes(shape = generation), size = 2.75)+
  geom_smooth(method = "lm", col ="grey", aes(group = sex_confirmed, fill = sex_confirmed))+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  theme_light(base_size = 14)+
  xlab("Critical thermal maximum in °C")+
  ylab("Prosome length in µm")

ggplot(assays, aes(y = length, x = Ctmax, col = sex_confirmed))+
  geom_point(aes(shape = generation), size = 2.75)+
  geom_smooth(method = "lm", col ="grey", aes(group = sex_confirmed, fill = sex_confirmed))+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_x_continuous(limits = c(22.5, 31))+
theme_light(base_size = 14)+
  xlab("Critical thermal maximum in °C")+
  ylab("Prosome length in µm")

ggplot(assays, aes(y = length, x = X2.week_mean, col = sex_confirmed))+
  geom_point(aes(shape = generation), size = 2.75)+
  geom_smooth(method = "lm", col ="grey", aes(group = sex_confirmed, fill = sex_confirmed))+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_x_continuous()+
  theme_light(base_size = 14)+
  xlab("Acclimation temperature")+
  ylab("Prosome length in µm")


#SST and wild CTmax

sub <- read.csv("~/RStuff/masterarbeit/temperature/sub.csv", header = TRUE)
mycolors <- c("kimocc" = "#D5968F", "ctmax"= "#178B76")
sub$Date <-  as.Date(sub$Date)

Sys.setlocale("LC_TIME", "English")#set locale to English to avoid German months

ggplot(sub, aes(x=Date, y=Temperature, group=Origin, color=Origin)) +
  geom_point() +
  theme_light(base_size = 14)+ 
  xlab("")+
  scale_y_continuous(name="Sea Surface Temperature in °C", sec.axis = sec_axis(~ 0.58*.+20, name="Critical thermal maximum in °C")) +
  scale_color_manual(name="Origin", values = mycolors) +
  theme(
    axis.title.y = element_text(color = mycolors["kimocc"]),
    axis.text.y = element_text(color = mycolors["kimocc"]),
    axis.title.y.right = element_text(color = mycolors["ctmax"]),
    axis.text.y.right = element_text(color = mycolors["ctmax"]),
    legend.position = "none")

wild <- assays[which(assays$treatment == "wild"),]
cor.test(wild$Ctmax, wild$X2.week_mean, method = "spearman")
#strong correlation btw wild Ctmax and temperature, spearman's p = 0.771
#stonger than for "temperature" 
cor.test(wild$Ctmax, wild$X2.week_mean)# pearson's r = 0.725
