##########################################################
##                       CTmax                          ##
##                   Alexandra Hahn                     ##
##########################################################

setwd("C:/Users/A.Hahn/Documents/RStuff/masterarbeit")
Sys.setenv(LANG = "en")

library(ggplot2)
library(dplyr)

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
col1 <- assays[which(assays$ï..collection == "1"),]
col2 <- assays[which(assays$ï..collection == "2"),]
col3 <- assays[which(assays$ï..collection == "3"),]
col4 <- assays[which(assays$ï..collection == "4"),]
col5 <- assays[which(assays$ï..collection == "5"),]
poster <- assays[which(assays$ï..collection != "3"),]
poster2 <- poster[which(poster$ï..collection != "4"),]

} 

#####plots#####

#CTmax - all collections
ggplot(poster, aes(x=date_sampling, y=Ctmax, fill=treatment))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = position_jitterdodge(jitter.width = 0.2))+
  theme_light(base_size = 17)+
  scale_x_discrete(labels=c("2022-04-06" = "April", "2022-05-16" = " May", "2022-06-27" = " June" , "2022-07-19" = "July"))+
  scale_y_continuous(limits = c(23,36), n.breaks = 5)+
  xlab("Sampling time")+ ylab("Critical thermal maximum in °C")+
  theme(legend.position = "right")+
  scale_fill_manual(values = c("#96B48E","#CFD4EB","#D5968F"), name = "treatment")

#males and females - thermal tolerance
ggplot(poster,aes(x=treatment, y=Ctmax, fill=sex_confirmed )) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width=0.2))+
  facet_wrap(~date_sampling)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "right")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")


group <- interaction(assays$temperature,assays$sex_confirmed)
p <- ggplot(assays,aes(x=temperature, 
                       y=Ctmax,
                       color=sex_confirmed,
                       fill = sex_confirmed,
                       group = group)) +
  theme_light(base_size = 17)+
  ylab("Critical thermal maximum in °C")+
  xlab("Treatment temperature in °C")+
  theme(legend.position = "right")+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  geom_point()

p 
p + geom_smooth(method = "lm", col ="grey", aes(group = sex_confirmed))

#males and females - length
ggplot(assays, aes(y = length, x = Ctmax, col = sex_confirmed))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", col ="grey", aes(group = sex_confirmed, fill = sex_confirmed))+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  theme_light(base_size = 17)+
  xlab("Critical thermal maximum in °C")+
  ylab("Prosome length in µm")

ggplot(assays, aes(y = length, x = Ctmax, col = sex_confirmed))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", col ="grey", aes(group = sex_confirmed, fill = sex_confirmed))+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_x_continuous(limits = c(22.5, 31))+
theme_light(base_size = 17)+
  xlab("Critical thermal maximum in °C")+
  ylab("Prosome length in µm")





