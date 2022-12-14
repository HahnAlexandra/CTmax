##########################################################
##                       CTmax                          ##
##                   Alexandra Hahn                     ##
##########################################################

setwd("C:/Users/A.Hahn/Documents/RStuff/masterarbeit")
Sys.setenv(LANG = "en")

library(ggplot2)
library(dplyr)
library(wesanderson)
library(cowplot)
library(gridExtra)

assays <- read.csv("~/RStuff/masterarbeit/assays.csv", sep=";", header = TRUE)
assays <- assays[-c(1, 59, 136, 246), ]#remove dead or inactive
assays <-  assays[!(is.na(assays$ï..collection)),]

data_species <- read.csv("~/RStuff/masterarbeit/data_species.csv", sep=";", header = TRUE)
data_species <- data_species[-c(1, 59, 136, 246), ]#remove dead or inactive
data_species <-  data_species[!(is.na(data_species$ï..collection)),]
data_species <- data_species[which(data_species$ï..collection != "3"),]


#Run for prep 
{

# Adding columns based on tank side and mean length:
assays<- assays %>%
  mutate(tank_side = case_when(
    grepl("R", assays$ID) ~ "right",
    grepl("L", assays$ID) ~ "left"
  ))

assays$length <- (assays$length1+assays$length2+assays$length3)/3  #new column mean length in µm
data_species$length <- (data_species$length1+data_species$length1+data_species$length1)/3
data_species$species <- as.factor(data_species$species)
data_species$generation <- factor(data_species$generation, levels = c("parental", "f1"))

# set as factor
assays$ï..collection <- factor(assays$ï..collection, levels =c("1", "2", "3", "4", "5"))
assays$treatment <- factor(assays$treatment, levels = c("wild", "cold", "warm"))
assays$generation <- factor(assays$generation, levels = c("parental", "f1"))
assays$sex_confirmed <- factor(assays$sex_confirmed, levels = c("f", "m"))
assays$tank_side <- factor(assays$tank_side, levels = c("left", "right"))
assays$position <- factor(assays$position, levels = c("side", "top"))

#subsets
assays$mean2 <- as.factor(assays$X2.week_mean)
poster <- assays[which(assays$ï..collection != "3"),]
data <- poster[which(poster$species == "hudsonica"),]
wild <- assays[which(assays$treatment == "wild"),]
wild <- wild[which(wild$species != "tonsa"),]

} 

#####plots#####

####species - CTmax/length####
ggplot(poster, aes(x = Ctmax, y = length, col = species))+
  geom_point(aes(shape = generation), size = 2.75)+
  scale_color_manual(values = c("#CFD4EB","#D5968F"), name = "species")+
  theme_light(base_size = 14)+
  xlab("Critical thermal maximum in °C")+
  ylab("Prosome length in µm")

ggplot(data_species, aes(x = Ctmax, y = length, col = species))+
  geom_point(aes(shape = generation), size = 2.75)+
  scale_color_manual(values = c("#CFD4EB","#D5968F","#333333", "#96B48E"), name = "species")+
  theme_light(base_size = 14)+
  xlab("Critical thermal maximum in °C")+
  ylab("Prosome length in µm")

ggplot(poster, aes(x = species, y = Ctmax, fill = species))+
  geom_boxplot()+
  scale_fill_manual(values = c("#CFD4EB","#D5968F"), name = "")+
  theme_light(base_size = 14)+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c("hudsonica" = "A. hudsonica", "tonsa" = "A. tonsa"))+
  xlab("Species")+
  ylab("Critical thermal maximum in °C")

####CTmax####
#CTmax - all collections
poster2 <- distinct(poster, date_sampling, treatment)%>%
  arrange(date_sampling, treatment)
poster2$yloc <- max(poster$Ctmax)+ .5
poster2$label <- c("a", "b", "cd", "bc", "a", "bcd", "de", "bc", "f", "ef", "bc", "g")#compact letter from Ctmax_stats

Ctmax_all <- ggplot(poster, aes(x=date_sampling, y=Ctmax, fill=treatment))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = position_jitterdodge(jitter.width = 0.2))+
  theme_light(base_size = 14)+
  scale_x_discrete(labels=c("2022-04-06" = "April", "2022-05-16" = " May", "2022-06-27" = " June" , "2022-07-19" = "July"))+
  scale_y_continuous(limits = c(23,36), n.breaks = 5)+
  xlab("Sampling time")+ ylab("Critical thermal maximum in °C")+
  theme(legend.position = "right")+
  scale_fill_manual(values = c("#96B48E","#CFD4EB","#D5968F"), name = "Treatment")
Ctmax_all

Ctmax_all +
  ylim(NA, max(poster$Ctmax)+ .5)+
  geom_text(data = poster2, aes(y = yloc, label = label),
            position = position_dodge(width = .75))

#Ctmax - outliers removed
data1a <- distinct(data, date_sampling, treatment)%>%
  arrange(date_sampling, treatment)
data1a$yloc <- max(data$Ctmax)+ .5
data1a$label <- c("a", "b", "c", "b", "a", "c", "c", "b", "c", "c", "b")#compact letter from Ctmax_stats

Ctmax_out <- ggplot(data, aes(x=date_sampling, y=Ctmax, fill=treatment))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = position_jitterdodge(jitter.width = 0.2))+
  theme_light(base_size = 14)+
  scale_x_discrete(labels=c("2022-04-06" = "April", "2022-05-16" = " May", "2022-06-27" = " June" , "2022-07-19" = "July"))+
  scale_y_continuous(limits = c(23,31), n.breaks = 5)+
  xlab("Sampling time")+ ylab("Critical thermal maximum in °C")+
  theme(legend.position = "right")+
  scale_fill_manual(values = c("#96B48E","#CFD4EB","#D5968F"), name = "Treatment")
Ctmax_out

Ctmax_out +
  ylim(NA, max(data$Ctmax)+ .5)+
  geom_text(data = data1a, aes(y = yloc, label = label),
            position = position_dodge(width = .75))



#Ctmax developmental temperature (+temp gradient)
ggplot(data, aes(x = mean2, y = Ctmax, fill = X2.week_mean))+
  geom_boxplot(outlier.shape = NA)+
  theme_light(base_size = 14)+
  scale_fill_gradientn(colors = wes_palette("Zissou1", type = "continuous"))+
  scale_x_discrete(labels =c("6.36" = "6.36 °C", "6.36" = "6.36 °C", "10.8" = "10.8 °C","11.44" = "11.44 °C","16.55" = "16.55 °C","17.9" = "17.9 °C" ,"18.11" = "18.11 °C"))+
  xlab("")+ ylab("Critical thermal maximum in °C")+
  labs(fill = "Developmental temperature in °C")+
  theme(legend.position = "bottom")

box <- ggplot(wild, aes(x = mean2, y = Ctmax, fill = X2.week_mean))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = position_jitterdodge(jitter.width = 0.2), aes(col = X2.week_mean, alpha = 0.2))+
  theme_light(base_size = 17)+
  scale_fill_gradientn(colors = alpha(wes_palette("Zissou1", type = "continuous"), 0.5))+
  scale_color_gradientn(colors = alpha(wes_palette("Zissou1", type = "continuous"), 0.1))+
  scale_x_discrete(labels =c("6.36" = "Collection 1","12.86" = "Collection 3","11.44" = "Collection 2","16.55" = "Collection 4","18.11" = "Collection 5"))+
  xlab("")+ ylab(expression("CT"["max"]* " in °C"))+
  theme(legend.position = "none")
  

box

#Ctmax developmental temperature (+treatment)
ggplot(data, aes(x = mean2, y = Ctmax, fill = treatment))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = position_jitterdodge(jitter.width = 0.6))+
  theme_light(base_size = 14)+
  scale_fill_manual(values = c("#96B48E","#CFD4EB","#D5968F"), name = "Treatment")+
  scale_x_discrete(labels =c("6.36" = "6.36 °C", "6.36" = "6.36 °C", "10.8" = "10.8 °C","11.44" = "11.44 °C","16.55" = "16.55 °C","17.9" = "17.9 °C" ,"18.11" = "18.11 °C"))+
  xlab("")+ ylab("Critical thermal maximum in °C")+
  labs(fill = "Developmental temperature in °C")+
  theme(legend.position = "bottom")

ggplot(poster, aes(x = mean2, y = Ctmax, fill = treatment))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = position_jitterdodge(jitter.width = 0.6))+
  theme_light(base_size = 14)+
  scale_fill_manual(values = c("#96B48E","#CFD4EB","#D5968F"), name = "Treatment")+
  scale_x_discrete(labels =c("6.36" = "6.36 °C", "6.36" = "6.36 °C", "10.8" = "10.8 °C","11.44" = "11.44 °C","16.55" = "16.55 °C","17.9" = "17.9 °C" ,"18.11" = "18.11 °C"))+
  xlab("")+ ylab("Critical thermal maximum in °C")+
  labs(fill = "Developmental temperature in °C")+
  theme(legend.position = "bottom")

####males and females####  
#males and females - thermal tolerance
ggplot(poster,aes(x=treatment, y=Ctmax, fill=sex_confirmed )) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width=0.2))+
  facet_wrap(~date_sampling,
             labeller = labeller(date_sampling = 
                                   c("2022-04-06" = "Collection 1 - 06 April 2022",
                                     "2022-05-16" = "Collection 2 - 16 May 2022",
                                     "2022-06-27" = "Collection 4 - 27 June 2022",
                                     "2022-07-19" = "Collection 5 - 19 July 2022")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme_light(base_size = 14)+
  theme(legend.position = "right")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  ylab(expression("CT"["max"]* " in °C"))+xlab("Treatment")+
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
p + geom_smooth(method = "lm", aes(group = sex_confirmed, col = sex_confirmed))

group_mean <- interaction(assays$daily_mean,assays$sex_confirmed)#with daily mean kimocc
p_mean <- ggplot(assays,aes(x=daily_mean, 
                       y=Ctmax,
                       color=sex_confirmed,
                       fill = sex_confirmed,
                       group = group_mean)) +
  theme_light(base_size = 14)+
  ylab(expression("CT"["max"]* " in °C"))+
  xlab("Treatment temperature in °C")+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  geom_point(aes(shape = generation), size = 2.75)
#"#FF8275","#8BA4FF" for darker colours

p_mean # plot for daily mean temperature
pm <- p_mean + geom_smooth(method = "lm", aes(group = sex_confirmed, col = sex_confirmed))

group_1w <- interaction(assays$X1.week_mean,assays$sex_confirmed)#with 1-week mean
p_1w <- ggplot(assays,aes(x=X1.week_mean, 
                            y=Ctmax,
                            color=sex_confirmed,
                            fill = sex_confirmed,
                            group = group_1w)) +
  theme_light(base_size = 14)+
  ylab(expression("CT"["max"]* " in °C"))+
  xlab("Treatment temperature in °C")+
  theme(legend.position = "right")+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  geom_point(aes(shape = generation), size = 2.75)

p_1w # plot for 1 week mean temperature
p1 <- p_1w + geom_smooth(method = "lm", aes(group = sex_confirmed, col = sex_confirmed))
p1

legend <- get_legend(p1)
 library(grid)
x11()
grid.draw(legend)

group_2w <- interaction(assays$X2.week_mean,assays$sex_confirmed)#with 2-week mean
p_2w <- ggplot(assays,aes(x=X2.week_mean, 
                          y=Ctmax,
                          color=sex_confirmed,
                          fill = sex_confirmed,
                          group = group_2w)) +
  theme_light(base_size = 14)+
  ylab(expression("CT"["max"]* " in °C"))+
  xlab("Developmental temperature in °C")+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  geom_point(aes(shape = generation), size = 2.75)

p_2w # plot for 2 week mean temperature
p2 <- p_2w + geom_smooth(method = "lm", aes(group = sex_confirmed, col = sex_confirmed))


combined_plot <- grid.arrange(pm, p1, p2, ncol = 1, nrow = 3)
plot_grid(combined_plot, legend, ncol = 2 , rel_widths = c(3/4, 1/4))

#2 week average but only hudsonica
group_hud2w <- interaction(data$X2.week_mean,data$sex_confirmed)#with 2-week mean
p_hud2w <- ggplot(data,aes(x=X2.week_mean, 
                          y=Ctmax,
                          color=sex_confirmed,
                          fill = sex_confirmed,
                          group = group_hud2w)) +
  theme_light(base_size = 14)+
  ylab(expression("CT"["max"]* " in °C"))+
  xlab("Developmental temperature in °C")+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  geom_point(aes(shape = generation), size = 2.75)

p_hud2w # plot for 2 week mean temperature
phud2 <- p_hud2w + geom_smooth(method = "lm", aes(group = sex_confirmed, col = sex_confirmed))
phud2


x11()
combined_plot <- grid.arrange(p2, phud2, ncol = 1, nrow = 2)
plot_grid(combined_plot, legend, ncol = 2 , rel_widths = c(4/5, 1/5))

####length####
#males and females - length per CTmax
all1 <- ggplot(assays, aes(y = Ctmax, x = length, col = sex_confirmed))+
  geom_point(aes(shape = generation), size = 2.75)+
  geom_smooth(method = "lm", aes(group = sex_confirmed, col = sex_confirmed, fill = sex_confirmed))+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  theme_light(base_size = 9)+
  theme(legend.position = "bottom")+
  ylab("Critical thermal maximum in °C")+
  xlab("Prosome length in µm")


legend <- get_legend(all1)

all <- ggplot(data, aes(y = Ctmax, x = length, col = sex_confirmed))+
  geom_point(aes(shape = generation), size = 2.75)+
  geom_smooth(method = "lm", aes(group = sex_confirmed, col = sex_confirmed, fill = sex_confirmed))+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  theme_light(base_size = 9)+
  theme(legend.position = "none")+
  ylab("Critical thermal maximum in °C")+
  xlab("Prosome length in µm")

all2 <- ggplot(data, aes(y = length, x = Ctmax, col = sex_confirmed))+
  geom_point(aes(shape = generation), size = 2.75)+
  geom_smooth(method = "lm", aes(group = sex_confirmed, col = sex_confirmed, fill = sex_confirmed))+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  theme_light(base_size = 9)+
  theme(legend.position = "none")+
  xlab("Critical thermal maximum in °C")+
  ylab("Prosome length in µm")

all
all2

ggarrange(all, all2, nrow =2)

data2 <- data[which(data$ï..collection != "2"),]#removes weird behaving col 2
data3 <- data2[which(data2$treatment != "wild"),]#removes wild ones
#17 for triangle

cold_warm <- ggplot(data3, aes(y = Ctmax, x = length, col = sex_confirmed))+
  geom_point(shape = 17, size = 2.75)+
  facet_wrap(~treatment)+
  theme_light(base_size = 9)+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black', size = 10))+
  geom_smooth(method = "lm", aes(group = sex_confirmed, col = sex_confirmed, fill = sex_confirmed))+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  theme(legend.position = "none")+
  ylab("Critical thermal maximum in °C")+
  xlab("Prosome length in µm")

cold_warm

x11()

plot_grid(all, cold_warm, legend, nrow = 3 , rel_heights  = c(5/10, 4/10, 1/10))

#length per developmental temperature
l_all <- ggplot(assays, aes(y = length, x = X2.week_mean, col = sex_confirmed))+
  geom_point(aes(shape = generation), size = 2.75)+
  geom_smooth(method = "lm", aes(group = sex_confirmed, col = sex_confirmed, fill = sex_confirmed))+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_x_continuous()+
  theme_light(base_size = 14)+
  theme(legend.position = "none")+
  ylab("Developmental temperature in °C")+
  xlab("Prosome length in µm")

l_hud <- ggplot(data, aes(y = length, x = X2.week_mean, col = sex_confirmed))+
  geom_point(aes(shape = generation), size = 2.75)+
  geom_smooth(method = "lm", aes(group = sex_confirmed, col = sex_confirmed, fill = sex_confirmed))+
  scale_color_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_fill_manual(values = c("#D5968F","#CFD4EB"), name = "sex")+
  scale_x_continuous()+
  theme_light(base_size = 14)+
  theme(legend.position = "none")+
  xlab("Developmental temperature in °C")+
  ylab("Prosome length in µm")

l_hud
x11()
combined <- grid.arrange(l_all, l_hud, ncol = 1, nrow = 2)
plot_grid(combined, legend, ncol = 2 , rel_widths = c(4/5, 1/5))

####wild CTmax####
#SST and wild CTmax

sub <- read.csv("~/RStuff/masterarbeit/temperature/sub.csv", header = TRUE)
mycolors <- c("kimocc" = "#D5968F", "ctmax"= "#689C87")##64885A#178B76##94B38B##689C87
sub$Date <-  as.Date(sub$Date)

Sys.setlocale("LC_TIME", "English")#set locale to English to avoid German months

ggplot(sub, aes(x=Date, y=Temperature, group=Origin, color=Origin)) +
  geom_point() +
  theme_light(base_size = 14)+ 
  xlab("")+
  scale_y_continuous(name="SST in °C", sec.axis = sec_axis(~ 0.58*.+20, name=expression("CT"["max"]* " in °C"))) +
  scale_color_manual(name="Origin", values = mycolors) +
  theme(
    axis.title.y = element_text(color = mycolors["kimocc"]),
    axis.text.y = element_text(color = mycolors["kimocc"]),
    axis.title.y.right = element_text(color = mycolors["ctmax"]),
    axis.text.y.right = element_text(color = mycolors["ctmax"]),
    legend.position = "none")

ggplot(wild, aes(y = Ctmax, x = X2.week_mean))+
  geom_point(size = 2.75, col = "#96B48E")+
  geom_smooth(method = "lm", aes(col = "#96B48E", fill = "#96B48E"))+
  scale_color_manual(values = c("#96B48E"), name = "")+
  scale_fill_manual(values = c("#96B48E"), name = "")+
  theme_light(base_size = 14)+
  theme(legend.position = "none")+
  ylab("Critical thermal maximum in °C")+
  xlab("Mean SST in °C")

ggplot(wild, aes(y = Ctmax, x = X2.week_mean, col = X2.week_mean))+
  geom_point(size = 2.75)+
  geom_smooth(method = "lm", color = "grey", fill = "lightgrey")+
  scale_fill_gradientn(colors = alpha(wes_palette("Zissou1", type = "continuous")))+
  scale_color_gradientn(colors = alpha(wes_palette("Zissou1", type = "continuous"),0.5))+
  theme_light(base_size = 14)+
  theme(legend.position = "none")+
  ylab("Critical thermal maximum in °C")+
  xlab("Mean SST in °C")


cor.test(wild$Ctmax, wild$X2.week_mean, method = "spearman")
#strong correlation btw wild Ctmax and temperature, spearman's p = 0.771
#stonger than for "temperature" 
cor.test(wild$Ctmax, wild$X2.week_mean)# pearson's r = 0.725
