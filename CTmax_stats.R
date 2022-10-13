##########################################################
##                    CTmax stats                       ##
##                   Alexandra Hahn                     ##
##########################################################

setwd("C:/Users/A.Hahn/Documents/RStuff/masterarbeit")
Sys.setenv(LANG = "en")

library(dplyr)

assays <- read.csv("~/RStuff/masterarbeit/assays.csv", sep=";", header = TRUE)
assays <- assays[-c(1, 59, 136, 246), ]#remove dead or inactive
assays <-  assays[!(is.na(assays$ï..collection)),]

#run for prep 
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
assays$time_assay <- factor(assays$time_assay, levels = c("morning", "afternoon"))

#subsets
data <- assays[which(assays$ï..collection != "3"),]
test <- data[which(data$ï..collection != "5"),]
parents <- data[which(data$generation == "parental"),]
f1 <- data[which(data$generation == "f1"),]
females <- assays[which(assays$sex_confirmed == "f"),]

}

####mixed model####
library(lme4)
library(nlme)
library(lmerTest)
library(car)
library(lattice)

####visualize individual effects####
bwplot(Ctmax~treatment|vial_number, data = data)
bwplot(Ctmax~ï..collection|vial_number, data = data)
bwplot(Ctmax~treatment|time_assay, data = data)
bwplot(Ctmax~ï..collection|time_assay, data = data)
bwplot(Ctmax~treatment|tank_side, data = data)
bwplot(Ctmax~ï..collection|tank_side, data = data)

interaction.plot(x.factor = data$ï..collection, 
                 trace.factor = data$treatment, 
                 response = data$Ctmax,
                 fun = mean)

####model####

#m1 <- lme(fixed = Ctmax~treatment*ï..collection, random = ~1|vial_number, method = "ML", data = data)
# Error bc instead of lme use lmer

# comparing models 
m1 <- lmer(Ctmax~treatment*ï..collection + sex_confirmed + (1|vial_number)+ (1|time_assay) + (1|tank_side),
               data = data)

m2 <- lmer(Ctmax~treatment*ï..collection + sex_confirmed + (1|vial_number),
               data = data)

m3 <- lmer(Ctmax~treatment*ï..collection + sex_confirmed + (1|vial_number) + (1|time_assay),
               data = data)

m4 <- lm(Ctmax~treatment*ï..collection + sex_confirmed,
               data = data)

anova(m3, m4)

# generates warning bc random effects are too small --> just drop them?
#random effects do not improve model fit --> drop random effects

library(sjPlot)
p <- plot_model(m1, type = "re", facet.grid=FALSE) 

library(gridExtra)# this visualizes the "random effects"
grid.arrange(p[[1]], p[[2]], p[[3]])# no strong effects....

####check model####

type3 <- list(treatment = contr.sum,ï..collection = contr.sum, sex_confirmed = contr.sum )
m6 <- lm(Ctmax~treatment*ï..collection + sex_confirmed,
         data = data, contrasts = type3)
Anova(m6, type = 3) # correct type 3

car::Anova(m4, type=c("III"))# does not work, aliased coefficients???
summary(m4)
plot(m4)

qqnorm(resid(m4))
qqline(resid(m4), col = "red")


#f1 model vs wild model
#genotyping - two species?
#temperature - polarfuchs data 

library(multcompView)

####more diagnostics####
#homogeneity of variances
plot(resid(m4)~fitted(m4))
abline(h=0, lwd=2, lty=2, col="red")

#normality of residuals
hist(resid(m4))
shapiro.test(resid(m4))#check residuals for normality, if insignificant residuals are normal

#influential data points
plot(cooks.distance(m4), type="h")#cook's distance shouldn't be larger than 1


#####post-hoc testing#####
library(multcomp)
library(emmeans)
library(LMERConvenienceFunctions)
pairwise.t.test(test$Ctmax, test$treatment, p.adjust ="holm")

lsmeans( object=m4, pairwise ~ treatment * ï..collection, adjust= "tukey")
data$interaction <- interaction(data$treatment, data$ï..collection, sep="x")
m5 <- lm(Ctmax ~ interaction + sex_confirmed, data= data )
summary(glht(model= m5, linfct= mcp(interaction = "Tukey")))


#normal tukey does not work for lm....

#tukey <- TukeyHSD(m4, conf.level = .95)
#print(tukey)

#cld <- multcompLetters4(m4, tukey) #add compact letters to indicate differences
#print(cld)

#table
#Tk<- group_by(data, interaction(treatment, ï..collection))%>%
#  summarise(mean = mean(Ctmax), quant = quantile(Ctmax, probs = 0.75))%>%
#  arrange(desc(mean))
#print(Tk)

#cld<- as.data.frame.list(cld$`data$treatment:data$ï..collection`)
#Tk$cld<-cld$Letters
#print(Tk)


####only f1####

m_f1 <- lm(Ctmax~treatment*ï..collection + sex_confirmed,
         data = f1)

car::Anova(m_f1, type="III")# does not work?
summary(m_f1)
plot(m_f1)

anova(m_f1)

####temperature####

m5 <- lm(Ctmax~temperature + sex_confirmed, data = data)

anova(m5)
plot(m5)
car::Anova(m5, type="III")


#acclimation temp and size, does size have effect?
