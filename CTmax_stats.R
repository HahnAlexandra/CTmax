##########################################################
##                    CTmax stats                       ##
##                   Alexandra Hahn                     ##
##########################################################

setwd("C:/Users/A.Hahn/Documents/RStuff/masterarbeit")
Sys.setenv(LANG = "en")

library(dplyr)
library(olsrr)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)

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
data_all  <- assays[which(assays$ï..collection != "3"),]
parents <- data_all[which(data_all$generation == "parental"),]
f1 <- data_all[which(data_all$generation == "f1"),]
data_all$mean2 <- as.factor(data_all$X2.week_mean)
data <- data_all[which(data_all$Ctmax < 32),]

}

df1 <- data %>%
  dplyr::select(Ctmax, treatment, sex_confirmed, length, ï..collection, X2.week_mean) %>%
  plot()
 
####multiple regression####

#check normality
hist(assays$Ctmax)
shapiro.test(assays$Ctmax)

hist(data$Ctmax)#outliers above 32 °C removed, still not normal
shapiro.test(data$Ctmax)

# check linearity

plot(Ctmax~temperature, data = data)
abline(lm(data$Ctmax~data$temperature), col = "red")

plot(Ctmax~X2.week_mean, data = data)
abline(lm(data$Ctmax~data$X2.week_mean), col = "red")

#check correlation

cor.test(data$temperature, data$length, method= "spearman")#to 46% negatively correlated
cor.test(data$X2.week_mean, data$length, method= "spearman")#to 56% negatively correlated

#####build models#####
#small models

boxplot(data$Ctmax ~ data$ï..collection)
a1 <- aov(data$Ctmax ~ data$ï..collection)
Anova(a1)
TukeyHSD(a1)

boxplot(data$Ctmax ~ data$treatment)
a2 <- aov(data$Ctmax ~ data$treatment)
Anova(a2)
TukeyHSD(a2)

data$X2.week_mean <- as.factor(data$X2.week_mean)# is that allowed, useful?
boxplot(data$Ctmax ~ data$X2.week_mean)
a3 <- aov(data$Ctmax ~ data$X2.week_mean)
summary(a3)
Anova(a3)
TukeyHSD(a3)

mm3 <- emmeans(object = a3,
                       specs = "X2.week_mean")#gets adjusted and weighted means per group

mm3_cld <- cld(object = mm3,
                       adjust = "sidak",
                       Letters = letters,
                       alpha = 0.05)#adds compact letters

mm3_cld

#anova

#a4 <- aov(data$Ctmax~data$ï..collection*data$treatment)
a4 <- aov(data$Ctmax~data$ï..collection:data$treatment)
summary(a4)
TukeyHSD(a4)

tukey <- TukeyHSD(a4, conf.level = .95)
print(tukey)

AIC(a3, a4)#AIC lower for 4 --> interaction of col and treatment is better predictor of CTmax than temperature

a5 <- aov(Ctmax ~ ï..collection * treatment, data = data)
summary(a5)

a6 <- aov(Ctmax ~ ï..collection * treatment * length, data = data)
summary(a6)

a7 <- aov(Ctmax ~ ï..collection * treatment * length * sex_confirmed, data = data)
summary(a7)

plot(Ctmax~length, data = data)
abline(lm(data$Ctmax~data$length), col = "red")

a8 <- aov(Ctmax ~ length, data = data)
summary(a8)

a8 <- lm(Ctmax~ length*sex_confirmed, data = data)
summary(a8)#explains 22 % of variance
anova(a8)

#compact letters
#lm interactions

#all
int_a <- interaction(data_all$treatment, data_all$ï..collection)
model_a <- lm(Ctmax ~ int_a, data = data_all)
summary(model_a)
anova(model_a)
boxplot(Ctmax ~int_a, data = data_all)


model_means_a <- emmeans(object = model_a,
                       specs = "int_a")#gets adjusted and weighted means per group

model_means_cld_a <- cld(object = model_means_a,
                       adjust = "sidak",
                       Letters = letters,
                       alpha = 0.05)#adds compact letters

model_means_cld_a

#without outliers
int <- interaction(data$treatment, data$ï..collection)
model <- lm(Ctmax ~ int, data = data)
summary(model)
anova(model)
boxplot(Ctmax ~int, data = data)


model_means <- emmeans(object = model,
                       specs = "int")#gets adjusted and weighted means per group

model_means_cld <- cld(object = model_means,
                       adjust = "sidak",
                       Letters = letters,
                       alpha = 0.05)#adds compact letters

model_means_cld#warm ones are together, cold ones are together with wild 2

int2 <- interaction(data$mean2, data$ï..collection)
model2 <- lm(Ctmax ~ int2, data = data)
summary(model2)
boxplot(Ctmax ~ int2, data = data)

model_means2 <- emmeans(object = model2,
                       specs = "int2")#gets adjusted and weighted means per group

model_means_cld2 <- cld(object = model_means2,
                       adjust = "sidak",
                       Letters = letters,
                       alpha = 0.05)#adds compact letters

model_means_cld2# same as treatment 

hist(resid(model))
ols_test_normality(model)
ols_test_correlation(model)#correlation between observed and expected residuals (under normality)


par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))

durbinWatsonTest(model)#no autocorrelation if p > 0.05

model3 <- lm(Ctmax ~ int + sex_confirmed, data = data)
summary(model3)

#####big models#####

M1a <- lm(Ctmax ~ treatment * ï..collection * sex_confirmed * length, data = data)

M1 <- lm(Ctmax ~ treatment * ï..collection * sex_confirmed, data = data)
summary(M1)
anova(M1)

M1a <- update(M1,~.-length, data = data)
M1b <- update(M1a,~.-treatment:ï..collection:length, data = data)
M1c <- update(M1b,~.-treatment:ï..collection:sex_confirmed:length, data = data)
M1d <- update(M1c,~.-ï..collection:sex_confirmed, data = data )
M1e <- update(M1d,~.-ï..collection:length, data = data)                      
M1f <- update(M1e,~.-sex_confirmed:length, data = data)
M1g <- update(M1f,~.-treatment:length, data = data)
M1h <- update(M1g,~.-treatment:ï..collection:sex_confirmed, data = data)
M1i <- update(M1h,~.-ï..collection:sex_confirmed:length, data = data)
M1j <- update(M1i,~.-treatment:sex_confirmed:length, data = data)


M2 <- update(M1,~.-ï..collection:sex_confirmed , data = data)
anova(M2)

M3 <- update(M2,~.-treatment:ï..collection:sex_confirmed, data = data)
anova(M3)

M4 <- update(M3,~.-treatment:sex_confirmed, data = data)
anova(M4)

anova(M1,M4)


m1<- lm(Ctmax~temperature*ï..collection + sex_confirmed + treatment,
        data = data)
summary(m1)

m2<- lm(Ctmax~treatment*ï..collection + sex_confirmed,
        data = data)
summary(m2)

m3<- lm(Ctmax~treatment*ï..collection + sex_confirmed + temperature,
        data = data)
summary(m3)


m4<- lm(Ctmax~treatment*ï..collection + sex_confirmed,
        data = data)
summary(m4)
anova(m4)

m4a <- lm(Ctmax~treatment*ï..collection + sex_confirmed + sex_confirmed:length,
          data = assays)
summary(m4a)#for whole data-set length is relevant, model expalins 80.87 % of variance
anova(m4a)

m5<- lm(Ctmax~temperature*ï..collection + sex_confirmed,
        data = data)
summary(m5)# lower R-squared but makes more sense to me

data$X2.week_mean <- as.factor(data$X2.week_mean)# is that allowed, useful?

m6<- lm(Ctmax~X2.week_mean*ï..collection + sex_confirmed,
        data = data)
summary(m6)

m7 <- lm(Ctmax~treatment*ï..collection + sex_confirmed + length, data = data)
anova(m7)#length not significant in "outlier-free data set"

#compare models

AIC(m3,m4)#the same --> simpler model m4
AIC(m4, m5)#m4 lower AIC
AIC(m4, m6)#how can it be the same? 

# test for normality, autocorrelation etc.

hist(resid(m4))
ols_plot_resid_hist(m4)#looks normal
ols_test_normality(m4)
#Kolmogorov-Smirnov for observations larger than 50, if p-value is > 0.05 --> residuals normally distributed

par(mfrow = c(2,2))
plot(m4)
par(mfrow = c(1,1))

durbinWatsonTest(m6)#no autocorrelation if p > 0.05

####Model <- lm(Ctmax~treatment*ï..collection + sex_confirmed, data = data)#####

####mixed model####
library(lme4)
library(nlme)
library(lmerTest)
library(car)
library(lattice)

####visualize individual effects####
bwplot(Ctmax~treatment|vial_number, data = assays)
bwplot(Ctmax~ï..collection|vial_number, data = assays)
bwplot(Ctmax~treatment|time_assay, data = assays)
bwplot(Ctmax~ï..collection|time_assay, data = assays)
bwplot(Ctmax~treatment|tank_side, data = assays)
bwplot(Ctmax~ï..collection|tank_side, data = assays)

#no visible difference between random effect plots

####model####


#m1 <- lme(fixed = Ctmax~treatment*ï..collection, random = ~1|vial_number, method = "ML", data = data)
# Error bc instead of lme use lmer

# comparing models 
m1 <- lmer(Ctmax~treatment*ï..collection + sex_confirmed + (1|vial_number)+ (1|time_assay) + (1|tank_side),
               data = assays)

m2 <- lmer(Ctmax~treatment*ï..collection + sex_confirmed + (1|vial_number),
               data = assays)

m3 <- lmer(Ctmax~treatment*ï..collection + sex_confirmed + (1|vial_number) + (1|time_assay),
               data = assays)

m4 <- lm(Ctmax~treatment*ï..collection + sex_confirmed,
               data = assays)

m5 <- lm(Ctmax~treatment*ï..collection + sex_confirmed + length,
         data = assays)

m5_2 <- lm(Ctmax~treatment*ï..collection + sex_confirmed + temperature,
         data = assays)

m5_3 <- lm(Ctmax~treatment*ï..collection + sex_confirmed + X2.week_mean,
           data = assays)

m6 <- lm(Ctmax~treatment*ï..collection + sex_confirmed + length + temperature,
         data = assays)


anova(m3, m4)#AIC lower for model 4
summary(m4)
summary(m5)
anova(m5)
summary(m5_2)
summary(m5_3)
summary(m6)# m5 and m6 have same R-squared, no added value of acclimation temperature


# generates warning bc random effects are too small --> just drop them?
#random effects do not improve model fit --> drop random effects

library(sjPlot)
p <- plot_model(m1, type = "re", facet.grid=FALSE) 

library(gridExtra)# this visualizes the "random effects"
grid.arrange(p[[1]], p[[2]], p[[3]])# no strong effects....

####check model####

type3 <- list(treatment = contr.sum,ï..collection = contr.sum, sex_confirmed = contr.sum )
m7 <- lm(Ctmax~treatment*ï..collection + sex_confirmed,
         data = assays, contrasts = type3)
Anova(m7, type = 3) # correct type 3

car::Anova(m4, type=c("III"))# does not work, aliased coefficients???
summary(m4)
plot(m4)

qqnorm(resid(m4))
qqline(resid(m4), col = "red")


#f1 model vs wild model
#genotyping - two species?


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
library(emmeans)
library(LMERConvenienceFunctions)
pairwise.t.test(test$Ctmax, test$treatment, p.adjust ="holm")

lsmeans( object=m4, pairwise ~ treatment * ï..collection, adjust= "tukey")
data$interaction <- interaction(data$treatment, data$ï..collection, sep="x")
m5 <- lm(Ctmax ~ interaction + sex_confirmed, data= data )
summary(glht(model= m5, linfct= mcp(interaction = "Tukey")))


#normal tukey does not work for lm....

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
