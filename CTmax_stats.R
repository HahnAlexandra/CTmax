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
library(sjPlot)
library(lme4)
library(nlme)
library(lmerTest)
library(lattice)

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
assays$species <- factor(assays$species, levels = c("hudsonica", "tonsa"))

#subsets
data_all  <- assays[which(assays$ï..collection != "3"),]
#parents <- data_all[which(data_all$generation == "parental"),]
#f1 <- data_all[which(data_all$generation == "f1"),]
data_all$mean2 <- as.factor(data_all$X2.week_mean)
data <- data_all[which(data_all$species == "hudsonica"),]
data <- droplevels(data)

}

df1a <- data_all %>%
  dplyr::select(Ctmax, treatment, sex_confirmed, length, ï..collection, X2.week_mean, species) %>%
  plot()

df1b <- data %>%
  dplyr::select(Ctmax, treatment, sex_confirmed, length, ï..collection, X2.week_mean) %>%
  plot()

#check normality
hist(assays$Ctmax)
shapiro.test(assays$Ctmax)

hist(data$Ctmax)#still not normal
shapiro.test(data$Ctmax)

# check linearity

plot(Ctmax~temperature, data = data)
abline(lm(data$Ctmax~data$temperature), col = "red")

plot(Ctmax~X2.week_mean, data = data)
abline(lm(data$Ctmax~data$X2.week_mean), col = "red")

#check correlation
cor.test(data$temperature, data$length, method= "spearman")# negatively correlated
cor.test(data$X2.week_mean, data$length, method= "spearman")# negatively correlated


# Chi squared tests for correlation
cor_sex.treat <- data.frame(data$sex_confirmed, data$treatment)
cor_sex.treat = table(data$sex_confirmed, data$treatment) 
chisq.test(cor_sex.treat)#no correlation

cor_sex.length <- data.frame(data$sex_confirmed, data$length)
cor_sex.length = table(data$sex_confirmed, data$length) 
chisq.test(cor_sex.length)#no correlation

cor_sex.col<- data.frame(data$sex_confirmed, data$ï..collection)
cor_sex.col = table(data$sex_confirmed, data$ï..collection) 
chisq.test(cor_sex.col)#no correlation

cor_length.treat <- data.frame(data$length, data$treatment)
cor_sex.col = table(data$length, data$treatment) 
chisq.test(cor_sex.col)#no correlation

cor_sex.col <- data.frame(data$length, data$ï..collection)
cor_sex.col = table(data$length, data$ï..collection) 
chisq.test(cor_sex.col)#no correlation 

cor_col.treat <- table(data$ï..collection, data$treatment)
print(cor_col.treat)
chisq.test(cor_col.treat)#"correlation"

####small models####

boxplot(data_all$Ctmax ~ data_all$species)
a0 <- aov(data_all$Ctmax ~ data_all$species)
summary(a0)
TukeyHSD(a0)

boxplot(data$Ctmax ~ data$ï..collection)
a1 <- aov(data$Ctmax ~ data$ï..collection)
summary(a1)
TukeyHSD(a1)

boxplot(data$Ctmax ~ data$treatment)
a2 <- aov(data$Ctmax ~ data$treatment)
summary(a2)
TukeyHSD(a2)
pairwise.t.test(data$Ctmax, data$treatment, p.adjust ="holm")


boxplot(data$Ctmax ~ data$mean2)
a3 <- aov(data$Ctmax ~ data$mean2)
summary(a3)
TukeyHSD(a3)

mm3 <- emmeans(object = a3,
                       specs = "mean2")#gets adjusted and weighted means per group

mm3_cld <- cld(object = mm3,
                       adjust = "sidak",
                       Letters = letters,
                       alpha = 0.05)#adds compact letters

mm3_cld

#for all data points
boxplot(data_all$Ctmax ~ data_all$mean2)
a3b <- aov(data_all$Ctmax ~ data_all$mean2)
Anova(a3b, type = 3)
TukeyHSD(a3b)

mm3b <- emmeans(object = a3b,
               specs = "mean2")#gets adjusted and weighted means per group

mm3b_cld <- cld(object = mm3b,
               adjust = "sidak",
               Letters = letters,
               alpha = 0.05)#adds compact letters

mm3b_cld

plot(data$Ctmax ~ data$length)
a4 <- lm(data$Ctmax ~ data$length)
summary(a4)


####compact letters####

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

#just hudsonica
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

hist(resid(model))
ols_test_normality(model)
ols_test_correlation(model)#correlation between observed and expected residuals (under normality)

par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))

durbinWatsonTest(model)#no autocorrelation if p > 0.05

####multiple regression####
#type 3 is needed to get non-sequential sums of squares
#summary with lm() already uses type III
#otherwise Anova(model, type = 3), but also set contrasts in model to type 3...
 
#with random effects
M0 <- lmer(Ctmax~treatment*ï..collection* sex_confirmed*length + (1|vial_number)+ (1|time_assay) + (1|tank_side),
             data = data)
summary(M0)

# generates warning bc random effects are too small --> just drop them?
p <- plot_model(M0, type = "re", facet.grid=FALSE) 

# this visualizes the "random effects"
grid.arrange(p[[1]], p[[2]], p[[3]])# no strong effects....

#random effects do not improve model fit --> drop random effects

#with all interactions
type3 <- list(treatment = contr.sum,ï..collection = contr.sum, sex_confirmed = contr.sum)


M1 <- lm(Ctmax ~ treatment * ï..collection * sex_confirmed * length, data = data)
M1a <- lm(Ctmax~ treatment *  ï..collection + sex_confirmed + length + sex_confirmed:length, data= data )
M1b <- lm(Ctmax~ treatment *  ï..collection + sex_confirmed + length, data= data, contrasts = type3)
anova(M1a, M1b)
anova(M1b, M1)
AIC(M1, M1b)# keep M1b
summary(M1b)
anova(M1b)
Anova(M1b, singular.ok = T, type = 3)
#M1a <- lm(Ctmax ~ treatment * ï..collection * sex_confirmed * length * species, data = data_all)
summary(M1)#adj. R squared 0.7252
summary(M1a)#0.9403
anova(M1); anova(M1a)

hist(resid(M1b))
ols_plot_resid_hist(M1b)
ols_test_normality(M1b)
ols_test_correlation(M1b)


par(mfrow = c(2,2))
plot(M1b)
par(mfrow = c(1,1))

durbinWatsonTest(M1b)#no autocorrelation if p > 0.05

##simplifications? tried but dropping terms means losing explanatory power

####more models####
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

m6<- lm(Ctmax~X2.week_mean*ï..collection + sex_confirmed,
        data = data)
summary(m6)

m7 <- lm(Ctmax~treatment*ï..collection + sex_confirmed + length, data = data)
anova(m7)#length not significant in "outlier-free data set"
summary(m7)
#compare models

AIC(m3,m4)#the same --> simpler model m4
AIC(m4, m5)#m4 lower AIC
AIC(m4, m6)#m4 lower AIC
#m4 lowest when comparing AIC, highest adj. R sq.


# test for normality, autocorrelation etc.

hist(resid(m4))
ols_plot_resid_hist(m4)#looks normal
ols_test_normality(m4)
#Kolmogorov-Smirnov for observations larger than 50, if p-value is > 0.05 --> residuals normally distributed

par(mfrow = c(2,2))
plot(m4)
par(mfrow = c(1,1))

durbinWatsonTest(m4)#no autocorrelation if p > 0.05

####Model <- lm(Ctmax~treatment*ï..collection + sex_confirmed, data = data)#####

####mixed model####

####visualize individual random effects####
bwplot(Ctmax~treatment|vial_number, data = assays)
bwplot(Ctmax~ï..collection|vial_number, data = assays)
bwplot(Ctmax~treatment|time_assay, data = assays)
bwplot(Ctmax~ï..collection|time_assay, data = assays)
bwplot(Ctmax~treatment|tank_side, data = assays)
bwplot(Ctmax~ï..collection|tank_side, data = assays)

bwplot(Ctmax~treatment|vial_number, data = data)
bwplot(Ctmax~ï..collection|vial_number, data = data)
bwplot(Ctmax~treatment|time_assay, data = data)
bwplot(Ctmax~ï..collection|time_assay, data = data)
bwplot(Ctmax~treatment|tank_side, data = data)
bwplot(Ctmax~ï..collection|tank_side, data = data)

#no visible difference between random effect plots


####check model####

type3 <- list(treatment = contr.sum,ï..collection = contr.sum, sex_confirmed = contr.sum )
m7 <- lm(Ctmax~treatment*ï..collection + sex_confirmed,
         data = assays, contrasts = type3)
Anova(m4, type = 3) # correct type 3

car::Anova(m4, type=c("III"))# does not work, aliased coefficients???
summary(m4)
plot(m4)



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

#normal tukey does not work for lm....
#post-hoc testing with emmeans

pairs(emmeans(M1b , ~ï..collection|treatment|length|sex_confirmed))
#that's it?


