#cargo los datos 
weight_gain_2 <- read.csv(file = "weight gain_2.csv")
head(weight_gain_2)
str(weight_gain_2)

#y librerias
library(nlme)

#plot

plot(weight_gain_2$weight ~ weight_gain_2$group)
#there is a lot of variance
plot(weight ~ group,  subset(weight_gain_2, week == 6)) 
#more intersting for week 6

#model
#Important, the cuadratic term needs to be explicit, otherwise R calculates it.
#workers is a covariable, you don't expect interactions.
#interaction is only between week and group.
#The only obvous random is nest (and is compulsory statistically speaking).
# This is the structure you want to test, so do not do model selection!
lme1 <- lme(weight ~ workers + group* (week + I(week^2)), random = ~1|nest, 
            data = weight_gain_2, na.action = na.omit)
plot(lme1) #clearly mean-variance relationship!

#try a quick fix
lme1 <- lme(log(weight) ~ workers + group* (week + I(week^2)), random = ~1|nest, 
            data = weight_gain_2, na.action = na.omit)
plot(lme1) #works... at some point we can try lme4
library(car) 
Anova(lme1, type = "III") #when interactions are present anova should be type 3, this is in package car
#there is a weak intercation between group and week.
summary(lme1) #in detail we see that T1 is the one more diferent from control (both as main as interaction)
#I woudn't do post hoc for this, just comment the estimates + SE. You can try visreg pacgage for visualization.
#or play with the formulas to plot the equation properly for each group. This is tricky, but all info is on the coefs

#One thing that I don't know is how to model the cuadratic interaction. 
#This was my first aproach, but I've never seen it...

#maybe using lme4 and another distribution will turn the 0.054 into a 0.045, BUT the interpretation would be exactly the same.

#another aproach would be to detrend the effect of week. For that I would scale all weeks
#usng tapply and scale() for example. and then look at the differences per treatment only.
#This may avoid the cuadratic problem and can be easier to explain?

#the diferences with time series is that you would try to model week as a continous random factor.
#I think for your purposes, you can ignore it.
