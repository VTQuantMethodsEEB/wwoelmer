# Week 11 Assignment - Part 1
# Make a generalized linear model (preferably with more than one variable) for one of your hypotheses. 
# Articulate which hypothesis you are testing.
# Explain what the R output is telling you about your data, in relation to your hypothesis.
# (Hint: you can use lsmeans, effects, relevel, or predict to help you.) You should include this 
# explanation in either your README or in your code.
# Plot your model (e.g. using predict) and overlay the model on top of the underlying data. 
#Remember that you will need to use “type=response”.
# Write a results statement (as you would in a scientific paper). 
#If you need to reference a statistical table, you can include this result statement and table as a separate word doc that you push to github titled “LASTNAME_week10_glm_results”

library(tidyverse)
library(emmeans)
library(effects)

pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE)
bvr <- read.csv("BVR_CTD.csv")

hist(bvr$Chla_ugL)
# this is somewhat gamma distributed, so will use a gamma distributed glm
plot(bvr$Chla_ugL, bvr$Depth_m)
# Hypothesis: chlorophyll is driven by changes in temperature by depth
glm1 <- glm(Chla_ugL ~ Temp_C + Depth_m, data = bvr, family = Gamma(link = 'inverse'))
summary(glm1)
# this output is saying there is no significant effect of temperature or depth on chl

plot(bvr$Chla_ugL) 
points(predict.glm(glm1, type = 'response'), col = 'red')

# hypothesis: chlroophyll is driven by changes in turbidity
glm2 <- glm(Chla_ugL ~ Turb_NTU, data = bvr, family = Gamma(link = 'log'))
plot(bvr$Chla_ugL)
points(predict.glm(glm2, type = 'response'), col = 'red')

# these aren't great models because it is total chlorophyll from all depths
# what about looking at just the surface chl?

surf <- bvr[bvr$Depth_m<0.5,]


glm3 <- glm(Chla_ugL ~ Turb_NTU, data = surf, family = Gamma(link = 'log'))
plot(surf$Chla_ugL)
points(predict.glm(glm3, type = 'response'), col = 'red')
summary(glm3)
# this model says there is a significant effect of turbidity on chl
# because the coefficient estimate is 0.378, this is a slightly positive relationship


###############################################################################################################
# week 11
#1. Use likelihood ratio tests and one other model selection approach to test at least 3 models of 
#your data.

# will test glm3 (chl ~ turbidity), glm4 (chl~ turbidity + temp), and glm5 (chl~turbdity + temp + DO)
glm4 <- glm(Chla_ugL ~ Turb_NTU + Temp_C, data = surf, family = Gamma(link = 'log'))
plot(surf$Chla_ugL)
points(predict.glm(glm4, type = 'response'), col = 'red')

glm5 <- glm(Chla_ugL ~ Turb_NTU + Temp_C + DO_mgL, data = surf, family = Gamma(link = 'log'))
plot(surf$Chla_ugL)
points(predict.glm(glm5, type = 'response'), col = 'red')

anova(glm3, glm4, glm5, test = "LRT")
# the anova says that none of these models are significant!
AIC(glm3, glm4, glm5)
# in this case AIC tells us that they are all equally as good, although we know from the anova
# and from looking at the predicted output that none of these models are good!

#2. Explain what the results are telling you for each approach.
# The results are saying that neither turbidity, water temperature, nor dissolved oxygen are good predictors
# of chlorophyll-a in this time series. 

#3. Include a synthesis statement on how the output of each approach is similar or different in 
#your code. Remember to update your README and annotate your code.
# The LRT tells us that while all the models are equally informative, not of them are significant, showing 
# that none of these are telling us any significant information. In contrast, AIC also tells us
# that the models are not different from each (i.e., they are all equally informative), but the format of
# AIC does not also tell you whether or not the models themselves provide significant information, meaning
# that it is important to use multiple model tests in order to get a more complete picture of the 
# performance of models. 
