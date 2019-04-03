# Make a univariate linear model for one of your hypotheses
# Examine the assumptions of linearity (using tests or diagnostic plots)
# Plot the relationship in ggplot using stat_smooth
# Remember to update your README file - we will combine this week and next week so you can indicate that in your file

library(tidyverse)
library(emmeans)
library(effects)

pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE)
bvr <- read.csv("BVR_CTD.csv")
# hypothesis: chlorophyll-a will increase with increasing temperature
lm1 <- lm(Chla_ugL~Temp_C, data = bvr)
summary(lm1)
plot(lm1)
# qq-plot does not look normal so log transform chl first
bvr <- bvr %>% mutate(chl_log = log(Chla_ugL))
lm1 <- lm(chl_log~Temp_C, data = bvr)
summary(lm1)
plot(lm1)

ggplot(bvr, aes(x = Temp_C, y = chl_log)) + stat_smooth(method = "lm") +geom_point()
# this isn't a very linear relationship! there is a threshold at which
# higher temperatures actually lead to decreases in chl

##################################################################################################
# PART 2
# 1. Make a linear model (with more than one variable) for one of your hypotheses. Articulate which 
# hypothesis you are testing.
# 2. Use an interactive model and an additive model. Explain what hypothesis each of these is testing, 
# and what the R output is telling you about your data.
# (Hint: you can use lsmeans, effects, relevel, or predict to help you.) You should include this explanation in either your README or in your code.
# 3. Plot your model (e.g. using predict) and overlay the model on top of the underlying data. 
# See code for example to plot both model and data.

# look first at the different potential drivers and their relationship with log_chl
ggplot(bvr, aes(x = DO_mgL, y = chl_log)) + stat_smooth() +geom_point()
ggplot(bvr, aes(x = Cond_uScm, y = chl_log)) + stat_smooth() +geom_point()
ggplot(bvr, aes(x = Turb_NTU, y = chl_log)) + stat_smooth() + geom_point()

# Hypothesis: chl increases with increasing water temperature and turbidity
# first an additive model
lm2 <- lm(chl_log~Temp_C + Turb_NTU, data = bvr)
summary(lm2)
plot(lm2)

plot(allEffects(lm2))

pred <- predict(lm2)
plot(pred)
plot(bvr$chl_log, col ='red')
points(pred, col = 'black')


# now an interactive model
# Hypothesis: chl is driven by the interaction btw water temperature and turbidity (water temperature and turbidity influence each other)
lm3 <- lm(chl_log~Temp_C*Turb_NTU, data = bvr)
summary(lm3)
plot(lm3)

plot(allEffects(lm3))

pred3 <- predict(lm3)
plot(pred3)
plot(bvr$chl_log, col ='red')
points(pred3, col = 'black')
# the interactive model seems to do a slightly better job at predicting chl than the additive model only,
# but neither of these are good models!
