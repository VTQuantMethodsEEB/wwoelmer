# week 12 assignment
# run a mixed model on one or more of your hypotheses
# in a separate document, write a results statements based on your model output. You can include any
# tables you need to reference in your statement
# remember to update your readme file, annotate your code, and push your changes to github. 

library(tidyverse)

bvr <- read.csv("BVR_CTD.csv")
bvr$Date <- as.Date(bvr$Date)
bvr$Month <- format(bvr$Date, '%m')
bvr$Month <- as.factor(bvr$Month)
surf <- bvr[bvr$Depth_m<1.0,]
surf$Temp_cor <- surf$Temp_C-5

# want to use month as a random effect to 'control' for the effect of season on chla
# other driver variables include water temperature and turbidity

gm1 = glmer(Chla_ugL~Turb_NTU + Temp_C + (1|Month),data=surf, family = "Gamma")
summary(gm1)
plot(surf$Chla_ugL)
points(predict(gm1, type = 'response'), col = 'red')

