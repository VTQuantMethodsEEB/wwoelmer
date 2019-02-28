# Week 5 Assignment
# formulate two hypotheses and describe how you would test them with 
# 1) and permutation and 2) and classic test


library(tidyverse)

bvr <- read.csv("./BVR_CTD.csv")
bvr$Date <- as.Date(bvr$Date, format = "%Y-%m-%d")

# hypothesis #1--chlorophyll a will be higher in surface waters than in the bottom waters
hist(bvr$Depth_m)
unique(bvr$Depth_m)
# make a vector of surface measurements and bottom measurements of chlorophyll
surf <- bvr[bvr$Depth_m<=3.0,]
bottom <- bvr[bvr$Depth_m>=9.0,]
plot(surf$Date, surf$Chla_ugL, col = 'green')
points(bottom$Date, bottom$Chla_ugL, col = 'blue')

surf <- surf$Chla_ugL
surf <- na.omit(surf)
bottom <- bottom$Chla_ugL
bottom <- na.omit(bottom)

res <- NA

for(i in 1:10000){
  boot <- sample(c(surf, bottom)) 
  surfboot <- boot[1:length(surf)]
  bottomboot <- boot[length(surf)+1]
  res[i] <- mean(surfboot) - mean(bottomboot)
  
}

obs <- mean(surf)-mean(bottom)
obs

hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")

##so how do we get our p-value?
res[res>=obs]
length(res[res>=obs])
length(res[res>=obs])/10000
mean(res>=obs)        
# p value says there is no significant difference
# this is interesting because BVR reservoir is actually known for having a deep chlorophyll maximum
# in other words, there are algae that are specifially suited to low light environments and
# thrive deeper in the water column in BVR, so this does make sense that there would be high
# algal biomass deeper in this reservoir

# Hypothesis #2--Chlorophyll a and turbidity are correlated
plot(bvr$Turb_NTU, bvr$Chla_ugL)
plot(bvr$Temp_C, bvr$Chla_ugL)

shapiro.test(bvr$Chla_ugL) # not normal
shapiro.test(bvr$Turb_NTU) # not normal
# log transform first
bvr$chla_log <- log(bvr$Chla_ugL)
bvr$turb_log <- log(bvr$Turb_NTU)

chla <- bvr$chla_log
turb <- bvr$turb_log
# now run the correlation
cor.test(chla, turb)

# correlation of 0.15, not a strong relationship