library(tidyverse)
library(MASS)

bvr <- read.csv("./BVR_CTD.csv")
hist(bvr$Chla_ugL)
# gamma 
chl <- na.omit(bvr$Chla_ugL)

nb.fit = fitdistr(chl,densfun = "gamma" )
nb.fit # shape = 1.92, rate = 0.49

length(chl)
# 728

hist(rgamma(728,shape = 1.92, rate = 0.49))

new.chl <- rgamma(728,shape = 1.92, rate = 0.49)
hist(new.chl)

# now as a question with the made-up distribution and use the functions to answer
#what is the 90th percentile value of chl?
qgamma(p = 0.9,shape = 1.92, rate = 0.49)
# chl value of 7.69 is in the 90th percentile

#what is the probability of getting a chl value of 25 or higher?
pgamma(q=25, shape = 1.92, rate = 0.49, lower.tail = FALSE)
# probability of 5.321349e-05 that chl will be 25 ug/L or higher
