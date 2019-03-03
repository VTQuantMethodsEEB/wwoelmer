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

# what is the probability of getting chl value of less than 1?
pgamma(q = 1, shape = 1.92, rate = 0.49)
# probability of 0.1 or 10% chance

# how does this compare to log transformed chl data?
bvr$chl_log <- log(bvr$Chla_ugL)
chl_log <- na.omit(bvr$chl_log)

norm.fit = fitdistr(chl_log,densfun = "normal" )
norm.fit # mean = 1.08, sd = 0.73
new.chl_log <- rnorm(728, mean = 1.08, sd = 0.73)
hist(new.chl_log)

exp(qnorm(p = 0.9, mean = 1.08, sd = 0.73))
# 7.5, almost the same!

pnorm(q = 1, mean = 1.08, sd = 0.73)
# this is a different value from the gamma distribution