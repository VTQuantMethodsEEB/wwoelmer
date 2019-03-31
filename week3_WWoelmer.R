# lesson 3 assignment
# data visualization with github

library(tidyverse)

bvr <- read.csv("./BVR_CTD.csv")
bvr$Date <- as.Date(bvr$Date, format = "%Y-%m-%d")

unique(bvr$Date)

# plot changes in temperature over time based on depth
ggplot(data = bvr, aes(x =Date, y = Temp_C , color = Depth_m) ) + 
  geom_point()+
  scale_colour_gradient(trans = "reverse")##added this here
### I want the colors to be opposite here, so the high depths (deeper) are the darker colors

# now the same idea but with chlorophyll a
ggplot(data = bvr, aes(x = Date, y = Chla_ugL, color= Depth_m)) + geom_point()
# how to get a better color ramp? it's hard to tell the difference between depths

# I want to compare years at the same time scale now
# so create a 'year' and a month-day column
bvr$year <- format(as.Date(bvr$Date, format="%d/%m/%Y"),"%Y")
bvr$mo_day <- format(as.Date(bvr$Date, format="%d/%m/%Y"),"%m-%d")
bvr$mo_day <- as.Date(bvr$mo_day, format = "%m-%d")
#lubridate can be a better option for working with dates
library(lubridate)
#https://lubridate.tidyverse.org/

bvr$mo <- month(bvr$Date)
bvr$day <- day(bvr$Date)
#you could paste them together, or do some reading on lubridate

# any idea why this adds the '2019' onto the month-day? I haven't figured out a way to get it to
# stay month-day without the year being added
--### see above###---

ggplot(data = bvr, aes(x = mo_day, y = Chla_ugL, color = year)) + geom_point() 
# this plot is cool but doesn't show the differences as you go deeper into the reservoir
# so use facet wrap to separate by depth
ggplot(data = bvr, aes(x = mo_day, y = Chla_ugL, color = year)) + geom_point() + facet_wrap(~Depth_m)
# is there a way to select only some depths to plot, e.g., I want to make facet plots of just 
# 0.1, 1, 5, 8, and 12

ggplot(data = subset(bvr, Depth_m == 0.1| Depth_m== 1 | Depth_m == 5), aes(x = mo_day, y = Chla_ugL, color = year)) + 
         geom_point() + 
         facet_wrap(~Depth_m)
# is there a way to select only some depths to plot, e.g., I want to make facet plots of just 
# 0.1, 1, 5, 8, and 12

###you can add the subset command into ggplot as above