# Lesson 2 Assignment
# tidy data!

library(tidyverse)

# read in the CTD data
ctd <- read.csv("./CTD_Meta_13_17.csv")
# subset out only beaverdam reservoir data
bvr_ctd <- ctd[ctd$Reservoir=='BVR',]

site50 <- bvr_ctd[which(bvr_ctd$Site == 50), ]


# get rid of data that doesn't make sense (e.g., for when the ctd was above the surface)
for(i in 1:length(site50$Depth_m)){
  if(site50$Depth_m[i]<0){
    site50$Temp_C[i] = NA
  }
  if(site50$DO_mgL[i]<0){
    site50$Temp_C[i] = NA
  }
  if(site50$ORP_mV[i]<0){
    site50$Temp_C[i] = NA
  }
  if(site50$Cond_uScm[i]<0){
    site50$Temp_C[i] = NA
  }
}

#this for loop doesn't run for me. change in data frame? 

# WW: if spits out an error for ORP because there are no values less then 0
# but it should have gotten rid of the other unecessary data

#i also don't think you need a for loop for this, you should be able to write with square brackets 
#e.g. I think you could do:
#site50$Temp_C[site50$ORP_mV<0]=NA
#unique(site50$Temp_C)

layer = data.frame(site50)

#since the ctd takes so many measurements, select the measurement closest to these depths
# which match the chemistry data

layer1 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
layer2 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.0)))
layer3 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.0)))
layer4 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.0)))
layer5 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.0)))
layer6 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.0)))
layer7 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.0)))
layer8 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.0)))
layer9 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.0)))
layer10 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.0)))
layer11 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 10.0)))
layer12 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 11.0)))
layer13 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 12.0)))

#would ?cut or ?findInterval be helpful here?

## replace name of depth with the selected layer depths for ease of merging later
layer1$Depth_m <- 0.1 
layer2$Depth_m <- 1.0
layer3$Depth_m <- 2.0 
layer4$Depth_m <- 3.0 
layer5$Depth_m <- 4.0 
layer6$Depth_m <- 5.0 
layer7$Depth_m <- 6.0 
layer8$Depth_m <- 7.0 
layer9$Depth_m <- 8.0 
layer10$Depth_m <- 9.0 
layer11$Depth_m <- 10.0 
layer12$Depth_m <- 11.0
layer13$Depth_m <- 12.0

df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,
                 layer11,layer12,layer13)
bvr_layers_binned <- df.final[order(df.final$Date, df.final$Depth_m), ]

# that name is too long, make it short again
bvr <- bvr_layers_binned

# calculate specific conductivity for times that sp cond was not calculated 
bvr <- mutate(bvr, SpCond_calc = Cond_uScm/(1+(0.0191*(Temp_C - 25))))

# reorganize some columns
bvr <- bvr %>% select(-Spec_Cond_uScm, -Site, -Reservoir)

write.csv(bvr, "BVR_CTD.csv", row.names = FALSE)

# calculate the mean over the entire time series of chlorophyll on each day
mean <- bvr %>% group_by(Date) %>%
  summarise(mean_chl = mean(Chla_ugL))
#better to call this something other than mean because you can accidentally write over the mean function



