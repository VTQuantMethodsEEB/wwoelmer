# Lesson 2 Assignment
# tidy data!

# read in the CTD data
ctd <- read.csv("./CTD_Meta_13_17.csv")
# subset out only beaverdam reservoir data
bvr_ctd <- ctd[ctd$Reservoir=='BVR',]