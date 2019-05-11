rm(list=ls())
##Loading the necessary packages

library(ggplot2)
library(gridExtra)
library(viridis)
library(MASS)
library(effects)
library(emmeans)
library(reshape2)
library(tidyverse)
library(car)
library(AICcmodavg)
library(qpcR)

camdat=read.csv("Camera_Data.csv")
head(camdat)
##Isolating species of interest.Plotting species on the x-axis,y= # of animals,color=camera.
g1=ggplot(data=camdat,aes(x=Species,y=animals))+
  geom_boxplot()+
  geom_point(aes(color=as.factor(Camera)),size=2,shape=1,stroke=1)+#shape specifies donut, and stroke changes thickness
  xlab("")+
  scale_colour_viridis(discrete = T)+
  theme_bw()+
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
        legend.position="top",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white"))
g1
##Plotting the animals and time they were seen
g2=ggplot(data=camdat,aes(x=Camera,y=Light,color=Species))+
  geom_point(size=2)
g2

r1=aggregate(animals~Camera+Light,FUN=sum,data=camdat)
day.animals = r1$animals[r1$Light=="Day"]
night.animals = r1$animals[r1$Light=="Night"]
day.animals
night.animals

population <- data.frame(
  time=rep(c("day.animals","night.animals"),
           c(length(day.animals), length(night.animals))),
  Animals=c(day.animals,night.animals)
)
population

theme_set(theme_bw())

##Shapiro-Wilk Test- Is my data normally distributed for my species of interest?
swt<-shapiro.test(population$Animals)
swt
##Checking the animals seen during the day
swt_day<-shapiro.test(day.animals)
swt_day
##Checking the animals seen at night
swt_night<-shapiro.test(night.animals)
swt_night
#Data is not normally distributed because the p-value is less than 0.05
#Since I have count data and it is not normally distributed, I will be running negative binomial generalised linear on it
g3 = glm.nb(animals~Light,data=camdat);
summary(g3)
#g1 shows the slope of the line.
##Plotting the effects
library(effects)
plot(allEffects(g3))
##Using emmeans to quantify the number of animals
library(emmeans)
emmeans(g3, pairwise~Light)
emmeans(g3, pairwise~Light, type="response")

#running the negative binomial glm with species as weights
camdat$Camera= as.factor(camdat$Camera)
unique(camdat$Species)
unique(camdat$Light)
dim(camdat)

#unwanted species
camdat2 = camdat%>%
  filter(Species!="ACR")%>%
  filter(Species!="ADU")%>%
  filter(Species!="AFE")%>%
  filter(Species!="ARB")%>%
  filter(Species!="CAL")%>%
  filter(Species!="CIV")%>%
  filter(Species!="FAM")%>%
  filter(Species!="HAG")%>%
  filter(Species!="MEM")%>%
  filter(Species!="MRN")%>%
  filter(Species!="NAT")%>%
  filter(Species!="PAL")%>%
  filter(Species!="PR")%>%
  #took these out because never seen at some times
  filter(Species!="HIR")%>%
  filter(Species!="CAB")%>%
  filter(Species!="ANU")%>%
  filter(Species!="SAN")%>%
  filter(Species!="SEM")%>%
  #remove blank animals
  drop_na(Species, Light, animals)


dim(camdat2)

which(!complete.cases(camdat2$Species))
which(!complete.cases(camdat2$Light))
which(!complete.cases(camdat2$animals))

camdat2$Species = relevel(camdat2$Species, ref = "SAP")
g4 = glm.nb(animals~Species*Light,data=camdat2);
summary(g4)

getOption("max.print")
options(max.print=99999)

emmeans(g4,pairwise ~ Species*Light, type = "response")

#g4 showing the slope of the line on an additive model of species and light
#Plotting the underlying aggregated data 
library(ggplot2)
plot1=ggplot(data=camdat2,aes(x=Species,y=animals,color=Light))+ #aggregated data file
  geom_jitter(width=0.2)  #add real data points to plot
#geom_boxplot(data=camdat2, aes(x=Light,y=animals,col = Species)) #plot smooth lines from dat.new
plot1
##Using expand grid and y-hats to fit the data and show he relationship between the vectors
#dat.new=expand.grid(Light= unique(camdat$Light),
#                    Species = unique(camdat$Species))
#predict(g4,newdata = dat.new)

#Adding predict and response to fill the "holes" using yhat
#dat.new$yhat  = predict(g4,type="response")#,newdata = dat.new
camdat2$yhat = predict(g4,type="response")

head(dat.new)
##Plotting the underlying and the new model
plot1=ggplot(data=camdat2,aes(x=Species,y=animals,color=Light))+
  geom_point(position = position_dodge(width=.2))+
  #geom_jitter(width=0.2,size=2,shape =1, position = "dodge") +
  #geom_line(data=dat.new, aes(x=Light,y=yhat,col = Species))+
  geom_boxplot(aes(x=Species,y=yhat,col = Light),position = position_dodge(width=.2))
plot1

##Running Likelihood Ratio Tests on my data
l1 = glm.nb(animals~Light,data = camdat)
l2 = glm.nb(animals~Light*Species,data = camdat)
l3 = glm.nb(animals~1,data = camdat) #My null model

#Testing Light vs Light + Species
anova(l1,l2)
#Testing Light vs Light *Species
anova(l1,l3)
#Looking at all 3
anova(l1,l2,l3)

#anova works slightly differently depending on which models we are comparing
h1 = glm.nb(animals~Light*Species,data = camdat)
h2 = glm.nb(animals~Light+Species,data = camdat)
#Running a glm for negative binomial
anova(h2,h1)

h1 = glm.nb(animals~Light*Species,data = camdat)
h2 = glm.nb(animals~Light,data = camdat)
h3 = glm.nb(animals~1,data = camdat)
#simple version
AIC(h1,h2,h3)
#h1 is the best supported model
#tabular
aictab(cand.set=list(h1,h2,h3),modnames=c("h1","h2","h3"))#AIC table

#this function will give a nice AIC table, but calculating weights and delta AIC is very straightforward

#here all of the weight is for the first model
#lPlotting the AIC table manually
n1 = glm.nb(animals~Camera,data = camdat)
n2 = glm.nb(animals~Species*Camera,data = camdat)
n3 = glm.nb(animals~1,data = camdat2)#Null model

#for AICc
n=nrow(camdat2)#or whatever the length of your df is
tabA = AIC(n1,n2,n3)
#it would be nice to have AICC for a dataset this small
tabA$k<-c(n1$rank,n2$rank,n3$rank)
tabA$aiccs<-tabA$AIC+((2*tabA$k*(tabA$k+1))/(n-tabA$k-1))
#now order from smallest to biggest
tabA=tabA[order(tabA$aiccs),]
#calculate delta AIC
tabA$dAIC = tabA$aiccs - min(tabA$aiccs)
#you use the next two lines to get weights
tabA$edel<-exp(-0.5*tabA$dAIC) 
tabA$wt<-tabA$edel/sum(tabA$edel)
tabA

##Saving file
ggsave(file="/Users/njambi/Desktop/Ethiopia Data/Njambi.pdf", 
       plot=g1,
       width=7,height=7,units="in",
       useDingbats=FALSE) 



