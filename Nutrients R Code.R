###R-Script for Carnivore and herbivore figures in Poop's Project (Jul 2020)###

#Packages to Load#
library(tidyverse)
library(ggplot2)
library(readxl)
library(lme4)
library(lmerTest)
library(car)
library(lme4)
library(MuMIn)
library(visreg)
require(glmmTMB)
require(cowplot)
library(quantreg)

###########
#files
###########

setwd("C:/Users/16047/Dropbox/2 - Manuscripts in prep/2 - submitted - Student - Kately and Jenny resource movement/revised manuscript")

herb_plotlevel <- read.csv("herb_nutrient_plotlevel.csv")
carn_sum <- read.csv("Carn_Sum_2020Apr15.csv")
colnames(carn_sum)[1]<-"site"

carn_each<-read.csv("carn_nutrient_2020Apr03.csv")

#per meter x area to scale to patch level
herb_patchlevel<-herb_plotlevel %>% group_by(site) %>%
  summarize(tot_weight=sum(weight_g),tot_N=sum(tot_N*16),area=mean(area)) #x16 to get per M

herb_patchlevel$patch_N<-herb_patchlevel$tot_N*herb_patchlevel$area

##################
#figure 3 analyses
##################

#quadrat level
h_patch<-as.data.frame(herb_plotlevel[,c("site","tot_N")])
colnames(h_patch)[2]<-"quadrat_N"
h_patch<-subset(h_patch,quadrat_N>0)
h_patch$type<-"herbivore"
c_patch<-as.data.frame(carn_each[,c("site","tot_N")])
colnames(c_patch)[2]<-"quadrat_N"
c_patch$type<-"carnivore"
all_quadrat<-rbind(h_patch,c_patch)
head(all_quadrat)

#patch level
carn_sum$site<-paste("R",carn_sum$site,sep="")
c_patch<-as.data.frame(carn_sum[,c("site","Sum_N")])
colnames(c_patch)[2]<-"patch_N"
c_patch$type<-"carnivore"
h_patch<-as.data.frame(herb_patchlevel[,c("site","patch_N")])
h_patch$type<-"herbivore"
all_patch<-rbind(h_patch,c_patch)
head(all_patch)

all_quadrat$type<-factor(all_quadrat$type, levels=c("herbivore","carnivore"))
all_patch$type<-factor(all_patch$type, levels=c("herbivore","carnivore"))

#plots
fig2a<-ggplot(data=subset(all_quadrat),aes(x=type,y=(quadrat_N+0.0001))) +  
  geom_boxplot(fill="navyblue",color="navyblue",alpha=0.2, outlier.shape = NA) +
  geom_jitter(size=2,color="#E66101",alpha=0.75, width=0.35) +
  theme_classic() +
  scale_y_log10() +
  labs(x="Consumer", y = "Nitrogen added (g/quadrat)")

fig2b<-ggplot(data=subset(all_patch),aes(x=type,y=(patch_N+0.2))) +  
  geom_boxplot(fill="navyblue",color="navyblue",alpha=0.1, outlier.shape = NA) +
  geom_jitter(size=2, color="#E66101",alpha=0.75,width=0.35) +
  theme_classic() +
  scale_y_log10() +
  labs(x="Consumer", y = "Nitrogen added (g/fragment)")

plot_grid(fig2a,fig2b, labels="auto")

#stats
lm1 <- glmmTMB(quadrat_N ~ type + (1|site), family=Gamma(link=log),data = all_quadrat)
summary(lm1)
Anova(lm1)
visreg(lm1,scale="response")

lm2 <- glmmTMB(patch_N+0.02 ~ type+(1|site), family=Gamma(link=log),data = all_patch) #needs site
summary(lm2)
Anova(lm2)
visreg(lm2,scale="response")


###################
#figure 2 analysis
###################

lm1 <- glmmTMB(tot_N+0.0001 ~ ndvi+slope + (1|site), family=Gamma(link=log),data = herb_plotlevel)
summary(lm1)
Anova(lm1)


hist(herb_plotlevel$res)
herb_plotlevel$res<-residuals(lm1)

lm1 <- glmmTMB(res ~ ndvi+slope + (1|site), data = herb_plotlevel)
summary(lm1)
Anova(lm1)
visreg(lm1,xvar="ndvi")

lm2 <- glmmTMB(Sum_N+0.2 ~ ndvi.m+slope, family=Gamma(link="log"),data = carn_sum)
Anova(lm2)
summary(lm2)

lm1.2 <- glmmTMB(tot_N+0.0001 ~ ndvi+slope + (ndvi|site), family=Gamma(link=log),data = herb_plotlevel)
summary(lm1.2)
Anova(lm1.2)


#Visreg 
a<-visreg(lm1, xvar="ndvi", scale="response")
b<-visreg(lm1, xvar="slope", scale="response")
c<-visreg(lm2, xvar="ndvi.m", scale="response")
d<-visreg(lm2, xvar="slope", scale="response")


#plotting
fig1a<-ggplot(a$fit, aes(ndvi, visregFit)) +
  geom_point(data=herb_plotlevel, aes(ndvi, tot_N, group=site), alpha=0.75, size=2, color = "#E66101") +
  theme_classic() +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr),fill="navyblue",alpha=0.1)+
  geom_line(aes(x=ndvi, y=visregFit),size=1.25,color="navyblue", linetype="dotted")+
  labs(x=NULL, y="Nitrogen added (g/quadrat)")

fig1b<-ggplot(b$fit, aes(slope, visregFit)) +
  geom_point(data=herb_plotlevel, aes(slope, tot_N, group=site), alpha=0.75, size=2, color = "#E66101") +
  theme_classic() +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr),fill="navyblue",alpha=0.1)+
  geom_line(aes(x=slope, y=visregFit),size=1.25,color="navyblue", linetype="solid")+
  labs(x=NULL, y="Nitrogen added (g/quadrat)")

fig1c<-ggplot(c$fit, aes(ndvi.m, visregFit)) +
  geom_line(aes(x=ndvi.m, y=visregFit),size=1.25,color="#E66101", linetype="dotted")+
  geom_point(data=carn_sum, aes(ndvi.m, Sum_N), alpha=0.75, size=2, color = "navyblue") +
  theme_classic() +
  labs(x="Productivity (NDVI)", y="Nitrogen added (g/fragment)") +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr),fill="#E66101",alpha=0.1)
  
fig1d<-ggplot(d$fit, aes(slope, visregFit)) +
  geom_point(data=carn_sum, aes(slope, Sum_N), alpha=0.75, size=2, color = "navyblue") +
  theme_classic() +
  labs(x="Slope", y="Nitrogen added (g/fragment)") +
  geom_ribbon(aes(ymin=visregLwr, ymax=visregUpr),fill="navyblue",alpha=0.1) +
  geom_line(aes(x=slope, y=visregFit),size=1.25,color="#E66101", linetype="dashed")

plot_grid(fig1a,fig1b,fig1c,fig1d,labels="auto")
ggsave("figure 1.pdf",w=7,h=6)


#quantile regression for Fig. S2
rqfit.95 <- rq(tot_N+0.0001 ~ ndvi, data = herb_plotlevel,tau=0.95)
rqfit.75 <- rq(tot_N+0.0001 ~ ndvi, data = herb_plotlevel,tau=0.75)
rqfit.50 <- rq(tot_N+0.0001 ~ ndvi, data = herb_plotlevel,tau=0.50)
rqfit.25 <- rq(tot_N+0.0001 ~ ndvi, data = herb_plotlevel,tau=0.25)
rqfit.05 <- rq(tot_N+0.0001 ~ ndvi, data = herb_plotlevel,tau=0.05)

plot((tot_N+0.0001) ~ ndvi, data = herb_plotlevel, pch = 16,ylab="Nitrogen added (g/quadrat)", xlab="Productivity (NDVI)")
abline(rqfit.95, col = "blue", lty = 3, lwd=2)
abline(rqfit.75, col = "blue", lty = 2, lwd=2)
abline(rqfit.50, col = "blue", lty = 1, lwd=2)
abline(rqfit.25, col = "blue", lty = 2, lwd=2)
abline(rqfit.05, col = "blue", lty = 3, lwd=2)

