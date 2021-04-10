#2020Jul14
#Mosaic Plots for Germination Trial

#packages
require(cowplot); require(dplyr)

#data folder
setwd("C:/Users/rache/Dropbox/2 - Manuscripts in prep/Student - Kately and Jenny resource movement/Scripts for Rachel")

#germination data
HerbCarnCategoricals<-read.csv("HerbCarnCategoricals.csv",head=TRUE)
colnames(HerbCarnCategoricals)[1]<-"disperser_type"
head(HerbCarnCategoricals)
HerbCarnCategoricals$serp_or_nonserp<-ifelse(HerbCarnCategoricals$serp_or_nonserp=="S","serpentine","non-serpentine")
HerbCarnCategoricals$native_or_exotic<-ifelse(HerbCarnCategoricals$native_or_exotic=="native","native","non-native")

HerbCarnCategoricals$native_or_exotic<-factor(HerbCarnCategoricals$native_or_exotic, levels=c("non-native","native"))
HerbCarnCategoricals$disperser_type<-factor(HerbCarnCategoricals$disperser_type, levels=c("herbivore","carnivore"))

#herbivore nutrient
herb_plotlevel <- read.csv("herb_nutrient_plotlevel.csv")
head(herb_plotlevel)

#converting to m2
herb_plotlevel$weight_m2<-herb_plotlevel$weight_g*(1/0.0625)
herb_plotlevel$scat_m2<-herb_plotlevel$scat*(1/0.0625)
range(herb_plotlevel$scat_m2); median(herb_plotlevel$scat_m2)

mean(herb_plotlevel$tot_N) #0.01 reported in manuscript (rounded)

#carnivore nutrient
carn_sitelevel<-read.csv("carn_nutrient_2020Apr03.csv")
head(carn_sitelevel)
carn_sitelevel$tot_N_m2<-carn_sitelevel$tot_N #this is the amount per m2

carn_sum<-carn_sitelevel %>% group_by(site) %>%
  summarise(weight_g=sum(weight_g),tot_N=sum(tot_N), mean_perquadrat=mean(tot_N))

mean(carn_sum$mean_perquadrat) #2.46 reported in results
sum(carn_sitelevel$tot_N) #22.14 reported in results


########################################
#scaling of N at small and large scales
########################################

#area info
h.sampled.area<-20*0.0625  
total.area<-sum(unique(herb_plotlevel[c("site", "area")])[2])

perfragment.area<-herb_plotlevel %>% group_by(site) %>%
  summarise(area=unique(area),weight_g=sum(weight_g), dec_N=mean(dec_N))

mean(perfragment.area$weight_g)

perfragment.area$h.multiplier<-(perfragment.area$area/h.sampled.area)
perfragment.area$tot.weight_g<-perfragment.area$h.multiplier*perfragment.area$weight_g
perfragment.area$tot.N_g<-perfragment.area$tot.weight_g*perfragment.area$dec_N

sum(perfragment.area$tot.N_g) #604 g total reported in ms

h.total.area<-sum(perfragment.area$area)
h.total.area

100*((10*h.sampled.area)/h.total.area) #% area of landscape sampled

h.total.weight<-sum(perfragment.area$tot.weight_g)
h.total.weight

h.extra <- 11.27 #grams
h.total <- sum(herb_plotlevel$weight_g)
h.used <- h.total-h.extra

h.seeds.totalsample<-18*(h.total/h.used) #total seeds in all poop sampled

multiplier.bypoop<-h.total.weight/h.total

#amount of seeds added by herbivores to all 10 fragments, at intantaneous moment in time
h.seeds.totalsample*multiplier.bypoop #7922 seeds total reported in ms



#total area of habitat patch to get totals per patch

c.extra<-294.21 #chelsea's weights
c.total<-sum(carn_sitelevel$weight_g) 
c.used<-c.total-c.extra

c.seeds.total<-9*(c.total/c.used)


#Figures for germination trial data
par(mgp=c(1,0.5,0),mfrow=c(1,2), mai = c(0.5, 0.5, 0.5, 0.5))

Germination_Trial_Data2 <- table(HerbCarnCategoricals$disperser_type, HerbCarnCategoricals$native_or_exotic)
mosaicplot(Germination_Trial_Data2, main=NULL, color = c("#fdb863", "#e66101"))
+ axis(4, seq(0, 1, by = 0.2),pos=1, cex.axis=0.75) + axis(1, seq(0, 1, by = 0.2), las=1, pos=-0.05, cex.axis=0.75) 
mtext("c", at=-0.1, cex=1)

Germination_Trial_Data1 <- table(HerbCarnCategoricals$disperser_type, HerbCarnCategoricals$serp_or_nonserp)
mosaicplot(Germination_Trial_Data1, main=NULL, color = c("#fdb863", "#e66101"))
+ axis(4, seq(0, 1, by = 0.2),pos=1, cex.axis=0.75) + axis(1, seq(0, 1, by = 0.2), las=1, pos=-0.05, cex.axis=0.75) 
mtext("d", at=-0.1, cex=1)

jpeg("fig 2.jpg")
dev.off()

fisher.test(Germination_Trial_Data2)
fisher.test(Germination_Trial_Data1)
