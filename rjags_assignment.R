#This script should be run from the main qmee directory 
#this script uses the file "SpeciesStarvation_Cleaned_MA.rds" located in the main qmee directory 

library(R2jags)
library(tidyverse)
library(lme4)
library(broom.mixed)
library(dotwhisker)

#constructing named list
named_list <- namedList

D_SSD_Cond <- readRDS("SpeciesStarvation_Cleaned_MA.rds")
#str(D_SSD_Cond)

D_proDF <-D_SSD_Cond[D_SSD_Cond$species_full =="D_prolongata",]
#str(D_proDF)
#nrow(D_proDF)

Dpro_dat1 <- with(D_proDF,
                named_list(N=nrow(D_proDF),           
                           nsex=length(levels(sex)), 
                           sex=as.numeric(sex),
                           leg_tibL)) 

model1 <- function() {
  for (i in 1:N) { #likelihood
    mean[i] <- (b[sex[1]] + ifelse(sex[i]==1, 0, b[sex[i]]))
    leg_tibL[i] ~ dnorm(mean[i], tau)
  } #prior
  for (i in 1:nsex) {
    b[i] ~ dnorm(0,0.01)
  }
    tau ~ dgamma(0.0001, 0.0001)
    sigma <-  1/tau
  }

#Discussion of prior assumptions: 
#Treatment contrasts: 
#beta prior is normally distributed, ranging between 0 and 0.01 - 
#Difference of 0.01 cm between males and female tibia length is probably a reasonable prior estimate given what I know about variation in tibia length in Drosophila
#variation: Setting to a gamma distribution with a relatively small range. Not expecting much variation in tibia length, but assuming it will be positive. 
j1 <- jags(data=Dpro_dat1,
           inits=NULL,
           parameters=c("b", "sigma"),
           model.file=model1)

## extract Bayesian CIs
J1_CIs <- tidy(j1,conf.int=TRUE, conf.method="quantile")
dwplot(J1_CIs, by_2sd = TRUE) +
  geom_vline(xintercept = 0, lty = 2)

#equivalent frequentist fit 
model_lm <- lm(leg_tibL ~ sex, data = D_proDF)
summary(model_lm)
lm1_CIs <- tidy(model_lm, conf.int=TRUE, conf.method="quantile")
dwplot(lm1_CIs, by_2sd = TRUE) +
  geom_vline(xintercept = 0, lty = 2)

#Estimated change in mean tibia length between male and females in both the frequentist and bayesian models. 
#CIs for both models are almost identical.
#b2 lower credible interval is slightly underestimated relative to the frequentist model.
#CIs in neither model are close to zero. 
#Both models suggest a positive effect of sex on tibia length. 

