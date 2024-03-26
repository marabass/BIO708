#This script should be run from the main QMEE directory

#loading libraries 
library(ggplot2); theme_set(theme_bw())
library(DHARMa)
library(dotwhisker)
library(emmeans)

#data set 
D_SSD_Cond <- readRDS("SpeciesStarvation_Cleaned_MA.rds")

#extracting D pro data 
Dpro_size <- D_SSD_Cond[D_SSD_Cond$species_full =="D_prolongata",]
                        
#I am modelling the effect of thorax length (a continuous predictor) on tibia length: thorax_length_mm ~ leg_tibL
#I am using family=gaussian because 
#I am converting milimeters to micrometers, then performing a log2 transformation of my continuous response and predictor
#I am using a standard Log link function as per the assumption that my data is normally distributed. 
#The log link function performs a log transformation of my predictor

glm1 <- glm(log2(1000 * Dpro_size$leg_tibL) ~ log2(1000 * thorax_length_mm) + sex, data = Dpro_size, family = gaussian(link="log"))

glm1_plot <- (ggplot(Dpro_size,aes(log2(1000 * thorax_length_mm),log2(1000 * Dpro_size$leg_tibL), group = sex, colour = sex)) +
  geom_point() + 
  geom_smooth(method="glm",colour="red",
                         formula=y~x,
                         method.args=list(family="gaussian")))
print(glm1_plot)
#diagnostics 
plot(glm1)

plot(simulateResiduals(glm1))

#coefficient plot 
summary(glm1)
dwplot(glm1)
plot(emmeans(glm1, specs = "sex", type = "response"))

