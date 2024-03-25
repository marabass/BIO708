#This script should be run from the main QMEE directory

#loading libraries 
library(ggplot2); theme_set(theme_bw())
#data set 
D_SSD_Cond <- readRDS("SpeciesStarvation_Cleaned_MA.rds")

#extracting D pro data 
Dpro_size <- D_SSD_Cond[D_SSD_Cond$species_full =="D_prolongata",]
                        
#I am modelling the effect of thorax length (a continuous predictor) on tibia length: thorax_length_mm ~ leg_tibL
#I am using family=gaussian because my response variable is continuous. 
#Should probably understand what the log link function does here 
#Log link function performs a log transformation 
glm1 <- glm(leg_tibL ~ thorax_length_mm + sex, data = Dpro_size, family = gaussian(link="log"))

glm1_plot <- (ggplot(Dpro_size,aes(thorax_length_mm,leg_tibL, group = sex)) +
  geom_point() + 
  geom_smooth(method="glm",colour="red",
                         formula=y~x,
                         method.args=list(family="gaussian")))
#diagnostics 
plot(glm1)
plot(lm1)

#The curve in the residuals vs fitted plot suggests that it might be best to model the effect of thorax length on tibia length 
#using a quadratic distribution 

glm2 <- update(glm1,.~poly(thorax_length_mm,2))
plot(glm2)

#log2 trasnformation? 

#coefficient plot 
summary(m1)
