#This script should be run from the main QMEE directory
#This script uses the data set 'SpeciesStarvation_Cleaned_MA.rds' located in my main QMEE directory 

#loading libraries 
library(dotwhisker)
library(emmeans)

#data set 
D_SSD_Cond <- readRDS("SpeciesStarvation_Cleaned_MA.rds")

#extracting D pro data 
Dpro_size <- D_SSD_Cond[D_SSD_Cond$species_full =="D_prolongata",]

#hypothesis: 1. larger flies will have longer forelegs. 2. Body size is a greater determinant of foreleg length than sex. 
#I am modelling the effect of thorax length and sex on tibia length
#I am using a Log link function as per the assumption that the relationship between thorax length, sex, and tibia length follows a gaussian distribution
#I am converting the units of my continuous variable from millimeters to micrometers, then performing a log2 transformation of my continuous response and predictor variables
#The log2 transformation is to help frame comparisons between thorax and tibia length in terms of size doubling. 

glm1 <- glm(log2(1000 * leg_tibL) ~ log2(1000 * thorax_length_mm) + sex, data = Dpro_size, family = gaussian(link="log"))

#diagnostics 
plot(glm1)

##Discussion of diagnostic plots
#None of my diagnostic plots would suggest that the model is a poor fit for my data. The QQ plot show that there is a positive skew in my residuals.  
#However, I am going to assume that my model is robust to deviations from normality. 

#inferential plots 
summary(glm1)
dwplot(glm1)
#The effect of thorax length of tibia length is positive and the confidence interval does not include zero. This indicates that larger flies have larger forelegs. 
#The effect of sex on tibia length is positive, indicating that males have larger tibias than females. 
#The effect of sex on tibia length is smaller than the effect of thorax length. However, the confidence intervals of both predictors almost entirely overlap. I cannot be sure that the difference between the effects of thorax length and sex on tibia length is biologically relevant. 

