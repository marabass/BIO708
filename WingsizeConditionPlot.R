#load library
library(tidyverse)

#read in the cleaned data ste 
Size_dataClean <- readRDS("MP_SpeciesStarvation_Cleaned_MA.rds")

summary(Size_dataClean)
#plot - plotting mean wing size per species against condition: How does wing size change as a function of condition, across all species?   

wing_size_data_condition <- (Size_dataClean
                             %>% group_by(species_full, condition)
                             %>% summarise(mean_wing_size = mean(wing_area_mm_sq)))

ggplot(wing_size_data_condition, mapping = aes(x = condition, y = mean_wing_size)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(alpha = 0.3, aes(colour = species_full))
