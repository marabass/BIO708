#This script should be run from the main QMEE directory 

#load packages
library(tidyverse)

#Read in the 'clean' data set
Size_data <- readRDS("SpeciesStarvation_Cleaned_MA.rds")
str(Size_data) #checking that all variable are the right type (Sex and condition are factors)

## Making a data frame that has mean tibia length and width for males and females of each species at high condition (fully fed) and low condition (starved for 24 or 48 hours during development, depending time of development)

#list containing order of species in the data frame by phylogenetic relationship for re-ordering levels in 'species_full'
#Closely related species are beside each other. # The phylogeny used to determine relatedness is in my main QMEE repo. File name: 

SpeciesPhyOrd <- c("D_santomea", "D_yakuba", "D_teissieri", "D_erecta", "D_simulans", "D_mauritiana", "D_sechellia", "D_melanogaster", 
                   "D_eugracilis","D_pseudotakahashii", "D_takahashii", "D_suzukii", "D_biarmipes", 
                   "D_prolongata", "D_fuyamai","D_elegans", "D_ficusphila", "D_serrata", "D_birchii", 
                   "D_kikkawai", "D_rufa", "D_parabipectinata", "D_bipectinata", "D_malerkotliana", 
                   "D_pseudoananassae", "D_ananassae")
TibiaMean <- (Size_data
              %>%group_by(species_full, condition, sex)
              %>%summarise(mean_leg_tibL = mean(leg_tibL), mean_leg_tibW = mean(leg_tibW),
                           mean_leg_log_tibL = mean(leg_log_tibL), mean_leg_log_tibW = mean(leg_log_tibW))
              %>% mutate(species_full=factor(species_full, levels = SpeciesPhyOrd)))
WingMean <- (Size_data
                   %>% group_by(species_full, condition, sex)
                   %>% summarise(m_wing_area_sq = mean(wing_area_mm_sq), 
                                 m_wing_log_area_mm_sq = mean(wing_log_area_mm_sq), 
                                 m_wing_sqroot_area_mm_sq = mean(wing_sqroot_area_mm_sq), 
                                 m_wing_log_sqroot_area_mm_sq = mean(wing_log_sqroot_area_mm_sq))
                   %>% mutate(species_full=factor(species_full, levels = SpeciesPhyOrd)))

#using TibiaMean to generate a box plot to show the distribution of leg tibia length values across the species. 

# Generating a boxplot to look at the distribution of tibia length. Looking at what factors might be most affecting the spread of data

TibiaL_CD_BaseBox <- (ggplot(Size_data)
                   + aes(x=condition, y=leg_tibL, color=sex))
TibiaLogL_CD_BaseBox <- (ggplot(Size_data)
                      + aes(x=condition, y=leg_log_tibL, color=sex))

TibiaL_CD_Box <- (TibiaL_CD_BaseBox
                  + geom_boxplot()
                  + labs(y="Tibia length (µm)"))
print(TibiaL_CD_Box)

TibiaLogL_CD_Box <- (TibiaLogL_CD_BaseBox
                     + geom_boxplot()
                     + labs("Tibia length (log2 µm)"))
print(TibiaLogL_CD_Box) 

#using 'TibiaMean' to generate a line plot that shows the mean of log tibia length (in micrometers) at high condition and low condition for males and females of each species.  
#I used the log measurements because there is such a wide range in the data. 
TibiaLogL_CD_BaseLine <- (ggplot(TibiaMean)
                         + aes(x=condition, y=mean_leg_log_tibL, colour = sex, group = (sex)))
TibialogL_CD_line <- (TibiaLogL_CD_BaseLine
      + geom_line()
      + geom_point()
      + facet_wrap(~species_full)
      + labs(y="Mean tibia length (log2 µm)")
      ) 
print(TibialogL_CD_line)

WingLogA_CD_BaseLine <- (ggplot(WingMean)
                         + aes(x=condition, y=m_wing_log_area_mm_sq, colour = sex, group = (sex)))
WingLogA_CD_line <- (WingLogA_CD_BaseLine
                      + geom_line()
                      + geom_point()
                      + facet_wrap(~species_full)
                      + labs(y="Mean wing area (log2 √µm)")) 
print(WingLogA_CD_line)






