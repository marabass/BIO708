#This script should be run from the main QMEE directory 
#This script uses a data set: "SpeciesStarvation_Cleaned_MA.rds" located in the main QMEE directory

#load packages
library(tidyverse)

#Read in the 'clean' data set 
Size_data <- readRDS("SpeciesStarvation_Cleaned_MA.rds")
str(Size_data) #checking that all variables are the right type (Sex and condition are factors)

#'SpeciesPhyOrd' orders of species in the data frame by phylogenetic relationship. To be used for re-ordering levels in 'species_full'
#Closely related species are beside each other. # The phylogeny used to determine relatedness is in my main QMEE repo ("DmelSp_phylogeny.png")

## BMB: it would be best to do this directly from the phylogeny (we
##  can show you how to do this ... Also, possibly best to do it upstream
##  in the data-cleaning step, unless this is going to vary from one
##  analysis to another
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
## BMB: could use summarise(across(...)) and then rename_with()

WingMean <- (Size_data
                   %>% group_by(species_full, condition, sex)
                   %>% summarise(m_wing_area_sq = mean(wing_area_mm_sq), 
                                 m_wing_log_area_mm_sq = mean(wing_log_area_mm_sq), 
                                 m_wing_sqroot_area_mm_sq = mean(wing_sqroot_area_mm_sq), 
                                 m_wing_log_sqroot_area_mm_sq = mean(wing_log_sqroot_area_mm_sq))
    %>% mutate(species_full=factor(species_full, levels = SpeciesPhyOrd)))

## BMB: ditto. Note that log(sqrt(area)) is *identical* to log(area)/2 ...

# Generating a boxplot to look at the distribution of values for tibia length across across the sexes and condition levels. 
# Does using untransformed vs Log2 transformed values address skewed data caused by very high values recorded for male HC and LC specimens. 

TibiaL_CD_BaseBox <- (ggplot(Size_data)
                   + aes(x=condition, y=leg_tibL, color=sex))
TibiaLogL_CD_BaseBox <- (ggplot(Size_data)
                      + aes(x=condition, y=leg_log_tibL, color=sex))

TibiaL_CD_Box <- (TibiaL_CD_BaseBox
                  + geom_boxplot()
                  + labs(y="Tibia length (µm)"))
print(TibiaL_CD_Box)

## BMB: probably best to do log2-scale like this, unless log2 values
## are really meaningful to you and your readers
TibiaL_CD_Box + scale_y_continuous(trans = "log2")

TibiaLogL_CD_Box <- (TibiaLogL_CD_BaseBox
                     + geom_boxplot()
                     + labs(y="Tibia length (log2 µm)"))
print(TibiaLogL_CD_Box) 

#Using 'TibiaMean' to generate a line plot that shows the mean of tibia length (log2 µm) at high condition and low condition for males and females of each species.  
# log2 transformed values were used because comparisons of the two box plots shows that log transformation if the data slightly reduces skews in the data caused by large values in males.  
TibiaLogL_CD_BaseLine <- (ggplot(TibiaMean)
                         + aes(x=condition, y=mean_leg_log_tibL, colour = sex, group = (sex)))
TibialogL_CD_line <- (TibiaLogL_CD_BaseLine
      + geom_line()
      + geom_point()
      + facet_wrap(~species_full)
      + labs(y="Mean tibia length (log2 µm)")
      ) 
print(TibialogL_CD_line)

#Using 'WingMean' to generate a line plot that shows the mean wing area (log2 √µm) for HC and LC male and female files in each species.
##log2 √µm values were used to standardize the wing size measurements to allow for a visual comparison between the effect of species starvation on tibia length and wing size

## BMB: you might want to reorder the species by the magnitude of dimorphism ... it might also be easier to distinguish if you used different shapes as well as different colours by sex

WingLogA_CD_BaseLine <- (ggplot(WingMean)
                         + aes(x=condition, y=m_wing_log_area_mm_sq, colour = sex, group = (sex)))
WingLogA_CD_line <- (WingLogA_CD_BaseLine
                      + geom_line()
                      + geom_point()
                      + facet_wrap(~species_full)
                      + labs(y="Mean wing area (log2 √µm)")) 
print(WingLogA_CD_line)

## mark: 2




