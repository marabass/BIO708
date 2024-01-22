# BIO708

The first script, DataCleanUp.R takes as input the csv file containing data from the species starvation experiment (MP_SpeciesStarvation_Clean.csv). The code examines the structure and classes of the data frame, generates a plot that compares the number of observations for each species, and removes one species (D_orena) with a low number of observations (n = 13), relative to other species in the data set. The script also generates subsets of the data organized in tables showing size values for each trait. Finally, the script generates an RDS file containing the 'cleaned' data set. The script is located in my main QMEE directory and should be run from the main QMEE directory. 

The second script, WingsizeConditionPlot.R takes as input the RDS file containing the 'cleaned' species starvation data (MP_SpeciesStarvation_Cleaned_MA.rds). The code generates a boxplot with jittered points showing the distribution of the  mean wing area in micrometers for each species at both condition levels. The script is in my main QMEE directory and should be run from the main QMEE directory. 

One of the major questions I plan on addressing using this data set is whether we see a different pattern of condition-dependent sexual dimorphism in traits exhibiting male- vs female-biased dimorphism. Some of the steps I can take to carry out this investigation are as followss: 
- Generate tables for each species, showing condition (HC and LC), sex, and size measurements for each specimen. These tables could be used to compute counts for condition, sex, and species-, sex-, and condition-specific means for size data. These tables should be assigned to variables and saved. 
- Comparisons between sex-specific means for each trait could be used to determine the direction of the size dimorphism for each trait in the species. 
- I can then compare the relationship between condition and size dimorphism for male- and female-biased traits across the species in the phylogeny. 