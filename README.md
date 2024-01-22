# BIO708

The first script, DataCleanUp.R takes as input the csv file containing data from the species starvation experiment. The code examines the structure and classes of the data frame, generates a plot that compares the number of observations for each species, and removes one species (D_orena) with a low number of observations (n = 13) relative to the rest of the species. The script also generates some tables that subset the data in a manner that corresponds to how I plan to visualize/analyze the data. Finally, the script generates a RDS file containing the 'cleaned' data set. The script is located in my main QMEE directory and should be run from the main QMEE directory. 

The second script, WingsizeConditionPlot.R takes as input the RDS file containing the 'cleaned' species starvation data set. The code generates a boxplot that looks at the distribution of mean wing area in micrometers squared for each species across both condition levels. the script is in my main QMEE directory and should be run from the main QMEE directory. 


