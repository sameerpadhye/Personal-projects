# Visualizing species occurrences using treemaps

# Common libraries used

library(tidyverse)

library(readxl)

# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-paste0(getwd(),"/species_richness_data.xlsx")

#Importing data for analysis (Species abundance dataset used here)

data_analysis<-read.csv(data_path)

# Visualization of species richness using a treemap. Please note that if the collections sites/localities are many, treemaps might not be the ideal visualization technique. 

# Data are converted to long form

require(treemap)

data_analysis%>%
    tidyr::gather(sp_names,occurrences,Species_1:Species_10)%>%
    dplyr::filter_at(vars(contains("occurrence")),
                     all_vars(. != 0))%>%  # remove the zeros to get only presences for calculating total occurrences
    dplyr::group_by(sp_names)%>%
    dplyr::summarise(Species_number=n())%>%
    tidyr::unite("Species_occurrence", # to generate a species name title for each facet of the tree map
             sp_names:Species_number,
             sep=":",
             remove=FALSE)%>%
    treemap(., 
            index=c("Species_occurrence"),  
            vSize = "Species_number",  
            type="index", 
            palette = "Set2",  
            title="Species occurrences", 
            fontsize.title = 12)
