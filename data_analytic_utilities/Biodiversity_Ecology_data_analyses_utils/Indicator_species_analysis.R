## Calculating the A) index of association between groups of sites and corresponding species occurrences and B) Indicator species for specific locality groups


#Libraries used

library(tidyverse)


# data file path (community_data from sample datasets used for the following analysis; file assumed to be stored in the working directory)


data_file_path<-paste0(getwd(),"/community_data.csv")


#Importing the dataset using the data file path


data_analysis<-read.csv(data_file_path)


#Exploring the data


head(data_analysis,5)


# Finding the number of groups (of sites)


table(data_analysis$Groups)


#Exploring the data: Finding the total species number in each of the groups


species_total_groups<-aggregate(subset(data_analysis,
                                  select = Species_1:Species_10), # Numerical data selected
                           by = list(data_analysis$Groups), # the category for which function is to be used
                           FUN = sum) # function to be used
    

species_total_groups


#A) Index of association


# The index of association is named Phi index of association and is calculated using the package indicspecies and the function 'multipatt'


if(!require(indicspecies))install.packages('indicspecies')


species_associations<-indicspecies::multipatt(subset(data_analysis,
                                   select = Species_1:Species_10), # the species community dataset (sites X species)
                            cluster=data_analysis$Groups, # the sites grouping  
                            func = "r.g", # how the index will be calculated (based on the species data and number of sites in each group)
                            control = how(nperm = 999)) # number of permutations for calculating the 'p' value


# Obtaining the results


summary(species_associations)


#B) Indicator species 


# The same indicspecies package is used to find out the indicator species for the specified groups of localities using the 'indicator' function


species_indicators<-indicspecies::indicators(subset(data_analysis,
                                                    select = Species_1:Species_10),# the species community dataset (sites X species)
                                             cluster=data_analysis$Groups, 
                                             group = 'A', # The group for which the indicator species combination is sought
                                             func = "IndVal.g", # how the index will be calculated (based on the species data and number of sites in each group)
                                             alpha=0.01, # the cutoff p value
                                             nboot = 499) # bootstrap number for confidence interval calculation


# Obtaining the results 


print(species_indicators)


#For more information on the additional arguments used in the functions and a general explaination of the results, please refer to 
# https://cran.r-project.org/web/packages/indicspecies/indicspecies.pdf
