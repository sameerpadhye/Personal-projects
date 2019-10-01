#Mann Whitney U and Wilcoxon signed rank tests for non parametric data


#libraries used

library(readxl)
library(tidyverse)


# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)

#data file path (Here data_for_visualization data have been used)

data_path<-paste0(getwd(),'/data_for_visualizations.xlsx')


#Importing data for analysis 

data_analysis<-read_excel(data_path,
                          sheet=1)


#Exploring the data

head(data_analysis)


#1. Mann Whitney U test

# Here its assumed that in the data that has been used (given in the sample_datasets) consists of two independent samples (here Factor_A and Factor_B as 2 samples) and variable 1 (as var_1) as the response variable for the purpose of Mann Whitney U analysis 


#Obtaining the data

mann_whitney_data<-data_analysis%>%
  dplyr::filter(Factor %in% c('Factor_A','Factor_B'))%>%
dplyr::mutate_if(is.character,
                 as.factor)

#Test
 
mann_whitney_test<-wilcox.test(var_1~Factor,
                         data = mann_whitney_data)

#Results

mann_whitney_test


#2. Wilcoxon signed rank test

#For the purpose of the analysis using the same dataset,it will be assumed that Factor_B is the sample  while the responses at two time points 2 and 3 will be variable 2 and 3 respectively (as var_2 and var_3)

#Obtaining the data

wilcoxon_data<-data_analysis%>%
  dplyr::filter(Factor=='Factor_B')%>%
  dplyr::mutate_if(is.character,
                   as.factor)

#Test

wilcox_test<-wilcox.test(wilcoxon_data$var_2,
                         wilcoxon_data$var_3,
                         paired = TRUE)

#Results

wilcox_test
