#Canonical correlation analysis is a multivariate technique used to explore associations between the variables of two different datasets. 
#More information can be obtained from https://stats.idre.ucla.edu/r/dae/canonical-correlation-analysis/

#libraries used
library(readxl)
library(tidyverse)
library(vegan)
library(CCA)

#file path
data_file<-"C:/Users/samee/Desktop/R data/env_data.xlsx"

#Importing sample Environmental data (100 samples)
env_data<-read_excel(data_file,
                         sheet=1)%>%
    select_if(is.numeric) #only numeric data should be selected

#Here, a sample generated dataset of species abundances has been used but the species data can be imported in the same way as environmental data    

species_data<-data.frame(cbind(sp_1=sample(1:1000,
                                           100,
                                           replace=T),
                    sp_2=sample(1:100,
                                100,
                                replace=T),
                    sp_3=sample(1:5000,
                                100,
                                replace=T),
                    sp_4=sample(1:50,
                                100,
                                replace=T)))    

#In case the data contains categorical data, care should be taken to select only numerical data for analysis

#Canonical correlation can be done using base R function 'cancor' as well as using some packages such as 'CCA' and 'vegan'. Analysis using these packages is provided below

#1. Canonical correlation using 'CCA' package

#correlations of variables within each datasets and between the two datasets
correlation_summary<-CCA::matcor(env_data,
                                       species_data)

#Visualizing all the correlations 
CCA::img.matcor(correlation_summary)

##Canonical correlation analysis
canonical_correlation<-CCA::cc(env_data, # numerical data can be selected at this step as well
                               species_data)

#obtainin the correlation (coe) of species
canonical_correlation$cor

#finding the names of all components of both the datasets
canonical_correlation$names

# Plot to visualize the Canonical correlation analysis
plt.cc(canonical_correlation, 
       var.label = TRUE)


#2.Canonical correlation using 'vegan' package (W.I.P.)

canonical_correlation_2<-CCorA(env_data,
                               species_data,
                               stand.X = TRUE)

#Value for significance of the correlation
canonical_correlation_2$Pillai

#Biplot of the CCA
biplot(canonical_correlation_2,which=1:2)
