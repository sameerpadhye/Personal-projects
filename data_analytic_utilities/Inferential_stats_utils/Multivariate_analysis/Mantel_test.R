## Performing a Mantel test 

# Mantel tests are performed to check correlation (association) between two matrices. 

# The matrices are distance matrices and/or a 'dist' object.

# More information for the same can be found on https://stats.idre.ucla.edu/r/faq/how-can-i-perform-a-mantel-test-in-r/ OR
# https://mb3is.megx.net/gustame/hypothesis-tests/the-mantel-test


#Datasets can be of species distributions, abundances or spatial distances between localities of the species.

#Libraries used
library(tidyverse)
library(ade4)
library(ecodist)
library(vegan)

#library(sp) # in case spatialdataframe data is used

#Importing the first dataset. (Datasets here are considered to be of class dataframe). Here the first data is assumed to be locality data of species

data_file_1<-paste0(getwd(),"/data_matrix_1.csv")

data_matrix_1<-read.csv(data_file_1)

# Obtaining the distance matrix of localities (using GIS data). Distance matrix can also be obtained using 'sp' package using  the 'spDists' function. This function though, takes a spatialdataframe object as the data.

data_gis_dist<- dist(data_matrix_1[,c("Longitude","Latitude")])

#Importing the second dataset. Here the second dataset is considered to be environmental variables of the localities. 

#Locality data sequence must match between the two datasets

data_file_2<-paste0(getwd(),"/data_matrix_2.csv")

data_matrix_2<-read.csv(data_file_2)%>%
    select_if(is.numeric) # numeric data selected for analysis
    
#Converting the environmental data into a distance object
data_env_dist<-vegdist(data_matrix_2,
                       method = 'gower', # this will change as per the data structure
                       na.rm = TRUE) # if there are NA values in the data

#The above can also be done by using 'dist' function

##There are many packages which offer mantel test analyses. Here, functions from 3 packages are given:

#1. Mantel test (vegan)
vegan::mantel(data_gis_dist, # data matrix 1 (gis)
              data_env_dist, # data matrix 2 (environment)
              method="spearman", # type of correlation coefficient
              permutations = 4999)  #permutation number for testing significance of correlation

#2. Mantel test (ade4)
mantel.rtest(distance_data, # data matrix 1 (gis)
             chislop_env_dist, # data matrix 2 (environment)
             nrepet = 9999)  #permutation number for testing significance of correlation

#3. Mantel test (ecodist)


ecodist::mantel(formula = distance_data~chislop_env_dist, ## both the datasets
                nperm = 1000,#permutation number for testing significance of correlation
                nboot = 500)

#Please note that there are other arguments which can be passed for mantel tests from different packages which are not provided here
