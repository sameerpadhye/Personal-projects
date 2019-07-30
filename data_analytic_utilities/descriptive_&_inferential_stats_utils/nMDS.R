## Basic Non metric dimensional scaling (nMDS) analysis

#libraries used

library(vegan)
library(tidyverse)
library(readxl)
library(magrittr)


# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-paste0(getwd(),"/nMDS_data.xlsx")


#Importing data for analysis (nMDS dataset used here)

data_analysis<-read_excel(data_path,
                          sheet=1)%>%
    mutate_at('Groups',
              as.factor)


#Exploring the data

head(data_analysis,5)


#nMDS is performed using vegan package 

data_nmds<-vegan::metaMDS(data_analysis%>%
                              dplyr::select_if(is.numeric), # selecting the numeric data
                          distance = 'bray', #bray is used here since the data are abundance data 
                          k=2, # number of dimensions
                          trymax = 199) # number of random starts


#Stressplot of the nMDS

stressplot(data_nmds)


# nMDS plot

# to obtain the number of observations per Groups factor

table(data_analysis$Groups)['A']

table(data_analysis$Groups)['B']

table(data_analysis$Groups)['C']


# NMDS plot

#1. blank plot

ordiplot(data_nmds,
         type="n")

#2. species data

orditorp(data_nmds,
         display="species",
         col="red",
         air=0.01)

#3. sites data

orditorp(data_nmds,
         display="sites",
         cex=0.9,
         air=0.01,
         col=c(rep("steelblue",
                   table(data_analysis$Groups)['A']),
               rep("forestgreen",
                   table(data_analysis$Groups)['B']),
               rep("orange",
                   table(data_analysis$Groups)['C'])))

#4. convex hulls

ordihull(data_nmds,
         groups=data_analysis$Groups,
         draw="polygon",
         col="grey50",
         label=F)




