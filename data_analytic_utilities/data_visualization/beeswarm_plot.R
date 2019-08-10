## Beeswarm plot to visualize the distribution of data where all the sample points are plotted 

#Libraries used

library(tidyverse)
library(readxl)

# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-paste0(getwd(),"/data_for_visualization.xlsx")

#Importing data for analysis 

data_analysis<-read_excel(data_path,
                          sheet=1)


#1. Using package beeswarm

if(!require(beeswarm))install.packages('beeswarm')

#number of categories for plotting (colors)

number_of_categories<-length(unique(data_analysis$Factor))

#plot (Here only few arguments have been used). For information on additional arguments, ?beeswarm can be run 

beeswarm(var_1~Factor,data = data_analysis,
         col=palette(rainbow(number_of_categories)), #colors used. Depends on number of categories
         pch=19, #shape of points
         method="square", # how points should be arranged
         cex=1) # size of the points


#2. Using the package ggbeeswarm

if(!require(ggbeeswarm))install.packages('ggbeeswarm')

#plot

data_analysis%>%
  ggplot(aes(Factor,
             var_1,
             color=Factor)) + 
  geom_quasirandom(dodge.width=1)+ #here method argument can be added to change the shape of the swarm
  theme_bw(base_size = 15)+
  ggtitle("Beeswarm plot")

#additional ggplot2 arguments can be passed as per user requirement