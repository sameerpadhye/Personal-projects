#Parallel co-ordinates plot

#This plot is useful in visualizing multivariate numerical data. Multiple variables can be compared at the same time in a single chart.For more information on this plot, please check https://datavizcatalogue.com/methods/parallel_coordinates.html

#libraries used
library(readxl)
library(tidyverse)
library(GGally)
library(RColorBrewer)
library(plotly)


#data_file (data file is presumed to be in the working directory; if its not, the complete path can be pasted)

file_data<-paste0(getwd(),"/data_for_analysis.xlsx")

#Importing data
data_for_analysis<-read_excel(file_data,
                          sheet=1)%>%
    mutate_if(is.character,
              as.factor) #converting the character to factor variables
 
#Plotting can be done using base R as well as with help of many packages. Plots using 1. Base R and 2. GGally and 3. Interactive plot by Plotly

#1. Base R 
#
parcoord(data_for_analysis%>%
             dplyr::select(Trait_1:Trait_5), ## selecting the numerical data. These will change as per the data
         col= data_for_analysis$Factor_level1) # color as per the number of categories

#2. GGally

parallel_plot<-GGally::ggparcoord(data_for_analysis, 
                   columns = 3:ncol(data_for_analysis), # specify the columns for which the parallel plot is needed
                   groupColumn = "Factor_level2", # grouping column name (or number)
                   scale = "robust", 
                   scaleSummary = "mean", 
                   missing = "exclude")+
    geom_line(size=1.2)+
    theme_classic(base_size = 16)+
    scale_color_brewer(palette = "Set2")+
    geom_point(size=1.2, 
               shape=21, 
               colour="black",
               fill='grey50',
               alpha=0.4) +
    theme(panel.grid.major.x=element_line(colour="grey70"))+
    xlab("Column_names")+ # names changed as per dataset
    ylab("Values") # names changed as per dataset

#3a. Passing ggplot object to plotly
ggplotly(parallel_plot)

#3b. Plotting by plotly (WIP)
