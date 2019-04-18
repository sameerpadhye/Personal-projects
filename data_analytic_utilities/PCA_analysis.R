##PCA analysis for exploring multivariate data and dimensionality reduction

#Libraries used
library(tmap)
library(raster)
library(ggmap)
library(sp)
library(rgdal)
library(rasterVis)
library(viridisLite)
library(RColorBrewer)
library(plotly)
library(rgeos)
library(magrittr)
library(ppcor)
library(reshape2)
library(tidyr)
library(dplyr)
library(purrr)
library(pipeR)
library(ggplot2)
library(psych)
library(readxl)
library(ggpubr)
library(ggfortify)
library(vegan)
library(betapart)
library(picante)
library(adespatial)
library(FD)
library(ape)
library(fpc)
library(tibble)
library(data.table)
library(tidyverse)

#paste0(getwd(),"/PCA_data_git.xlsx")
data_path<-"C:/Users/samee/Desktop/R data/PCA_data_git.xlsx"

#Importing data for analysis
PCA_data<-read_excel(data_path,
                          sheet=1)%>%
    mutate_if(is.character,
              as.factor)

#exploring the structure of the data
str(PCA_data)

# PCA analysis
pca_analysis<- PCA_data%>%
    dplyr::select_if(is.numeric)%>% # only numeric data should be selected
    data.frame(.)%>% # converted to dataframe since tibble is returned
    prcomp(.,scale. = T) # scale is TRUE when column descriptors are of different scales (E.g. one column contains pH values while the second contains Temperature)

#extracting the PCA co-ordinates
pca_analysis_values<-pca_analysis$x

#extracting PCA vector values of the first two axes
pca_analysis_vector<-pca_analysis$rotation[,c(1,2)]

#plot for PCA

#1.autoplot function from the package ggplot2 used to visualize the result s of the PCA analysis
ggplot2::autoplot(pca_analysis, 
         scale = 0, 
         data = PCA_data, 
         #colour='', can be added as per requirement (any specific factor or category)
         label=T,
         label.label = "country",# can be changed as per the analysis requirement
         size = 5, 
         #shape='', shape of points as per the factor/s
         frame=T,
         #frame.colour = '',shape of points as per the factor/s
         loadings = TRUE, 
         loadings.colour = 'black', 
         loadings.label = TRUE, 
         loadings.label.size = 4, 
         loadings.label.hjust = 0.5, 
         loadings.label.vjust = 1.2)

## There are mulitple arguments not passed here that can be added as per requirement.    

##Scree plot for PCA (used to visualize the contribution of each eigenvector in explaining the variation)
library(factoextra)
fviz_eig(pca_c.hislopi)
