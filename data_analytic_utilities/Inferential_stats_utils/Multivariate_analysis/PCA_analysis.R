##PCA analysis for exploring multivariate data and dimensionality reduction
#It is a technique used to explore associations if/any in the multivariate data. Detailed explaination for it can be found in the book titled , 'Numerical Ecology'by Legendre and Legendre


#Libraries used


library(tidyverse)
library(magrittr)
library(readxl)


# Data path


data_path<-"C:/Users/samee/Desktop/Personal-projects/sample_datasets/PCA_data_git.xlsx"


#Importing data for analysis


PCA_data<-read_excel(data_path,
                          sheet=1)%>%
    mutate_if(is.character,
              as.factor)


# Exploring the structure of the data


str(PCA_data)


# PCA analysis


pca_analysis<- PCA_data%>%
    dplyr::select_if(is.numeric)%>% # only numeric data should be selected
    data.frame(.)%>% # converted to dataframe since tibble is returned
    prcomp(.,scale. = T) # scale is TRUE when column descriptors are of different scales (E.g. one column contains pH values while the second contains Temperature)


#extracting the PCA co-ordinates


pca_analysis_values<-pca_analysis$x


#extracting PCA vector values 


pca_analysis_vector<-pca_analysis$rotation


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


## There are mulitple arguments not passed here that can be added as per requirement (ggplot arguments can also passed to the above plot)

##Scree plot for PCA (used to visualize the contribution of each eigenvector in explaining the variation)


library(factoextra)


fviz_eig(pca_c.hislopi)


#Calculating the contribution of individual descriptors to the PCA axes


# (Adopted from: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/)


#1. Obtaining the Coordinates


PCA_loadings <- pca_analysis$rotation


std.dev <- pca_analysis$sdev


coord_env_var <- t(apply(PCA_loadings, 
                         1, function (x) {x*std.dev}))


head(coord_env_var,5)


#2. Obtaining the cos2 values


coord_env_var_cos2 <- coord_env_var^2


head(coord_env_var_cos2)


#3 Obtaining the contributions 


#a.


PCA_env_var_contri <- apply(coord_env_var_cos2, 2, sum)


#b.


env_contri_fn <- function(cos2_val, cos2_add){cos2_val*100/cos2_add}


env_var_contri <- t(apply(coord_env_var_cos2,
                          1, 
                          env_contri_fn,
                          PCA_env_var_contri))


head(env_var_contri,5)


