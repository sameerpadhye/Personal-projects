#Beta diversity partitioning analysis to check patterns in faunal dissimilarity

#libraries used
library(betapart)
library(tidyverse)
library(readxl)


# This library uses a wide data format with species as rows and the samples as columns. Species abundances or presence/absence data can be used. Following code is for presence/absence data

#importing dataset
dataset_betadiv<-read_excel(paste0(getwd(),"/betadata_for_analysis.xls",sheet=1))%>%
    dplyr::rename("habitat_type"="Habitat type")%>%
    mutate_at(vars(contains('Habitat')),
              as.factor)%>%
    filter(habitat_type=="Pond")

#structure of the dataset
str(dataset_betadiv)

#for further analysis. the factor column 'habitat_type' cannot be included and hence needs to be omitted 

#checking if there are any zero totals in rows and columns as is required for the beta diversity partitioning analysis

within(dataset_betadiv, rm(habitat_type))%>%
    rowSums(.)==0
within(dataset_betadiv, rm(habitat_type))%>%
    colSums(.)==0

#calculating the beta diversity (and the Beta sim and Beta nes partitions) of Pond fauna

#For overall beta diversity values which consider all the pond samples 
fauna_beta_multi<-dataset_betadiv%>%
    subset(.,select=-c(habitat_type))%>%
    betapart::betapart.core(.)%>%
    betapart::beta.multi(.)


#For pairwise beta diversity values which consider faunal dissimilaritybetween 2 samples 
fauna_beta_pairs<-dataset_betadiv%>%
    subset(.,select=-c(habitat_type))%>%
    betapart::betapart.core(.)%>%
    betapart::beta.pair(.)

#For more information on the these indices, readers can refer to 'betapart'library documentation and the references therein



