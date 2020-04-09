##Performing a One way parametric ANOVA 

#Libraries used

library(tidyverse)
library(readxl)
library(data.table)

#data file (data assumed to be saved in the working directory)
data_file<-"C:/Users/samee/Desktop/Personal-projects/sample_datasets/data_for_analysis_ANOVA.xlsx"
data_file<-paste0(getwd(),"/data_for_analysis_ANOVA.xlsx")


#Data for analysis. Data should be in long format (stacked data)

data_for_analysis<-read_excel(data_file,
                              sheet=2)%>%
    mutate_if(is.character,
              as.factor)%>%
    data.table::data.table(.)

# Viewing the data

data_for_analysis


# Selecting specific rows from the datatable

#1. row_number

data_for_analysis[1:30,]

#2. rows based on specific category (without using by)

#a. One category

data_for_analysis[Factor=="Habitat_1"]

#b. Multiple categories

data_for_analysis[Factor%in% c("Habitat_1","Habitat_3")]

# Selecting specific columns

data_for_analysis[,.(Factor,Trait)]

# Using aggregrate functions on columns

data_for_analysis[,.(sums=sum(Trait))]

data_for_analysis[,.(means=mean(Trait,na.rm=T))]

# Plotting specifying the columns

data_for_analysis[,.(hist(Trait),plot(Trait))]
