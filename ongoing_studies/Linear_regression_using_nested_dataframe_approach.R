## Simple linear regression using a nested dataframe approach

# Libraries used

library(tidyverse)
library(readxl)
library(MASS)
library(magrittr)


#Data file path (here it is assumed that the sample data is saved in the working directory and saved as an excel file; data_for_analysis_ANOVA has been used here)

data_path<-"C:/Users/samee/Desktop/R data/sample_datasets/data_for_analysis_ANOVA.xlsx"


#Importing data for analysis

data_analysis<-read_excel(data_path,
                          sheet=1)%>%
    dplyr::select(Trait_1,
                  Trait_2,
                  Factor_level1)


# Use unique() to see how many possible values the categorical variable takes

unique(data_analysis$Factor_level1)


# Regression formula

formula_regression<-as.formula("Trait_1~Trait_2")


# Fitting the model by nested dataframe approach using nest function from tidyr and map package from purrr package

# using purrr package, the map function loops over all the levels of Factor_level1 and performs a linear regression using Trait_1 and Trait_2 variables. This is then followed by using the tidy function from broom package to obtain the results

require(broom)

nested_regression<-data_analysis%>%
    tidyr::nest(-Factor_level1)%>% # group based on Factor_level1
    dplyr::mutate(reg_models=purrr::map(data,~lm(formula_regression,
                                                 data = .)))%>%
    dplyr::mutate(tidy_res=map(reg_models,
                               tidy))%>% 
    tidyr::unnest(tidy_res) # to visualize the results

# Visualize the results using DT package

require(DT)

DT::datatable(nested_regression)
