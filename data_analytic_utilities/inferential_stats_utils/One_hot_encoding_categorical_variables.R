## One-hot encoding of categorical variables in regression analysis


#Libraries used

library(tidyverse)
library(readxl)
library(MASS)
library(magrittr)
library(purrr)


#Data file path (here it is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-paste0(getwd(),"/data_for_analysis.xlsx")


#Importing data for analysis

data_analysis<-read_excel(data_path,
                          sheet=1)


# Creating a vector of variable names (colnames of all independent variables; var_1 here is the dependent variable)

field_names<-colnames(subset(data_analysis,select=-var_1))


# The library vtreat is used for modifying the dataset for one-hot encoding

if(!require(vtreat))install.packages('vtreat')


# Creating a modified dataset for the treatment variables (contains all the independent variables)

data_modified <- vtreat::designTreatmentsZ(data_analysis, field_names)


# Examining the scoreFrame (which contains details of the original and modified names along with respective codes assigned).

data_modified$scoreFrame

# 'Clean' variables in the scoreframe are the values selected after excluding NA's and NAN's (if present in the data); 'Levels' are the categorical variables


#Creating a character vector of the (modified) names of independent variables required for one hot encoding

selected_var_names <- data_modified %>%
    magrittr::use_series(scoreFrame) %>% #selecting the dataframe from the 'data_modified object'
    filter(code %in% c("clean", "lev")) %>%# Only the rows with codes "clean" or "lev" are used for creating the new modified dataset
    magrittr::use_series(varName) ##selecting the modified names


# Creating the new modified dataset 

new_modified_data <- vtreat::prepare(data_modified, # modified dataset
                                     data_analysis, #original dataset
                                     varRestriction = selected_var_names) #Modified names of the independent variables required in the new dataset


#Exploring the new modified data

head(new_modified_data,5)

