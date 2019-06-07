##Using Random forest method in regression (Linear regression used here)


#Libraries used

library(tidyverse)
library(readxl)
library(MASS)
library(magrittr)
library(purrr)
library(gridExtra)


#Data file path (here it is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-paste0(getwd(),"/regression_data.xlsx")


#Importing data for analysis

data_analysis<-read_excel(data_path,
                          sheet=1)


#Obtaining the formula using the column names of the dataset. The column numbers will change as per the data and user requirement

formula_regression<-reformulate(names(data_analysis)[c(3:5)], 
                                names(data_analysis[2]))


# Using the package ranger (for random forest)

if(!require(ranger))install.packages('ranger')


# Fit and print the random forest model

rf_model <- ranger(formula_regression, # regression formula
                   data_analysis, # data
                   num.trees = 500, #number of trees to generate
                   respect.unordered.factors = 'order')


# Exploring the results

rf_model


# Obtaining the predicted values

data_analysis$pred <- ranger::predict(model_rf,
                                      data_analysis,
                                      type = 'response')$predictions


# Calculating the RMSE of the predictions

data_analysis %>% 
    dplyr::mutate(residual = var_1 - pred)  %>%        # calculate the residual
    dplyr::summarize(rmse  = sqrt(mean(residual^2))) # calculate rmse


# Plot original variables vs predictions

ggplot(data_analysis, 
       aes(x = pred, 
           y = var_1)) + 
    geom_point() + 
    geom_abline()
