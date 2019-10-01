##Regression using the Generalized Additive Models (GAM)


#Libraries used

library(tidyverse)
library(readxl)
library(MASS)
library(magrittr)
library(purrr)
library(gridExtra)


#Data file path (here it is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-paste0(getwd(),"/data_for_analysis.xlsx")


#Importing data for analysis

data_analysis<-read_excel(data_path,
                          sheet=1)


#Exploring and Visualizing the data

#Exploring the data

str(data_analysis)

#Visualizing the relationships between the dependent and different independent variables

data_analysis%>%
    gather(value_names,
           values,
           var_2:var_4)%>%
    ggplot(aes(x=values,
               y=var_1))+
    geom_point()+
    stat_smooth()+
    facet_grid(~value_names,
               scales = 'free')+
    theme_bw(base_size = 16)

#Since none of the relationships are linear, a smooth function 's()' is used in the regression formula


#Obtaining the formula using the column names of the dataset. The column numbers will change as per the data and user requirement

(gam_formula<-var_1~s(var_2)+s(var_3)+s(var_4))


# Load the package mgcv for GAM

if(!require(mgcv))install.packages('mgcv')


# Fit the GAM Model

gam_model <- gam(gam_formula, data = data_analysis, family = gaussian)


# model summary

summary(gam_model)


# predicted values from gam model

data_analysis$pred_val <- predict(gam_model)


# residual values using gam model

#a. subtracting the dependent variable from the predicted value

data_analysis$residuals<-data_analysis$var_1-data_analysis$pred_val

#b. using 'residuals' function

data_analysis$residuals2<-residuals(gam_model)


#Calculating the RMSE of the model

gam_rmse<-sqrt(mean(data_analysis$residuals2^2))

gam_rmse


#Models using interaction effects will be added soon
