# Simple linear regression with a categorical and continous variable (no interaction)


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
                  Factor_level2,
                  Trait_2)


# Use unique() to see how many possible values the categorical variable takes

unique(data_analysis$Factor_level2)


# Regression formula

formula_regression<-as.formula("Trait_1~Trait_2+Factor_level2")


#  model matrix to explore how the categories are used for regression

reg_model_matrix <- model.matrix(formula_regression, data_analysis)


#Examining the matrix

reg_model_matrix 


# Fitting the model

model_cat_reg <-  lm(formula_regression, data = data_analysis)


# Summary of the model

summary(model_cat_reg)


# Predicting the outcome variable

data_analysis$predictions <- predict(model_cat_reg)


# Plot predictions vs actual values (predictions on x-axis)

ggplot(data_analysis, aes(x = predictions, 
                          y = Trait_1)) + 
    geom_point() +
    geom_abline(color = "blue")+
    theme_bw(base_size = 16)+
    ggtitle("Scatterplot")


#2. Continous data analysis

# Same original data has been used 

# Importing the data

data_analysis2<-read_excel(data_path,
                           sheet=1)%>%
    dplyr::select(Trait_1,
                  Trait_2)


#Scatterplot to visualize the association of the two continous variables

data_analysis2%>%
    ggplot(aes(Trait_1,
               Trait_2))+
    geom_point(size=3,
               col='forestgreen')+
    theme_bw(base_size = 16)+
    ggtitle("Scatterplots")


#Correlation of the two variables (to check linear association)

data_analysis2%$%
    cor.test(Trait_1,
             Trait_2,
             method = c("pearson"))


# Regression formula

formula_regression2<-as.formula("Trait_1~Trait_2")


# Fitting the model

model_cont_reg <-  lm(formula_regression2, 
                      data = data_analysis2)


# Summary of the model

summary(model_cont_reg)


#Adjusted R squared value (strength of the regression model)

summary(model_cont_reg)$adj.r.squared


# Predicting the outcome variable

data_analysis2$predictions <- predict(model_cont_reg)


# Plot predictions vs actual values (predictions on x-axis)

ggplot(data_analysis2, aes(x = predictions, 
                           y = Trait_1)) + 
    geom_point() +
    geom_abline(color = "blue")+
    theme_bw(base_size = 16)+
    ggtitle("Scatterplot")
