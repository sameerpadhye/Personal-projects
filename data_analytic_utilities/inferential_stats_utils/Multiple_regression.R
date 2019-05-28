# Multiple linear regression 


#libraries used

library(tidyverse)
library(readxl)
library(MASS)


#Data file path (here it is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-paste0(getwd(),"/multiple_regression_data.xlsx")


#Importing data for analysis

data_analysis<-read_excel(data_path,
                          sheet=1)


#exploring the data

str(data_analysis)

head(data_analysis,3)


#defining the formula for the regression (only main effects model)

#Writing the formula explicitly

formula_regression<-as.formula("var_1~var_2+var_3+var_4")

#Obtaining the formula using the column names of the dataset. The column numbers will change as per the data and user requirement

formula_regression_2<-reformulate(names(data_analysis)[c(3:5)], 
                         names(data_analysis[2]))


#Linear regression 

reg_model<-lm(formula_for_reg,
                    data=data_analysis)

#Summary of the model

summary(reg_model)


# confidence intervals of the independent variables

confint(reg_model)


# Obtaining the predicted values based on the linear model

data_analysis$predictions <- predict(reg_model)


# Obtaining the residuals based on the linear model

data_analysis$residuals<- residuals(reg_model)


# Plot different aspects of the model (using base R) (Here the basic plot code provided. Users can add additional code as per their requirements)

layout(matrix(c(1,2,3,4),2,2)) 

plot(reg_model)

#Plot to check the distribution of residuals. For linear model, the residuals  should be normally distributed (atleast close to)

hist(residuals(reg_model), 
     col="black")


#Stepwise approach to check the most significant independent variables (Single or combination of variables)

stepwise_model <- stepAIC(reg_model, direction="both")

#Results

stepwise_model$anova 


#Prediction aspect will be added soon
