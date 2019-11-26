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


#Plotting predicted vs actual values

data_analysis%>%
    ggplot(aes(x = predictions, 
               y = var_1)) + 
    geom_point(size=3,
               fill='orange',
               col='black',
               pch=21) + 
    geom_abline(color = "blue")+
    theme_bw(base_size = 16)+
    xlab("Predicted values")+
    ylab("Variable 1")


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


#Obtaining the RMSE (root mean square error) of the whole dataset

if(!require(Metrics))install.packages('Metrics') 

data_rmse<-rmse (data_analysis$var_1,data_analysis$predictions)


#Obtaining the a. relative error and b. root mean squared relative error (rmsre) of the model

#a.
data_analysis$relative_error<-(data_analysis$residuals)/(data_analysis$var_1)

#b.
data_rmsre<-sqrt(mean((data_analysis$relative_error)^2,na.rm = T))


#Comparing rmse and rmsre

data_rmse

data_rmsre


# Best model selection (using forward selection) 


# First a series of models is run, adding one independent variable

#1. with intercept

mod_intercept<-lm(var_1~ 1, 
                  data= data_analysis)

#2 . With first independent variable

mod_var_2<-lm(var_1~ var_2, 
              data= data_analysis)

#3 . With two independent variables

mod_var_2_3<-lm(var_1~ var_2 + var_3, 
                data= data_analysis)

#4 . With all the three independent variables

mod_var_2_3_4<-lm(var_1~ var_2 + var_3 + var_4, 
                  data= data_analysis)


# Running an ANOVA for selecting the best fit model

anova(mod_intercept,
      mod_var_2,
      mod_var_2_3,
      mod_var_2_3_4) 
