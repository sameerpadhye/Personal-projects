
# Model evaluation (of simple linear regression) by means of dataset splitting (into a training and testing dataset) 


#libraries used

library(tidyverse)
library(readxl)
library(MASS)
library(magrittr)

#Data file path (here it is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-paste0(getwd(),"/multiple_regression_data.xlsx")


#Importing data for analysis

data_analysis<-read_excel(data_path,
                          sheet=1)


# Total number of rows

row_number <- nrow(data_analysis)

row_number


#Here,an 80:20 split has been used for splitting the training-testing data

# Obtaining the number of rows which equal 80% of the observations

row_no_cutoff <- round(row_number * 0.80)

row_no_cutoff


# Creating a training subset by using a vector of random row_numbers using 80% of the observations

cutoff_train<-sample(row_number,row_no_cutoff,replace = TRUE)


# Creating a testing subset by using a vector of random row_numbers using remaining 20% of the observations 

cutoff_test<-sample(row_number,(row_number-row_no_cutoff),replace = TRUE)


# Obtaining the training and testing datasets using the cutoffs

dataset_training<- data_analysis[cutoff_train, ]

dataset_testing<- data_analysis[cutoff_test, ]


# Performing the linear regression 

#Writing the formula explicitly (y~x)

formula_regression<-as.formula("var_1~var_2")


#Linear regression model using the training data

reg_model<-lm(formula_regression,
              data=dataset_training)


#Summary of the model

summary(reg_model)


# Obtaining the predicted values using the linear model

dataset_training$prediction <- predict(reg_model)


# Obtaining the predicted values based on the test data using the linear model

dataset_testing$prediction <- predict(reg_model, newdata = dataset_testing)


# Plotting the predictions against the dependent variable on the test data

dataset_testing%>%
ggplot(aes(x = prediction, 
           y = var_1)) + 
    geom_point(size=3) + 
    geom_abline()+
    theme_bw(base_size = 15)+
    labs(title='Test data prediction plot')


# RMSE for both training and test data using the package Metrics

if(!require(Metrics))install.packages('Metrics') 

rmse_training <- rmse(dataset_training$prediction, dataset_training$var_1)

rmse_testing <- rmse(dataset_testing$prediction, dataset_testing$var_1)

rmse_train

rmse_test


#Using 'modelr' for generating cross validation folds (sets of training and testing data generated from the parent dataset)


if(!require(modelr))install.packages('modelr')

data_cross_val<-data_analysis%>%
    dplyr::select(var_1,var_2)%>% #selecting the variables for analysis
    modelr::crossv_kfold(5) # number of folds 


# 'data_cross_val' is a resample object(). Looping through this object for regression and subsequent results then becomes convenient. More information can be obtained by reading the documentation on the function 'crossv_kfold'.

data_cross_val%>%
    mutate(model_reg=map(train,
                         ~lm(formula_regression,data=.)))%>% #model based on training data and looping through the data by map (from purrr)
    #mutate(predicted_val=map2(model_reg,test,predict))%>% #(if predicted values are needed)
    #mutate(residual_val=map(model_reg,residuals))%>% #(if resdiuals are needed)
    mutate(rmse=map2_dbl(model_reg,
                         test,rmse))%>% # calculating the rmse using the test split created by the crossv_kfold using the map2_dbl function which takes two arguements besides the function (which is rmse here)
    dplyr::select(.id,rmse) #displaying the rmse for each of the fold


##Using a package called 'DAAG' to obtain predicted scores via kfold cross validation. The function cv.lm is used to obtain the predicted values directly from a kfold cross validation 

if(!require(DAAG))install.packages("DAAG")


# model

model_res<-cv.lm(data=(data_analysis%>%
                           dplyr::select(var_1,var_2)), 
                 form.lm=formula_regression, #regression formula
                 m= 10, #number of folds 
                 plotit = F)


# predicted scores using cross validation

model_res$cvpred


# RMSE

rmse_model_reg<-Metrics::rmse(model_res$cvpred,model_res$var_1)

rmse_model_reg


#Using Bootstrap Aggregating (Bagging) in model evaluation


dataset_training # training dataset

dataset_testing # testing dataset


# using set.seed for reproducibility

set.seed(100)


# Train a bagged model. Library 'ipred' is used for the analysis

if(!require(ipred))install.packages("ipred")

bag_model_train <- bagging(formula = formula_regression, 
                           data = dataset_training,
                           coob = TRUE)

# Print the model

print(bag_model_train)


# Obtaining predictions using the test data

prediction <- predict(object = bag_model_train,
                      newdata = dataset_testing,
                      type = "class")


# Look at the pred format

head(prediction)


#RMSE for evaluation

rmse_bag_model<-Metrics::rmse(prediction ,dataset_testing$var_1)

rmse_bag_model
