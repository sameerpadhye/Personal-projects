#Gradient boosting machine (GBM) basics

#Details about this technique can be found on the following website: https://machinelearningmastery.com/gentle-introduction-gradient-boosting-algorithm-machine-learning/

#libraries used

library (tidyverse)


#The initial steps of preparing the data for the analysis is similar to that used in case of classification (except for conversion of response variable to presence/absence)


#Data file path (assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-"C:/Users/samee/Desktop/R data/sample_datasets/log_regression_sample_data.csv"


#Importing data for analysis

data_analysis<-read.csv(data_path)


#Exploring the data structure

str(data_analysis)


#Creating the training/testing dataset (80%/20%)

# Creating indices to sample training and test datasets

set.seed(1)

sample_assignment <- sample(1:2, size = nrow(data_analysis), prob = c(0.8,0.2), replace = TRUE)

# Training dataset

train_data <- data_analysis[sample_assignment == 1, ]   

#Test dataset

test_data <- data_analysis[sample_assignment == 2, ]   


#Obtaining the formula using the column names of the dataset. The column numbers will change as per the data and user requirement

formula_regression<-reformulate(names(data_analysis)[c(1:7)], 
                                names(data_analysis[8]))


# Obtaining the gbm model using the 'gbm' package

if(!require(gbm))install.packages('gbm') 


#gbm model

train_model <- gbm(formula = formula_regression, #formula
                    distribution = "bernoulli", # bernoulli option since the outcome is binary 
                    data = train_data,
                    n.trees = 10000) # number of trees
#here 'cross validation folds can also be included (as 'cv.folds' argument) but not used in this case'


# summary of the model

summary(train_model)  


# Predictions using test dataset

predictions <- predict(object = train_model, #training model
                  newdata = test_data, # the test data
                  n.trees = 10000, # number of trees
                  type = "response") # response as an argument gives the values with same scale of the response variable


# Range of the predicted values

range(predictions)


# AUC scores of the model

auc(actual = test_data$Species_A, predicted = predictions)  #default


