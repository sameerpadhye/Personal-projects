

#libraries used

library(tidyverse)
library(readxl)
library(MASS)
library(magrittr)

#Data file path (here it is assumed that the sample data is saved in the working directory and saved as an excel file; Data used here is same as used for logistic regression which gives a presence/absence data of a species and a few predictor variables)

data_path<-paste0(getwd(),"/log_regression_sample_data.csv")


#Importing data for analysis

data_analysis<-read.csv(data_path)


#Adding a separate column for the response the 1's and 0's into a binary (presence, absence) categories (If the data are originally encoded into a yes/no or presence/absence response, then this step can be skipped)

data_analysis<-data_analysis%>%
    mutate(sp_response=ifelse(Species_A=='1',
                              'present',
                              'absent'))%>%
    mutate_at(vars(contains('sp_resp')),
              as.factor)


# Exploring the data

str(data_analysis)


#Creating the training/testing dataset (80%/20% split)

# Creating indices to sample training and test datasets

set.seed(1)

sample_assignment <- sample(1:2, size = nrow(data_analysis), prob = c(0.8,0.2), replace = TRUE)

# Training dataset

train_data <- data_analysis[sample_assignment == 1, ]   

#Test dataset

test_data <- data_analysis[sample_assignment == 2, ]   


#Obtaining the formula using the column names of the dataset. The column numbers will change as per the data and user requirement

formula_regression<-reformulate(names(data_analysis)[c(1:7)], 
                                names(data_analysis[9]))


# Obtaining the model based on the training dataset using rpart

if(!require(rpart))install.packages('rpart')

model_train <- rpart(formula = formula_regression, 
                     data = train_data, 
                     method = "class")


# model output                      

print(model_train)


# Using the rpart.plot package to visualize the tree

if(!require(rpart.plot))install.packages('rpart.plot')

# Plot the tree model

rpart.plot(x = model_train, yesno = 2, type = 2, extra = 0)


# Plotting the training model to get the optimal point to prune the tree by checking the complexity parameter (for tuning the model)

rpart::plotcp(model_train)

# Generate predicted classes using the training data model (using the test dataset)

prediction_class <- predict(object = model_train,  
                            newdata = test_data,   
                            type = "class")  


# Obtaining the confusion matrix for the test set (using caret package)

if(!require(caret))install.packages('caret')

confusionMatrix(data =prediction_class,       
                reference = test_data$sp_response)  


# Calculating the AUC (from Metrics package) (Here the response data needs to be as 1 and 0) for model evaluation

if(!require(Metrics))install.packages('Metrics') 

auc(actual = ifelse(test_data$sp_response == "present", 1, 0), 
    predicted = prediction_class)  


################# Using Randomforest package for classification trees ########

if(!require(randomForest))install.packages('randomForest') 


#Obtaining the model based on the training dataset

train_model <- randomForest(formula = formula_regression, 
                            data = train_data,
                            ntree=999)

# Checking the model output      

print(train_model)


# Grab OOB error matrix & take a look

OOB_err <- train_model$err.rate

head(OOB_err)


# Obtaining the final OOB value

oob_err <- OOB_err[nrow(OOB_err), "OOB"]

print(oob_err)


# Plot the model trained in the previous exercise

plot(train_model)
legend(x = "right", 
       legend = colnames(OOB_err),
       fill = 1:ncol(OOB_err))


# Printing the OOB accuracy

paste0("OOB Accuracy: ", 1 - oob_err)
