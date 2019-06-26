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


###Model_evaluation in Random forest (Here code provided uses different variables than those given above)#


#Data file path 

data_path2<-paste0(getwd(),"/regression_data2.xlsx")


#Importing data for analysis

data_analysis2<-read_excel(data_path2,
                           sheet=1)%>%
    dplyr::select(Trait_1:Trait_4)


# Creating indices to sample training and test datasets

set.seed(1)

sample_assignment <- sample(1:2, size = nrow(data_analysis2), prob = c(0.7,0.3), replace = TRUE)


# Create a train, validation and tests from the original data using the indices

train_data <- data_analysis2[sample_assignment == 1, ]    #training data subset using the sample_assignment index

test_data <- data_analysis2[sample_assignment == 2, ]   #testing data subset using the sample_assignment index


# Using the package rpart (for random forest)

if(!require(rpart))install.packages('rpart')


# Training the model using rpart 

train_model <- rpart(formula = Trait_1 ~ ., 
                     data = train_data, 
                     method = "anova")


# Look at the model output  

train_model


# Using the rpart.plot package to visualize the tree

if(!require(rpart.plot))install.packages('rpart.plot')

# Plot the tree model

rpart.plot(x = train_model, yesno = 2, type = 2, extra = 0)


# Predictions using the test dataset

predictions <- predict(object = train_model,   # model object 
                       newdata = test_data)  # test dataset


#Obtaining the RMSE (root mean square error) using the package Metrics

if(!require(Metrics))install.packages('Metrics') 

rmse(actual = test_data$Trait_1, 
     predicted = predictions)


# Pruning the tree based on Complexity Parameter (CP) and visualizing the optimized tree

# Plotting the CP Table

plotcp(train_model)


# Obtaining the least CP value based on cross-validated error

least_cp_val_index <- which.min(train_model$cptable[, "xerror"])

least_cp <- train_model$cptable[least_cp_val_index, "CP"]


# Pruning the model using the least CP value

optimum_tree_model <- prune(tree = train_model, 
                            cp = least_cp)


# Visualizing the new optimized model

rpart.plot(x =optimum_tree_model, yesno = 2, type = 2, extra = 0)


#################Using a different package 'Randomforest' for analysis########

if(!require(randomForest))install.packages('randomForest')

# train_data and test_data used here as well

set.seed(1)  # for reproducibility


#Training the model

rf_model2 <- randomForest(formula = Trait_1 ~ ., # formula
                          data = train_data, # training_dataset
                          ntree=5000)  #number of trees

# Print the model output  

print(rf_model2)


# Exploring the importance of predictor variables

randomForest::importance(rf_model2)


# Generating predictions using the model

prediction2 <- predict(object = rf_model2,   # model object 
                       newdata = test_data,  # test dataset
                       type = "response") # for obtaining the values
prediction2


#Obtaining the RMSE (root mean square error) using the package Metrics

if(!require(Metrics))install.packages('Metrics') 

Metrics::rmse(actual = test_data$Trait_1, 
              predicted = prediction2)


#Hyperparameter tuning of the 'mtry' with TuneRF in the package Randomforest.'mtry' selects the number of predictor/s sampled randomly at each split while building the tree

tuning_var <- tuneRF(x = subset(train_data, 
                                select = -Trait_1), # subset of all independent variables
                     y = train_data$Trait_1, # the dependent variable
                     ntreeTry = 999) # number of trees to be run

#results (here since the number of predictors is less and dataset is small, result of this optimization can be scanned manually to pick the least value)

tuning_var


# In case the number is high and the dataset is large, following code can be used to obtain the least value

optimum_mtry <- tuning_var%>%
    as.data.frame()%>%
    filter(OOBError==max(OOBError))

#output

print(optimum_mtry)


#Using the this value in the model

rf_model_tuned <- randomForest(formula = Trait_1 ~ ., # formula
                               data = train_data, # training_dataset
                               ntree=5000,
                               mtry=optimum_mtry[,"mtry"])
