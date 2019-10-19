# Multinomial logisitic regression 


#Libraries used


library(tidyverse)
library(readxl)


#data file path (Here environmental data has been used)


data_path<-paste0(getwd(),"/environmental_data.xlsx")


# Importing the dataset


mult_log_data<-readxl::read_excel(data_path,sheet=1)


# exploring the data


str(mult_log_data)


table(mult_log_data$habitat_type)


# Visualizing the data based on the three levels of the habitat_type


mult_log_data%>%
    gather(env_trait,values,Altitude:Salinity)%>%
    ggplot(aes(x=habitat_type,
               y=values))+
    geom_boxplot(fill='grey',
                 lwd=0.9)+
    theme_bw(base_size = 18)+
    facet_wrap(~env_trait,
               scales = 'free')


# Defining the formula for regression(Only a certain dependent variables have been used in the analysis.


reg.formula<-reformulate(names(mult_log_data)[c(3,5,6,9)], 
                         names(mult_log_data[1]))

reg.formula


# Performing multinomial regression using nnet package


if(!require(nnet))install.packages('nnet') 


# Fitting the regression model


mult_reg_model <- nnet::multinom(reg.formula, data = mult_log_data)


# Summarize the model


summary(mult_reg_model)


# Obtaining the coefficients from the model 


exp(coef(mult_reg_model))


#Obtaining the predicted probabilities


fitted(mult_reg_model)


## Splitting the data into training and testing datasets for predictive performance. Package 'caret' is used for partitioning the dataset


library(caret)


#create an index to be used for selecting training and test samples from the dataset


sample.index<- mult_log_data$habitat_type%>%
    caret::createDataPartition(p = 0.8, 
                               list = FALSE)


sample.index


# Training data


mult_log_data.train  <- mult_log_data[sample.index, ]


# Test data


mult_log_data.test <- mult_log_data[-sample.index, ]


# Using the training dataset for making the model (Code similar to the one used above for the complete data). Same formula 'reg.formula' is used.


mult_reg_model.train <- nnet::multinom(reg.formula, 
                                       data = mult_log_data.train)


# Summarize the model


summary(mult_reg_model.train)


# Predictions using the test data


mult_reg_test<-mult_reg_model.train%>%
    predict(mult_log_data.test)


# Accuracy of the model


mean(mult_reg_test == mult_log_data.test$habitat_type)


### Multinomial regression with equal sample size of all types


#Checking the number of observations per nominal level of a variable (here habitat_type)


table(mult_log_data$habitat_type)


# The minimum number is for type_B; Therefore, those many observations from other two levels will be sampled randomly using sample_n function from dplyr


multlog_data_eq_samp<-mult_log_data%>%
    group_by(habitat_type)%>%
    sample_n(table(mult_log_data$habitat_type)['type_B'])


#Rechecking the observations of the new dataset


table(multlog_data_eq_samp$habitat_type)


# This dataset is now used for making the model (Code similar to the one used above for the complete data). Same formula 'reg.formula' is used.

library(nnet)


mult_reg_model2 <- nnet::multinom(reg.formula, 
                                  data = multlog_data_eq_samp)


# Summarize the model


summary(mult_reg_model2)


# Predictions using the test data


mult_log_reg_pred<-mult_reg_model2%>%
    predict(multlog_data_eq_samp)


# Accuracy of the model


mean(mult_log_reg_pred == multlog_data_eq_samp$habitat_type)
