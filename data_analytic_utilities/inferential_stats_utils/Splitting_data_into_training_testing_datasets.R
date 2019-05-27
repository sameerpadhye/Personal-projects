#Splitting the data into training and testing datasets for data analysis

#libraries used

library(tidyverse)
library(readxl)


#Path for the data file (Assuming that the data are stored in working directory)

data_path<-paste0(getwd(),"/data_analysis.xlsx")


#Importing data for analysis

data_analysis<-read_excel(data_path,
                         sheet=1)


# Use nrow to get the number of rows in mpg (N) and print it

row_number <- nrow(data_analysis)

row_number


# Obtaining the number of rows which equal 80% of the observations

row_no_cutoff <- round(row_number * 0.80)

row_no_cutoff


# Creating a training subset by using a vector of random row_numbers using 80% of the observations ('target')

cutoff_train<-sample(row_number,
                     row_no_cutoff,
                     replace = TRUE)


# Creating a testing subset by using a vector of random row_numbers using remaining 25% of the observations ('target')

cutoff_test<-sample(row_number,
                    (row_number-row_no_cutoff),
                    replace = TRUE)


# Obtaining the training and testing datasets using the cutoffs

dataset_training<- data_analysis[cutoff_train, ]

dataset_testing<- data_analysis[cutoff_test, ]


# Checking the number of rows in each dataset

nrow(dataset_training)

nrow(dataset_testing)


#Packages such as vtreat can also be used for this splitting where the function 'kWayCrossValidation'  provides sets of training and testing rownumbers from the original data. 

if(!require(vtreat))install.packages('vtreat')

# Using the number of rows obtained earlier, we can obtain random sets of  row numbers (separately for training and testing) based on the original data with multiple sets. The split of training to testing is 75:25 (approx.)

Data_split <- kWayCrossValidation(row_number, 3, dframe=NULL, y=NULL)

# Checking the split (here the first of the three subsets used)

# training 
Data_split[[1]]$train

#testing
Data_split[[1]]$app

#The row numbers obtained above are then used for subsetting the original data 

Data_split_training<-data_analysis[Data_split[[1]]$train
, ]

Data_split_testing<-data_analysis[Data_split[[1]]$app
                                   , ]

#Checking the number of rows 

nrow(Data_split_training)

nrow(Data_split_testing)
