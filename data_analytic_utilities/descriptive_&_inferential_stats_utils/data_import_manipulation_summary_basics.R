#Basics of data import, manipulation and summary of data used in diversity/ecology studies 

# Data used here (as in many studies) are dataframe data structures. For basics on dataframes please visit these links


#1. Data import


#Defining a path for the data file (This is the path where the data are saved)

#excel file

data_path_xl<-"C:/Users/samee/Desktop/R data/sample_datasets/data_for_visualizations.xlsx"

#csv file

data_path_csv<-"C:/Users/samee/Desktop/R data/sample_datasets/community_data.csv"


#if this is stored in the working directory, then following command can be  used by first Obtaining the working directory by

getwd()

#Obtaining the data path

data_path_wd<-paste0(getwd(),"/data_for_visualizations.xlsx")

# the file can be changed by changing the file name provided in quotations above


#importing a csv data file. The first argument here should either be the 

data_for_analysis_csv<-read.csv(data_path_csv,
                                header=TRUE, # Header of columns
                                stringsAsFactors=FALSE) #whether strings should be treated as factors or not

#There are other multiple arugments for read.csv function which can be checked by using ?read.csv


#File can also imported by an easier approach by placing file.choose() inside the read.csv function. This allows the user to select any csv file which is required

data_analysis_choose<-read.csv(file.choose())


#Importing excel data for analysis using readxl package

if(!require(readxl))install.packages('readxl') 
library(readxl)
data_analysis<-readxl::read_excel(data_path_xl,
                                  sheet=1) # the sheet to be imported

#OR

data_analysis_choose<-read_excel(file.choose())


# Viewing the data

#Data imported via read_excel will be used for further work

#Viewing the structure of the data which gives the class of the data and the data types of the columns of the dataframe

str(data_analysis)


# Examine the types of variables present using sapply function

sapply(data_analysis,
       class)


#Viewing the entire data

View(data_analysis)


# Print the entire data

print(data_analysis)


#Viewing the top n observations of the data (here 5)

head(data_analysis,5)


#Viewing the bottom n observations of the data (here 5)

tail(data_analysis,5)


#Obtaining the total number of rows in the data

nrow(data_analysis)


#Obtaining the total number of columns in the data

ncol(data_analysis)


# Check for missing data (Missing data is represented as NA)

sapply(data_analysis,
       anyNA)


#Removing NA if/any from the columns

#To check if there are any NA's (FALSE means NA is present at that row position)

NA_index<-complete.cases(data_analysis)

# selecting only the TRUE (non NA values)

data_analysis[NA_index,]


#Obtaining the column names 

colnames(data_analysis)


#Obtaining the row names 

rownames(data_analysis)


#Accessing specific rows and columns (subsetting)

data_analysis[1,2] # accessing first row from second column

data_analysis[c(1:3),]  # accessing the first three rows of all columns

data_analysis[,c(1,2)] # acessing the first two columns 

data_analysis[c("var_1","var_3")] # accessing the columns by name (instead of position)

#Using subset function

subset(data_analysis,  # the dataframe
       Factor=="Factor_A",  # subsetting by the category (Factor_A here)
       select=var_1:var_3) # subsetting only var_1 to var_3


#Obtaining data summary of rows of the dataframe (Only the numeric data has to be selected for this function)

#a. sum

rowSums(data_analysis[,c(2:4)])

#using apply for rowSums

apply(data_analysis[,c(2:4)],1,sum) # The number '1' signifies rowwise calculations while 'sum' states the function to be used (which is sum of values) here

#b. mean

rowMeans(data_analysis[,c(2:4)])

#using apply (the syntax remains same as for sum with the substitution of 'mean' instead of 'sum')

#Obtaining data summary of columns of the dataframe (Only the numeric data has to be selected for this function)

#a. sum

colSums(data_analysis[,c(2:4)])

#using apply for rowSums

apply(data_analysis[,c(2:4)],2,sum) # The number '2' signifies columnwise calculations while 'sum' states the function to be used (which is sum of values) here

#b. mean

colMeans(data_analysis[,c(2:4)])

#using apply (the syntax remains same as for sum with the substitution of 'mean' instead of 'sum')


# Viewing the levels of a factor data type. First it should be checked whether Factor data type is present in the dataframe (by str function)

#a. checking the structure of the data

str(data_analysis) 

# Since the Factor column is character, it is first converted to a factor

#b. Converting character (one data type) to factor (another data type) data type

data_analysis$Factor<-as.factor(data_analysis$Factor)

#c. Viewing the levels of the factor

levels(data_analysis$Factor)


#Binding rows and columns 

#1. Columns

#Additional rows and columns can be bound to the existing dataframe 

#the current data is first subsetted into 2 dataframes

data_1<-subset(data_analysis,
               select = Factor:var_2)

data_2<-subset(data_analysis,
               select = var_3:var_4)

#Column bind

data_combined<-cbind(data_1,
                     data_2)

View(data_combined)


#2. Rows

#For binding rows the column names of both the datasets must be matching.
#Hence if the same data used for column bind is used, an error will be obtained

rbind(data_1,data_2)

#Obtaining datasets for rbind

data_3<-subset(data_analysis,
               Factor=='Factor_A',
               select = Factor:var_2)

data_4<-subset(data_analysis,
               Factor=='Factor_B',
               select = Factor:var_2)

#Row bind with the above datasets

data_combined2<-rbind(data_3,
                      data_4)

View(data_combined2)

# Attach a dataset for easier functionality

attach(data_analysis)

#after attaching the dataset, the name of the dataset need not be called upon everytime (which is data_analysis$)

#getting basic summary of the data (after attaching the dataset)

#mean
mean(var_1,na.rm = T)

#sum
sum(var_2,na.rm = T)

#Standard deviation
sd(var_3,na.rm = T)

#range
range(var_4)

#median
median(var_1,na.rm = T)

#Variance
var(var_2)

# the dataset can be detached after the analysis has been conducted

detach(data_analysis)


# Obtaining the complete data summary of the dataframe

summary(data_analysis)


#Obtaining the data summary of the numerical variables of the dataframe using 'describe' function from psych package

if(!require(psych))install.packages('psych') 

describe(subset(data_analysis,
                select = var_1:var_4)) # here subset function is used to select the numerical variables


#Data summary can also be obtained per group/s (Factors for this dataset) using describeBy function from psych

describeBy(subset(data_analysis,
                  select = var_1:var_4), # numerical data
           group = data_analysis$Factor, # the grouping variable
           mat = TRUE, # output returned as a matrix
           digits = 3)  # rounding the digits in the output

# here the attach and detach functions would be helpful 
