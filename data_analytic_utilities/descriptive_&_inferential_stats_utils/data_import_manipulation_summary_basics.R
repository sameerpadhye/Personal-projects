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


# The file can also be downloaded and subsequently used in the table. Here, the file is downloaded using the 'fread' function in data.table library

if(!require(data.table))install.packages('data.table') 

data_file <- data.table::fread('http://www.xyz.com/datasets/data_file.csv') #provide the path


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

#minimum value
min(var_2)

#maximum value
max(var_4)

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


# Aggregating data using a factor (this is used to aggregate row values for the categories present in the data)

data_agg_factor<-aggregate(subset(data_analysis,
                                  select = var_1:var_4), # Numerical data selected
                           by = list(data_analysis$Factor), # the category for which function is to be used
                           FUN = mean) # function to be used


#Using tapply for aggregating data

attach(data_analysis)

tapply(var_1,
       Factor,
       mean)

detach(data_analysis)


##Using 'dplyr' for basic data manipulation
# dplyr is a packge from the tidyverse collection of libraries which is mainly used in manipulation of dataframes. For more information on Tidyverse, please check the following website https://www.tidyverse.org/

if(!require(tidyverse))install.packages('tidyverse') 

# Using dplyr is very convenient and data manipulation becomes easier as compared to base R in many instances

# Pipes (%>%) are commonly used in tidyverse to make the code more readable and easier to interpret. The symbol implies the word 'then'. It will used for all dplyr statements and explained in bried

#1. Select statement: Used for selecting columns of the dataset

#normal select

data_analysis_2<- data_analysis%>% # meaning use the original dataframe named data_analysis and then
    dplyr::select(Factor,  # selecting the requisite columns by name
                  var_1,
                  var_3)

# conditional select 1: select can also be used to select specific columns based on certain conditions

data_analysis_3<-data_analysis%>%
    dplyr::select_if(is.numeric)  # here it means select the data only if its numeric (is.numeric)

# conditional select 2: select by stating specific position

data_analysis_4<-data_analysis%>%
    dplyr::select_at(vars(contains('Fact'))) # here the column is selected based on the name (even if only a part of the word is mentioned, select will still pick up the right column unless the part of the word is not unique i.e. suppose the dataset contains two columns having names Factor_1 and Factor_2, using partial matching like 'Fact' will select both the columns)


#2. Filter: Used for selecting specific rows

#filter with one condition

data_analysis_5<-data_analysis%>%
    dplyr::filter(Factor=='Factor_A')  # here the only the rows (and all columns) corresponding to Factor_A will be filtered out

data_analysis_5

#filtering with multiple conditions 

data_analysis_6<-data_analysis%>%
    dplyr::filter(Factor=='Factor_B', var_3 > 500) # here the rows with factor B and var_3 having values more than 500 will be filtered

#checking if the filter was right

min(data_analysis_6$var_3)


#Filtering with numerical condition

data_analysis_7<-data_analysis%>%
    dplyr::filter(var_4 > mean(var_4, 
                               na.rm = TRUE)) # here only the rows having values more than the mean of var_4 will be filtered and returned


#3. Group by and Summarize

#These two functions can be used in combination to summarize data based on certain grouping factors.

data_analysis_8<-data_analysis%>%
    dplyr::group_by(Factor)%>% # grouping column
    dplyr::summarise(mean=mean(var_1,na.rm = T),   # different summarizing functions
                     median=median(var_1,na.rm = T),
                     std_dev=sd(var_1,na.rm = T))

#Viewing the result

View(data_analysis_8)


#4. Mutate

#This function is used to generate new columns in the original dataframe

data_analysis_9<-data_analysis%>%
    dplyr::mutate(new_var_1=(mean(var_1) - var_1), # defining new columns
                  new_var_2=(max(var_2)-var_2))

# Exploring the result

View(data_analysis_9)


# Conditional mutate (These mutate statements do not generate new columns but modify the existing ones)

#a.

data_analysis_10<-data_analysis%>%
    dplyr::mutate_if(is.factor,   # are there any factor columns
                     as.character)  # if there are, convert it to character

#checking whether the data type has changed or not

class(data_analysis_10$Factor)

#b. 

data_analysis_11<-data_analysis%>%
    dplyr::mutate_at(vars(contains('var_1')),log10) # find the column var_1 and convert it to a log10 value

#View the result

View(data_analysis_11)


#Arranging the dataframe based on values of a particular variable of the dataset. Arrange function in dplyr is used for this purpose. The dataset is arranged as per the ascending values of the variable

# Arrange in ascending (default) way

data_analysis%>%
    dplyr::arrange(var_1)

# Arrange in a descending way

data_analysis%>%
    dplyr::arrange(desc(var_1))


# Selecting the rows of the dataframe using slice (based on position)

data_analysis_slice<-data_analysis%>%
    dplyr::slice(.,1:20)

#View the result

View(data_analysis_slice)


# Selecting the top 'n' observations of the dataframe after arranging or grouping the dataset

data_analysis_topn<-data_analysis%>%
    dplyr::arrange(var_2)%>% # arrange the result
    dplyr::top_n(15)   # select the top 15 observations

#View the result

View(data_analysis_topn)


# Counting observations based on factors (frequencies of each category)

data_analysis_counts<-data_analysis%>%
    group_by(Factor)%>%
    summarise(counts=n())

#View result

data_analysis_counts


#Counting distinct observations of values (here the variable can be categorical or continous)

data_analysis%>%
    summarise(counts=n_distinct(var_3))


#Categorizing data based on a condition 

data_analysis_ifelse<-data_analysis %>%
    dplyr::mutate(new_col=if_else(Factor == "Factor_A",  #if factor is factor A
                                  "Correct",             # then place correct
                                  "Otherwise"))             # else place otherwise in the new column 'new_column'

#View result

data_analysis_ifelse


#Extracting a single variable (column) from the data

data_analysis_pull<-data_analysis%>%
    dplyr::pull(var_3)

#View result

data_analysis_pull


# Selecting specific elements  depending on position 

print(data_analysis%>%
          dplyr::pull(var_1)%>%
          dplyr::nth(5))


# Long and wide dataframe (conversion of wide to long)

# Two types of dataframes are used in many analyses in R (depending on the type of library used)

#Wide dataframe contains a unique set of observations for each row. 

# Example
data_analysis[1,]

# This wide dataframe can be converted to a long dataframe where the observations are stacked. This conversion can be done using 'gather' function from the 'tidyr' package from tidyverse

data_analysis_long<-data_analysis%>%
    tidyr::gather(variable_types,  # a name for the new column where the column names (which are needed) of the wide dataset will be placed
                  values,             # second column where the values of the corresponding columns will be placed
                  var_1:var_4)   # the columns which need conversion from wide to long

#Exploring the long dataset

View(data_analysis_long)

data_analysis_long[1,]


#Merging two dataframes. 

#Two dataframes can be merged based on common column between the two datasets. The names of the two columns could be different in the dataframes but the type and sequence of the rows should be identical for a correct match. Here data_analysis_9 and data_analysis_11 are used


data_analysis_merged<-merge(data_analysis_9, 
                            data_analysis_11,
                            by='Factor')

#View result

View(data_analysis_merged)

# Joins from dplyr package can also be used for joining datasets and its code will be added soon

## Matching values between columns two dataframes

#Checking if two columns have any matching values

data_analysis_9$var_3 %in% data_analysis_11$var_1

#OR

# using Intersect

intersect(data_analysis_9$var_3,
          data_analysis_11$var_1)


#Unique value in one of the columns

setdiff(data_analysis_9$var_3,
        data_analysis_11$var_1)


## Exporting the result as a csv (file will be exported to the working directory)

write.csv(data_analysis_9,          # the dataset to be exported
          "data_analysis_9.csv")   # name of the file to be exported
