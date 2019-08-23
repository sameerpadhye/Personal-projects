#Basics of data import, manipulation and summary of data used in diversity/ecology studies 

# Data used here (as in many studies) are dataframe data structures


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


