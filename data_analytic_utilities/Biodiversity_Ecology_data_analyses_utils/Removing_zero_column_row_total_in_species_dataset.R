# Removing the sites/species with zero totals 

# In many instances, it is observed that there are rows and columns which have no values and can cause problems in analysis

# libraries used

require(readxl)
require(tidyverse)


# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-"C:/Users/samee/Desktop/Personal-projects/sample_datasets/species_richness_data.xlsx"


#Importing data for analysis (Species abundance dataset used here)

data_analysis<-read_excel(data_path,
                          sheet=1)

# Exploring the data

str(data_analysis)


#Exploring the data for any zero row and column totals.Since Sites column is not numerical, only the numerical data needs to be selected

numeric_data<-subset(data_analysis,
                     select=-c(Sites))

# check the columns for zero Column sums

which(colSums(numeric_data)==0)

# check the rows for zero Rowsums

which(rowSums(numeric_data)==0)

## Removing the columns/rows with zero totals using base R

# Removing columns

col_select<-numeric_data[,-which(colSums(numeric_data)==0)]

# Removing Rows

row_select<-numeric_data[-which(rowSums(numeric_data)==0),]

## Removing the columns/rows with zero totals using tidyverse

data_select<-numeric_data%>%
    filter_all(., any_vars(. != 0))%>%
    select_if(.,~sum(.)!=0)

