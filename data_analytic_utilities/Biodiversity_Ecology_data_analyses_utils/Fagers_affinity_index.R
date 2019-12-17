## Calculation of Fager's index of affinity to study the strength of co-occurences of species pairs in species communities


# Libraries used


library(tidyverse)
library(readxl)

#Data_file_path


data_path<-paste0(getwd(),"/Data_for_Fager_index.xlsx")


#Importing data for analysis


data_analysis<-read_excel(data_path,
                          sheet=1)


# Exploring the data


head(data_analysis,5)


# Fager's index is given by the formula 2 * J/Na + Nb where 'J' are the total joint occurrences of the species, 'Na' and 'Nb' are the total occurrences of species 'a' and 'b' respectivley


# Step 1: To calculate 2 * J

# The sum of occurrences of two species is calculated and then converted to 1 and 0 where 1 corresponds to co-occurence and 0 does not. 

joint_occ_1<-data_analysis[,"Species_1"]+data_analysis[,"Species_2"]

joint_occ_2<-ifelse(joint_occ_1>=2,1,0)%>%
    sum(.)

# This value is then multiplied by 2 to get the 2 * J term value

two_J<-2*joint_occ_2

two_J


## Step 2: Calculation of total occurrences of the individual species


Na_Nb<-sum(data_analysis[,"Species_1"]+data_analysis[,"Species_2"])

Na_Nb


## Step 3: Calculating the Fager's index

Fag_aff_index<-two_J/Na_Nb

Fag_aff_index


## This can be used to obtain the value for any two species combinations from the dataset

# Function to calculate Fager's index

Fager_index<-function(x,                    # dataset (Species occurrence data)
                    name_1 = colnames(x),  # species 1
                    name_2=colnames(x)) {  # species 2 
    
    joint_occ_1<-x[,name_1]+x[,name_2]
    
    joint_occ_2<-ifelse(joint_occ_1>=2,1,0)%>%
        sum(.)
    
    two_J<-2*joint_occ_2
    
    Na_Nb<-sum(x[,name_1]) + sum (x[,name_2])
    
   Fager_value<-two_J/Na_Nb
    
    return(Fager_value)
    
}



