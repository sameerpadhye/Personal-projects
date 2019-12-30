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

### Obtaining index value for a multi species community 

# This has been done using 'combn' function in r


# Exploring how the combinations look like

sp_combo<-combn(colnames(data_analysis),
                2,simplify = T)

sp_combo

# Writing a function which will utilize these combinations to get the Fager's indices for the dataset

fg_index<-function(x){ # x- species combinations dataset
     
    joint_occ_1<-rowSums(data_analysis[,x])
    
    joint_occ_2<-ifelse(joint_occ_1>=2,1,0)%>%
        sum(.)
    
    joint_occ_3<-2*joint_occ_2
    
    tot_occ_1<-sum(data_analysis[,x])
    
    joint_occ_3/tot_occ_1
    
}

# Using the function to obtain values of species combinations

Fager_values<-apply(combn(colnames(data_analysis),2),2,fg_index)

View(Fager_values)

# The result obtained is a vector with no species names. Hence the species combinations are added to the result

Fager_data<-cbind(Values=round(Fager_values,4),
                  t(sp_combo))%>%
    data.frame(.)%>%
    dplyr::rename('Species_A'='V2',
                  'Species_B'='V3')%>%
    dplyr::mutate_at(vars(contains('Value')),
                     as.character)%>%
    dplyr::mutate_if(is.character,
                     as.numeric)%>%
    arrange(desc(Values))

head(Fager_data)

