#Chisquare test of Independence and Goodness of fit

##Test of Independence

#For information on the test, please refer to https://www.statisticssolutions.com/using-chi-square-statistic-in-research/

#libraries used

library(tidyverse)
library(readxl)
library(magrittr)

#data file path.

data_file_path<-"C:/Users/samee/Desktop/R data/sample_datasets/Chisquare_sample_data.xlsx"

#Importing the data. 

data_analysis<-read_excel(data_file_path,
                          sheet=1)%>%
    column_to_rownames(.,'Categories')

#Exploring the dataset

head(data_analysis,5)   

#Visualizing the strength of association of each category with each column
#Data is first converted into a table and then visualized by a balloonplot
#This part has been adpoted from http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r

table_data<-as.table(as.matrix(data_analysis))

library(gplots)

balloonplot(t(table_data), 
            main ="Association strength", 
            xlab ="", 
            ylab="",
            label = FALSE, 
            show.margins = FALSE)

#For direct visualization of the tables with values

library(gridExtra)

library(grid)

grid.table(table_data)

#Performing the Chisquare test. 

chisq_test<-chisq.test(data_analysis)


#results

#P value of the test can also be calculated by using Monte Carlo Simulation by using the 'simulate.p.value' argument

#Chisquare statistic

chisq_test$p.value

#Chisquare statistic

chisq_test$statistic


#Testing the strength of association using Cramer's V and Phi Coefficient (High values suggests strong association)

if(!require(DescTools))install.packages('DescTools')

DescTools::Phi(table_data)

DescTools::CramerV(table_data)


##Goodness of fit

#Same data is used for convenience.


#Obtaining the proportions (observed)

goodness_data<-data_analysis%>%
    rownames_to_column(.)%>%
    dplyr::select(rowname,
                  Phenomenon_1)%>%
    column_to_rownames('rowname')%>%
    mutate(freq=Phenomenon_1/sum(Phenomenon_1))%>%#obtaining the expected proportions
    mutate(obs_values=data_analysis$Phenomenon_2) #using the Phenonmenon 2 values here as observed values 


#Exploring the new data

head(goodness_data,5)


#Goodness of fit test

goodness_chisq<-goodness_data%$% # using the %$% operator from magrittr 
    chisq.test(x=obs_values,
               p=freq)


#Result

#statistic

goodness_chisq$statistic

#p value

goodness_chisq$p.value

