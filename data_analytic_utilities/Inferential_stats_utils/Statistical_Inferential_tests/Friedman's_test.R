#Friedman's test (non parametric equivalent to a repeated measures ANOVA)


#libraries used

library(readxl)
library(tidyverse)


# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)

#data file path (Here data_for_visualization data have been used)

data_path<-paste0(getwd(),'/Friedman_sample_data.xlsx')


#Importing data for analysis 

data_analysis<-read_excel(data_path,
                          sheet=1)


#Exploring the dataset

head(data_analysis,5)


#For Friedman's test, we assume that the Response_A to Response_C is at different time periods for the same specimens (specimen row numbers not given)


#visualizing the data (data is inherently wide so its first converted into a long format)

data_analysis%>%
  tidyr::gather(Names,
                values,
                Response_A:Response_C)%>%
  ggplot(aes(Names,
             values))+
  geom_boxplot()

#Friedman's test

friedman_test<-data_analysis%>%
  as.matrix(.)%>%
  friedman.test(.)


#Results

friedman_test


#Posthoc pairwise comparisons are performed using the PMCMRplus package

if(!require(PMCMRplus))install.packages('PMCMRplus')

#test

data_analysis%>%
  as.matrix(.)%>%
  posthoc.friedman.nemenyi.test(.)
