##Using simple logistic regression to assess the significance of certain envrionmental variables in explaining the occurrence of a zooplankton species


#libraries used
library(psych)
library(QuantPsyc)
library(tidyverse)
library(magrittr)
library(car)
library(corpcor)

#Importing the data
log_reg_data<-read.csv(paste0(getwd(),"/log_regression_sample_data.csv"),header=T)

#exploring the data (structure)
str(log_reg_data)

head(log_reg_data,3)

#Re-formatting the data for analysis 
#1. Making a new column using the species occurrence data (1: present and 0:absent)

data_for_analysis<-log_reg_data%>%
  dplyr::mutate(sp_occ=ifelse(Ilyocryptus_spinifer=='0',#Species name#
                                   'absent',
                                   'present'))%>%
  dplyr::select(-Ilyocryptus_spinifer)%>%
  dplyr::select(sp_occ,
         dplyr::everything())%>%
  dplyr::mutate_at(vars(matches("sp_")),
                   as.factor)%>%
  dplyr::mutate_at(vars(matches("Tot.sp.")),
                   as.integer)%>%
    dplyr::rename('habitat_type'='type')

head(data_for_analysis,3)
#checking for multicollinearity in the environmental descriptors

cor2pcor(cov(data_for_analysis[,c(3:7)]))

#Since none of the environmental variables are collinear, all of them are used in the regression analysis

#plots to check how environmental variables fare in terms of presenceabsence of the species

#reformatting data into a long version

data_for_analysis%>%
  dplyr::select(-c(habitat_type,
                   Total.sp., 
                   Aq..Veg))%>%
  gather(env_var,
         value,
         Altitude:Salinity)%>%
  mutate_at(vars(matches('env_var')),
            as.factor)%>%
    ggplot(aes(x=sp_occ,
               y=value))+
    geom_boxplot(fill="grey")+
    facet_wrap(~env_var,scales = "free")+
    theme_bw(base_size = 16)

#defining the formula for the regression
reg.formula<-reformulate(names(data_for_analysis)[c(3:7)], 
                        names(data_for_analysis[1]))

#Logistic regression using all environmental descriptors
logistic_model<-glm(reg.formula,
                    data=data_for_analysis,
                    family=binomial)

#Summary of the model
summary(logistic_model)

## Obtaining the odds ratios
exp(coef(logistic_model))

#classification of the cases based on the model
QuantPsyc::ClassLog(logistic_model,data_for_analysis$sp_occ)


