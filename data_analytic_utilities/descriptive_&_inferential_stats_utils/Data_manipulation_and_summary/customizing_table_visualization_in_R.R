## Formatting and customizing tables in R


# Libraries used

 
library(tidyverse)


#Data used


table_data<-data.frame(
    trait_1=c(10,12,17,29,35,34,56,89,112,156),
    trait_2=seq(160,1,length.out = 10),
    trait_3=runif(10),  
    trait_4=sample(c(1:200),10,replace = T)) 


## Basic interactive table using DT package


if(!require(DT))install.packages('DT')


library(DT)


# Obtaining the table


#1. rounding the values 


rounded_table<-round(table_data,3)


#2. Interactive table


mod_table<-datatable(rounded_table,
                          rownames = TRUE,
                          width = '100%', 
                          height = '100%')


#View the table (in the Viewer)


mod_table


# Creating tables using expss package


if(!require(formattable))install.packages('formattable')


library(formattable)


#Obtaining the basic table


mod_table2<-table_data %>% 
    formattable(.)


#View the table (in the Viewer)


mod_table2


# Tabulating Regression results using jtools package


if(!require(jtools))install.packages('jtools')


library(jtools)


# Linear Regression using the above dataset


reg_model<-lm(trait_2~trait_1,data=table_data)


# Tabulated result using summ function


jtools::summ(reg_model)


#Adding extra information in the table


# Confidence interval


jtools::summ(reg_model,confint=TRUE)


# Remove p value


jtools::summ(reg_model,pvals=FALSE)


## Using the package stargazer to export tables in doc file


install.packages('stargazer')


library(stargazer)


# Table output as a doc file as html type of output


stargazer(table_data,
          type = "html",
          out="table_data.doc")


#Output the table as a doc file as text type of output


stargazer(reg_model,
          type="text",
          out = "table2.doc")


# Using the package sjPlot to export result tables (from correlation, regression etc.)


install.packages('sjPlot')


library(sjPlot)


## Correlation table using sjPlot


# Correlation of the table data


correlation_analysis<-cor(table_data)


# Exporting the correlation_analysis table in a doc file

sjt.corr(correlation_analysis,
         corr.method = "pearson",
         file="sjt_corr_3.doc")

## WIP
