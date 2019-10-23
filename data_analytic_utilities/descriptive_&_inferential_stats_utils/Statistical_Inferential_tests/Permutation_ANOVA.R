##Performing a One way permutational ANOVA 

#Libraries used

library(tidyverse)
library(plotly)
library(magrittr)
library(readxl)

#data file (data assumed to be saved in the working directory)


data_file<-paste0(getwd(),"/data_for_analysis_ANOVA.xlsx")


#Data for analysis. 


data_for_analysis<-read_excel(data_file,
                              sheet=2)%>%
    mutate_if(is.character,
              as.factor)
 

#Viewing the data]


head(data_for_analysis,3)


# Viewing the levels of the Factor


levels(data_for_analysis$Factor)


#Data visualization


data_for_analysis%>%
    plot_ly(x=~Factor,
               y=~Trait,
            type='box',
            color = ~Factor)%>%
    layout(title = 'Boxplot',
           axis = list(title = "Habitats"),
           yaxis = list(title = "Trait values"))


#Checking the data for normality 


#Normality (Shapiro Wilk test)


data_for_analysis%>%
    group_by(Factor)%>%
    summarise(shapiro_W=shapiro.test(Trait)$statistic,
              shapiro_p=shapiro.test(Trait)$p.value)


# ANOVA by permutation is conducted using the package lmPerm


require(lmPerm)


# ANOVA test


anova_perm<-aovp(Trait~Factor, # the formula
     data_for_analysis, # the data
     seqs = FALSE) # this is used when the design is unbalanced


summary(anova_perm)
