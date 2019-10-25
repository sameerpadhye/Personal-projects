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


#Using error blocks in permutation ANOVA's 


# Here an additional category with 4 levels is used as a block variable in the analysis


data_block_analysis<-read_excel(data_file_path,
                                sheet=3)


# Same library LmPerm is used here


# ANOVA test


anova_perm_block<-aovp(Trait_2~Factor_level1+Error(Factor_level2), # the formula using the Error term (Factor_level2)
                       data_block_analysis, # the data
                       seqs = FALSE) # since the data is unbalanced


summary(anova_perm_block)
