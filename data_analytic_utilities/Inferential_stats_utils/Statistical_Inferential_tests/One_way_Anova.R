##Performing a One way parametric ANOVA 

#Libraries used
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(magrittr)
library(pipeR)
library(readxl)

#data file (data assumed to be saved in the working directory)

data_file<-paste0(getwd(),"/data_for_analysis_ANOVA.xlsx")


#Data for analysis. Data should be in long format (stacked data)

data_for_analysis<-read_excel(data_file,
                              sheet=2)%>%
    mutate_if(is.character,
              as.factor)

#Viewing the data

head(data_for_analysis,3)

# Viewing the levels of the Factor

levels(data_for_analysis$Factor)

#Summarizing the data

data_for_analysis%>%
    group_by(Factor)%>%
    summarise(mean_values=mean(Trait,
                               na.rm=T),
              sd_values=sd(Trait,
                           na.rm = T))

#Data visualization

data_for_analysis%>%
    plot_ly(x = ~Factor, 
            y = ~Trait,
            color=~Factor) %>%
    add_boxplot(opacity=0.6)%>%
    layout(title = 'Boxplot',
           axis = list(title = "Factors"),
           yaxis = list(title = "Trait"))

#Checking the data for normality 

#Normality (Shapiro Wilk test)

data_for_analysis%>%
    group_by(Factor)%>%
    summarise(shapiro_W=shapiro.test(Trait)$statistic,
              shapiro_p=shapiro.test(Trait)$p.value)

#ANOVA

#1. using AOV

data_for_analysis%$%
    aov(Trait~Factor,
        data=.)%>%
    summary(.)

#2. using lm

lm_aov_model<-data_for_analysis%$% #this type of pipe is used from piper library 
    lm(Trait~Factor,
       data=.)

summary(lm_aov_model)

# Checking the residuals distribution 

# Homoskedasticity

plot(lm_aov_model) 

# residual normality

hist(residuals(lm_aov_model)) 

#Tukey Posthoc test

data_for_analysis%$%
    aov(Trait~Factor,
        data=.)%>%
    TukeyHSD(.)

