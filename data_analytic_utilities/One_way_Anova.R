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
    ggplot(aes(x=Factor,
               y=Trait))+
    geom_boxplot(col='black',
                 fill="orange")+
    theme_bw(base_size = 18)+
    ylab("Trait_values")+
    xlab("Factor")

#Checking the data for normality 

#Normality (Shapiro Wilk test)

data_for_analysis%>%
    group_by(Factor)%>%
    summarise(shapiro_W=shapiro.test(Trait)$statistic,
              shapiro_p=shapiro.test(Trait)$p.value)

#ANOVA

data_for_analysis%$%
    aov(Trait~Factor,
        data=.)%>%
    summary(.)

#Tukey Posthoc test
data_for_analysis%$%
    aov(Trait~Factor,
        data=.)%>%
    TukeyHSD(.)
