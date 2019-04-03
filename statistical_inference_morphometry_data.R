##Exploring and testing the differences in morphometric traits of a commonly occurring freshwater zooplankter, Ceriodaphnia cornuta from a few localities in tropical India

#Libraries used
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(vegan)
library(psych)
library(FSA)
library(ggpubr)
library(ggfortify)

#loading and editing the data

cerio_data<-read.csv(file.choose(),header=T)

# Selecting and modifying data for analysis
cerio_morphometry<-
    dplyr::select(cerio_data,
                  site,
                  longitudo_corporis,
                  Altitudo_carapacis)%>%
    dplyr::rename('tot.len'='longitudo_corporis',
                  'tot.wid'='Altitudo_carapacis')%>%
    arrange(site)%>%
    mutate(ratio=tot.len/tot.wid)%>%
    dplyr::select(site,ratio)

#Viewing the data
head(cerio_morphometry,3)

#Checking the data for normality and variance homogeneity

#1. Normality (Shapiro Wilk test)
shapiro.test(cerio_morphometry$ratio)

#2. Homogeneity of variances (Levene's Test)

leveneTest(ratio ~ site, 
           data=cerio_morphometry)

#Since the data are non normal, Kruskal Wallis test has been used

#Krusal Wallis test for non normal data
kruskal.test(ratio~site,data=cerio_morphometry)

#post hoc test for Kruskal
dunnTest(ratio ~ site,data=cerio_morphometry,method="bh")

# plotting Kruskal test
ggboxplot(cerio_morphometry, 
          x = "site", 
          y = "ratio",
          color = "site",
          palette="jco",
          add = "jitter",
          shape = "site")+
    theme_bw(base_size = 16)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1))+
    xlab("Sites")+
    ylab("Ratio")

############################END########################################

