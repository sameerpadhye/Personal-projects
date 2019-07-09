#One way MANOVA

#libraries used

library(tidyverse)
library(readxl)
library(MASS)
library(magrittr)
library(plotrix)


# Data file path (Data file is assumed to be in the working directory)

data_file<- paste0(getwd(),"/MANOVA_data.xlsx")


#Importing data for analysis

data_analysis<-read_excel(data_path,
                          sheet=1)


#Assessing the normality of the variables using MVN package

if(!require(MVN))install.packages('MVN') 

mvn_data<-mvn(data = data_analysis%>%dplyr::select_if(is.numeric), 
              mvnTest = "dh")%>% 
    use_series(univariateNormality)%>%
    rownames_to_column()%>%
    dplyr::select(-c('rowname',
                     'Test'))

View(mvn_data)


# Checking homogeneity of variances of the variables using package heplots

if(!require(heplots))install.packages('heplots') 

homogeneity_var_data<-bartlettTests(data_analysis%>%dplyr::select_if(is.numeric),
                                    data_analysis$GROUP)%>%
    rownames_to_column()

View(homogeneity_var_data)


#Exploring the distribution of data

data_plot<-data_analysis%>%
    gather(ratio_type,
           ratios,
           Trait_1:Trait_8)%>%
    mutate_at(vars(contains('ratio_type')),
              as.factor)%>%
    group_by(GROUP,ratio_type)%>%
    summarize(means=mean(ratios,na.rm = T),
              std_error=plotrix::std.error(ratios,na.rm = T))%>%
    ggplot(aes(GROUP,
               means,
               group=GROUP))+
    geom_point(color='steelblue',
               size=4,
               position=position_dodge(0.5))+
    geom_errorbar(aes(ymin=means-std_error,
                      ymax=means+std_error),
                  width=.2,
                  position=position_dodge(0.5))+
    theme_bw(base_size = 18)+
    theme(axis.title.x = element_blank())+
    facet_wrap(~ratio_type,
               scales = 'free')+
    ylab('Mean')

data_plot 


#Performing MANOVA

data_manova<-manova(cbind(Trait_1,
                          Trait_2,
                          Trait_3,
                          Trait_4,
                          Trait_5,
                          Trait_6,
                          Trait_7,
                          Trait_8) ~ GROUP, data = data_analysis)

#Exploring the results of MANOVA

summary(data_manova)


#ANOVA's of individual response variables to the grouping variable (posthoc test)

post_aov_data<-summary.aov(data_manova)

#Exploring the results of MANOVA

post_aov_data
