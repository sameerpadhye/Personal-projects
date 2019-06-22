#Using Lollipop plots in data visualization

#libraries used

library(readxl)
library(tidyverse)

#Path for the data file (assuming the data file is stored in the working directory)

data_path<-paste0(getwd(),"/data_for_analysis.xlsx")

#Importing data for analysis

data_for_plot<-read_excel(data_path,
                     sheet=1)%>%
    mutate_if(is.character,
              as.factor)

#Re-ordering the factors (based on variable of choice, here its var_3)

data_for_plot$Factor<-fct_reorder(data_for_plot$Factor, 
                                -data_for_plot$var_3) 

# Lollipop plot (Data is summarized and that is used as length of the segment; A base plot is provided here. Additional customizations can be carried out using ggplot2 functions)

data_for_plot%>%
    group_by(Factor)%>% # grouped by Factor and then summarized
    summarize(mean_1=mean(var_1,na.rm=T),
           mean_2=mean(var_2,na.rm=T),
           mean_3=mean(var_3,na.rm=T),
           mean_4=mean(var_4,na.rm=T))%>%
    ggplot(aes(x=Factor,
               y=mean_3))+
    geom_segment(aes(x=Factor, 
                     xend=Factor, 
                     y=0, 
                     yend=mean_3),
                 color='grey50',
                 size=1.5)+ # thickness of the segment
    geom_point(color='orange',
               size=5)+ # size of the point
    theme_classic(base_size = 16)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1))+
    xlab("Factors")+
    ylab("mean_value_3")

