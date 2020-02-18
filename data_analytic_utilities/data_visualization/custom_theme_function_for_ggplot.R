# Creating and using custom theme function in ggplot visualization

# Common libraries used

library(tidyverse)
library(magrittr)
library(readxl)


# data path

data_path<-"C:/Users/samee/Desktop/Personal-projects/sample_datasets/data_for_visualizations.xlsx"

data_for_plot<-read_excel(data_path,sheet = 1)

head(data_for_plot)

# Basic ggplot boxplot visualization

Plot_data<-data_for_plot%>%
    ggplot(aes(x=Factor,
               y=var_1))+
    geom_boxplot(fill="gray60")+
    labs(x="Factor",y="Variable 1 values",
        title = "Boxplot of factors with variables",
         subtitle = "Variable 1 variation with all factors",
         caption = "Factor_C showed the least median value")

Plot_data

# Writing a custom theme function which can be used with the plot

theme_custom<-function(){
    theme_bw(base_size = 15)+
    theme(
        text = element_text(family = "Arial",
                            color = "black"),
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(color = "gray50"),
        plot.margin = unit(c(5, 10, 5, 10), units = "mm")
    )
    
    
    
}

# Using the function with the plot

Plot_data+theme_custom()
