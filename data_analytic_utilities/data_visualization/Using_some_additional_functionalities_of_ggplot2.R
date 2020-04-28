# Using some additional functionalities of ggplot2

# Libraries used

require(readxl)
require(tidyverse)


#Path for the data file (data_for_visualization.xlsx provided in the data can also be used for practice)

data_path<-paste0("C:/Users/samee/Desktop/Personal-projects/sample_datasets/data_for_visualizations.xlsx")

#Importing data for analysis

data_for_viz<-read_excel(data_path,
                         sheet=1)%>%
    mutate_if(is.character,
              as.factor)

# Exploring the data

str(data_for_viz)

#1. Adding subtitle and caption

plot_1<-data_for_viz%>%
    ggplot(aes(var_1,
               var_2))+
    geom_point()+
    labs(
        x = "Variable 1",
        y = "Variable 2",
        title = "Relationship of variable 1 with variable 2",
        subtitle = "Relationship between two variables",
        caption = "Data visualization techniques")
    
plot_1    
    
#2. Modifying the text style and background and margins
    
plot_1 +
    theme_classic()+
    theme(
        text = element_text(family = "Arial", 
                            color = "gray80"),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(color = "steelblue"),
        plot.background = element_rect(fill = "gray20"))
    )
