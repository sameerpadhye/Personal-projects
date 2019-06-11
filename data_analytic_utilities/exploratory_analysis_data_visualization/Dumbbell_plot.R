##Creating Dumbbell plots for data visualization


#Libraries used

library(tidyverse)
library(readxl)


#Path for the data file (assuming the data file is stored in the working directory)

data_path<-paste0(getwd(),"/data_dumbbell_plot.xlsx")

#Importing data for analysis

data_for_plot<-read_excel(data_path,
                          sheet=1)%>%
    mutate_if(is.character,
              as.factor)


#Re-ordering the sites based on their sequence using DescTools

data_for_plot$sites<-reorder(data_analysis$sites)


#Dumbbell plot using ggalt library

library(ggalt)

#Plot

data_for_plot%>%
    ggplot(aes(x=population_1,
               xend=population_2,
               y=sites)) + 
    geom_dumbbell(color="black", 
                  size_x=3.5, 
                  size_xend = 3.5,
                  colour_x="orange", 
                  colour_xend = "grey30")+
    theme_bw(base_size = 18)+
    xlab('Population')+ 
    ylab('Sites')+
    geom_text(color="black", 
              size=4, 
              hjust=-0.5,
              vjust=-0.5,
              aes(x=population_1, 
                  label=population_1))+
    geom_text( color="black", 
               size=4, 
               hjust=1.6,
               vjust=-0.2,
               aes(x=population_2, 
                  label=population_2))+
    labs(title = 'Dumbbell plot')


#Dumbbell plot using plotly

library(plotly)

data_for_plot%>%
plot_ly(color = I("grey80")) %>%
    add_segments(x = ~population_1, 
                 xend = ~population_2, 
                 y = ~sites, 
                 yend = ~sites, 
                 showlegend = FALSE)%>%
    add_markers(x = ~population_1, 
                y = ~sites, 
                name = "Population 1",
                marker = list(
                    color = 'steelblue',
                    size = 10,
                    line = list(
                        color = 'black',
                        width = 1.2)))%>%
    add_markers(x = ~population_2, 
                y = ~sites, 
                name = "Population 2",
                marker = list(
                    color = 'orange',
                    size = 10,
                    line = list(
                        color = 'black',
                        width = 1.2)))%>%
    layout(title = 'Dumbbell plot',
           xaxis = list(title = "Population",
                        size=20),
           yaxis = list(title = "Sites",
                        size=20))
