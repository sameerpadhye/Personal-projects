#Some plots for data visualization using plotly. Detailed information for plotly can be found on https://plot.ly/

#libraries used

library(readxl)
library(tidyverse)
library(GGally)
library(RColorBrewer)
library(plotly)


#Path for the data file (data_for_visualization.xlsx provided in the data can also be used for practice)

data_path<-paste0(getwd(),"/data_for_visualization.xlsx")

#Importing data for analysis

data_for_viz<-read_excel(data_path,
                     sheet=1)%>%
    mutate_if(is.character,
              as.factor)

#exploring the structure of the data
str(data_for_viz)

# Some data visualizations

#Boxplot 

data_for_viz%>%
    plot_ly(x = ~Factor,
            y=~var_1,
            type = "box",
            color = ~Factor)%>%
    layout(title = 'Boxplot',
           axis = list(title = "Factors"),
           yaxis = list(title = "variable 1"))


#Scatter plot 

#.a with a single color (for data points)

data_for_viz%>%
plot_ly(x = ~var_2, 
        y = ~var_3,
        marker = list(size = 10,
                      color = "forestgreen",
                      line = list(color = 'black',
                                  width = 1)))%>%
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 2"),
           yaxis = list(title = "variable 3"))

#.b with color based on a category (for data points)

data_for_viz%>%
    plot_ly(x = ~var_2, 
            y = ~var_3,
            type = 'scatter',
            color = ~Factor)%>%
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 2"),
           yaxis = list(title = "variable 3"))


#Histogram

#a. Histogram of a variable without factor based separation

data_for_viz%>%
    plot_ly(x=~var_4,
            type = 'histogram')%>%
    layout(title = "Histogram", 
           xaxis = list(title = "variable 4"), 
           yaxis = list(title = "Count"))

#b. histogram of the variable considering factor wise separation

data_for_viz%>%
    plot_ly(x=~var_4,
            type = 'histogram',
            color = ~Factor,
            split = ~Factor)%>%
layout(title = "Histogram", 
       xaxis = list(title = "variable 4"), 
       yaxis = list(title = "Count"))


# Simple Barplot

data_for_viz%>%
    plot_ly(x = ~Factor,
            y=~var_3,
            type = "bar")%>%
    layout(title = 'Barchart',
           xaxis = list(title = "Factors"),
           yaxis = list(title = "variable 3"))


#Stacked barplot

data_for_viz%>%
    plot_ly(x = ~Factor,
            y=~var_3,
            type = "bar")%>%
    add_trace(y = ~var_2, name = 'variable 2')%>%
    layout(barmode = 'group',
           title = 'Barchart',
           xaxis = list(title = "Factors"),
           yaxis = list(title = "variable 3"))


#3D scatter

data_for_viz%>%
    plot_ly (type = "scatter3d" , 
             x = ~var_1, 
             y = ~var_2, 
             z = ~var_3,
             mode = "markers" )%>%
    layout(title = '3D scatterplot',
           xaxis = list(title = "variable 1"),
           yaxis = list(title = "variable 2"),
           zaxis=list(title = "variable 3"))


#2D histogram

data_for_viz%>%
    plot_ly (type = "histogram2d" , 
             x = ~var_2, 
             y = ~var_4)%>%
    layout(title = '2D Histogram',
           xaxis = list(title = "variable 1"),
           yaxis = list(title = "variable 2"))


#The plots can be modified much more based on the requirement
