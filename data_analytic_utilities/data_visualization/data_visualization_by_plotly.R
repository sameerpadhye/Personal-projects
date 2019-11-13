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

#OR

data_for_viz %>%
    plot_ly(x = ~Factor, 
            y = ~var_1,
            color=~Factor) %>%
    add_boxplot(opacity=0.6)%>%
    layout(title = 'Boxplot',
           axis = list(title = "Factors"),
           yaxis = list(title = "variable 1"))


#Scatter plot 

#Scatter plot 

#.a1 with a single color (for data points)

data_for_viz%>%
    plot_ly(x = ~var_2, 
            y = ~var_3,
            marker = list(size = 10,
                          color = ~Factor,
                          line = list(color = 'black',
                                      width = 1)))%>%
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 2"),
           yaxis = list(title = "variable 3"))

#OR

data_for_viz%>%
    plot_ly(x =~var_2, 
            y =~var_3) %>%
    add_markers(color=I("#111e6c"))%>%
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 2"),
           yaxis = list(title = "variable 3"))


#.a2 with a single color,size, shape and opacity (for data points)

data_for_viz%>%
    plot_ly(x = ~var_2, 
            y = ~var_3) %>% 
    add_markers(marker = list(color='orange',
                              symbol = "diamond", 
                              size = 6,
                              opacity=0.4))%>%
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 2"),
           yaxis = list(title = "variable 3"))


#.b with color based on a category (for data points)

data_for_viz%>%
    plot_ly(x = ~var_2, 
            y = ~var_3,
            type = 'scatter',
            color = ~Factor)%>%  # here the numbers based on a continous variable as well such as e.g. log(var_1)
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 2"),
           yaxis = list(title = "variable 3"))


#.c with different symbols for different categories 

data_for_viz%>%
    plot_ly(x = ~var_1, 
            y = ~var_3, 
            color = ~Factor, 
            colors = "Set1",
            symbol = ~Factor, 
            symbols = c('circle','x','o'),
            size=~var_4)%>%
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 1"),
           yaxis = list(title = "variable 3"))

#OR

data_for_viz %>%
    plot_ly(x = ~var_1, 
            y = ~var_3, 
            symbol = ~Factor)  %>%
    add_markers()%>%
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 1"),
           yaxis = list(title = "variable 3"))


# Bubblechart (scatterplot with size based on a specific variable)

#.a with a single color

data_for_viz%>%
    plot_ly(x = ~var_2, 
            y = ~var_3, 
            type = 'scatter', 
            mode = 'markers',
            marker = list(size = ~var_4, 
                          opacity = 0.6,
                          color='forestgreen'),
            size=~var_4)%>%
    layout(title = 'Bubbleplot',
           xaxis = list(title = "variable 1"),
           yaxis = list(title = "variable 3"))

#.b with a color scale based on a continous variable

data_for_viz%>%
    plot_ly(x = ~var_2, 
            y = ~var_3, 
            type = 'scatter', 
            mode = 'markers',
            color = ~var_4, 
            colors = 'Reds',
            marker = list(size = ~var_4, 
                          opacity = 0.6),
            size=~var_4)%>%
    layout(title = 'Bubbleplot',
           xaxis = list(title = "variable 1"),
           yaxis = list(title = "variable 3"))

#.c with a color scale based on a categorical variable

data_for_viz%>%
    plot_ly(x = ~var_2, 
            y = ~var_3, 
            type = 'scatter', 
            mode = 'markers',
            color = ~Factor, 
            colors = 'Set2',
            marker = list(size = ~var_4, 
                          opacity = 0.6),
            size=~var_4)%>%
    layout(title = 'Bubbleplot',
           xaxis = list(title = "variable 1"),
           yaxis = list(title = "variable 3"))


#Histogram

#a. Histogram of a variable without factor based separation

data_for_viz%>%
    plot_ly(x=~var_4,
            type = 'histogram')%>%
    layout(title = "Histogram", 
           xaxis = list(title = "variable 4"), 
           yaxis = list(title = "Count"))

#OR

data_for_viz %>%
    plot_ly(x = ~var_4) %>%
    add_histogram(color=I("#111e6c"))%>%
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

#OR

data_for_viz %>%
    plot_ly(x = ~Factor, 
            y = ~var_2, 
            color = ~Factor) %>%
    add_bars()%>%
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


#Line chart

#a. Simple line chart

data_for_viz%>%
    plot_ly(x = ~var_1, 
            y = ~var_4, 
            type = 'scatter',
            mode = 'lines')%>%
    layout(title = 'Line plot',
           xaxis = list(title = "variable 1"),
           yaxis = list(title = "variable 4"))

#b. Line chart of different dependent variables (please note that the Y axis should have the same units)

data_for_viz%>%
    plot_ly(x = ~var_1,
            y=~var_2,
            name = 'variable 2', 
            type = 'scatter', 
            mode = 'lines') %>%
    add_trace(y = ~var_3, 
              name = 'variable 3',
              mode = 'lines') %>%
    add_trace(y = ~var_4, 
              name = 'variable 4', 
              mode = 'lines+markers') %>%
    layout(title = 'Line plots of 3 variables',
           xaxis = list(title = "variable 1"),
           yaxis = list(title = "values"))


#Pie chart 

data_for_viz%>%
    plot_ly(labels = ~Factor, 
            values = ~var_1, 
            type = 'pie')%>%
    layout(title = 'Var_1 proportion w.r.t. the Factor',
           xaxis = list(showgrid = T),
           yaxis = list(showgrid = T))


#Violin plot

data_for_viz%>%
    plot_ly(
        x=~Factor,
        y = ~var_4,
        type = 'violin',
        split = ~Factor,
        box = list(
            visible = T
        ))%>%
    layout(title = 'Var_4 distribution w.r.t. the Factor',
           xaxis = list(title="Factor",
                        showgrid = T),
           yaxis = list(title="var_4 values",
                        showgrid = T))


# Density plots

var1_density <- density(data_for_viz$var_1, 
                        na.rm = TRUE)

var3_density <- density(data_for_viz$var_3, 
                        na.rm = TRUE)

# Overlay density plots

plot_ly() %>%
    add_lines(x = ~var1_density$x, 
              y = ~var1_density$y, 
              name = "variable_1", 
              fill = 'tozeroy') %>%
    add_lines(x = ~var3_density$x, 
              y = ~var3_density$y, 
              name = "variable_3", 
              fill = 'tozeroy') %>%
    layout(title="Variable density plots",
           xaxis = list(title = 'Variables'),
           yaxis = list(title = 'Density'))


# Modifications which can be made in the basic plots

#1. Changing the hover information using just one axis in the dataset

data_for_viz%>%
    plot_ly(x =~var_2, 
            y =~var_3,
            hoverinfo='y') %>% ## Hoverinformation 
    add_markers(color=I("#111e6c"))%>%
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 2"),
           yaxis = list(title = "variable 3"))


#2. Changing the hover information using all available information as given in the dataset

data_for_viz%>%
    plot_ly(x =~var_2, 
            y =~var_3,
            color=~Factor,
            hoverinfo='text',
            text=~paste("var_2:", var_2, "<br>",
                        "var_3:", var_3, "<br>",
                        "Factor:", Factor))%>% ## Hoverinformation customized
    add_markers()%>%
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 2"),
           yaxis = list(title = "variable 3"))



#3. Transforming the x-axis (log scale shown here)

data_for_viz%>%
    plot_ly(x =~var_1, 
            y =~var_2)%>% 
    add_markers(color=I("black"),
                size=5,
                opacity=0.5)%>%
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 2",
                        type= "log"), #log conversion for xaxis added here
           yaxis = list(title = "variable 3",
                        type='log')) #log conversion for yaxis added here



#4. Set the background color to #ebebeb and remove the vertical grid

data_for_viz%>%
    plot_ly(x =~var_1, 
            y =~var_2)%>% 
    add_markers(color=I("orange"),
                size=5,
                opacity=0.3)%>%
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 2"), 
           yaxis = list(title = "variable 3"),
           paper_bgcolor="gray40")  # background color added here


#5. Visualizing a fitted regression model 

#model

reg_model <- lm(var_1 ~ var_4, 
                data = data_for_viz)

# plot

data_for_viz%>%
    plot_ly(x =~var_4, 
            y =~var_1,
            hoverinfo='text',
            text=~paste("var_2:", var_2, "<br>",
                        "var_3:", var_3, "<br>",
                        "Factor:", Factor))%>% 
    add_markers(showlegend=FALSE)%>%
    layout(title = 'Scatterplot',
           xaxis = list(title = "variable 2"),
           yaxis = list(title = "variable 3"))%>%
    add_lines(y=~fitted(reg_model)) # line fitted here


#6. Creating subplots

# First scatterplot

plot_A <- data_for_viz %>%
    filter(Factor == "Factor_A") %>%
    plot_ly(x = ~var_1, 
            y = ~var_2) %>% 
    add_markers(name = "Factor_A")

# Second scatterplot

plot_B <- data_for_viz %>%
    filter(Factor == "Factor_B") %>%
    plot_ly(x = ~var_1,
            y = ~var_2) %>% 
    add_markers(name = "Factor_B")

# Create a facted scatterplot containing both the plots

subplot(plot_A, 
        plot_B, 
        nrows =2,
        shareX = TRUE,
        shareY = TRUE)%>%
    layout(title = "Factors A & B vs variables 1 & 2")


#7. Creating a scatterplot matrix (of scatterplots)

data_for_viz%>%
    plot_ly(color=~Factor) %>%
    add_trace(
        type = 'splom',
        dimensions = list(
            list(label = 'variable_1', 
                 values = ~var_1),
            list(label = 'variable_2', 
                 values = ~var_2),
            list(label = 'variable_3', 
                 values = ~var_3)
        )
    )%>%
    style(diagonal = list(visible = FALSE),
          showupperhalf = FALSE)%>%
    layout(title = "Scatterplot matrix of variables 1-3")


#8. adding a specific line in the graph

data_for_viz %>%
  filter(Factor == "Factor_A") %>%
  plot_ly(x = ~var_1, 
          y = ~var_2) %>% 
  add_markers(name = "Factor_A") %>%
  add_lines(x = c(70, 200), 
            y = c(100,150)) %>%
  layout(showlegend=FALSE)


#9. reordering the factors using fct_reorder from forcats package based on a specific value

data_for_viz %>%
  plot_ly(x = ~fct_reorder(Factor,
                           var_4),
          y = ~var_2, 
          color = ~Factor) %>%
  add_bars()%>%
  layout(title = 'Barchart',
         xaxis = list(title = "Factors"),
         yaxis = list(title = "variable 3"))


## Adding animation to the plots


# Base plot 


animated_plot<-data_for_viz%>%
  plot_ly(x = ~var_1, 
          y = ~var_4)%>%
  add_markers(size = ~var_2, color = ~Factor, 
              frame = ~ Factor,
              marker = list(sizemode = "diameter"))%>%
  layout(title = 'Bubbleplot',
         xaxis = list(title = "variable 1"),
         yaxis = list(title = "variable 3")) %>%
  layout(showlegend = FALSE)


# Manipulating the animated component


animated_plot %>% 
  animation_opts(
    frame = 700,        # frame rate      
    transition = 400,  # holdup time for each frame
    easing = "linear"  # how the frame progress
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = NULL, 
      font = list(color = "forestgreen")
    )
  ) 


# Adding animated background text and remvoing the slider information


data_for_viz%>%
  plot_ly(x = ~var_1, 
          y = ~var_4,
          hoverinfo = 'text',
          text= ~Factor)%>%
  add_text(x= 200,
           y=750,
           text=~Factor,
           frame=~Factor,
           textfont=list(color = toRGB('gray50'),
                         size=70))%>%
  add_markers(size = ~var_3, color = ~Factor, 
              frame = ~ Factor,
              marker = list(sizemode = "diameter"))%>%
  layout(title = 'Bubbleplot',
         xaxis = list(title = "variable 1"),
         yaxis = list(title = "variable 4")) %>%
  animation_slider(hide=TRUE)%>%
  animation_opts(
    frame = 800,             
    transition = 400,  
    easing = "linear"  
  )%>%
  hide_legend()



## Linking two plots. Linking is achieved by using the package crosstalk


require(crosstalk)


# Creating a shared object using crosstalk package


shared_data <- SharedData$new(data_for_viz)


#1. Creating a scatterplot of variable 1 and variable 4


plot_1 <- shared_data %>%
  plot_ly(x = ~var_1, 
          y = ~var_4) %>%
  add_markers()

#2. Creating a scatterplot of variable 3 and variable 4


plot_2 <- shared_data %>%
  plot_ly(x = ~var_3, 
          y = ~var_4) %>%
  add_markers()


# Using subplot function to make a plot of the plots above


subplot(plot_1, 
        plot_2, 
        titleX = TRUE, 
        shareY = TRUE) %>% 
  hide_legend()%>% 
  highlight(on = "plotly_hover")


# Using a Factor as a linking key between two different types of plots. shared_data object created using crosstalk package is used here as well with added argument of key


shared_data2<-SharedData$new(data_for_viz,
                             key=~Factor)


#1. Creating a barplot of variable 3 counts and different Factors


bar_chart<-shared_data2 %>%
  plot_ly() %>%
  group_by(Factor)%>%
  count(var_3) %>%
  add_bars(x = ~Factor, y = ~n) %>%
  layout(barmode = "overlay")


# Creating a scatter plot having different symbols, colors and sizes for different factors


scatter_plot<-shared_data2%>%
  plot_ly(x = ~var_1, 
          y = ~var_3, 
          color = ~Factor, 
          colors = "Set1",
          symbol = ~Factor, 
          symbols = c('circle','x','o'),
          size = 5)


# Linking the two plots


subplot(bar_chart, 
        scatter_plot, 
        titleX = TRUE) %>% 
  hide_legend()
