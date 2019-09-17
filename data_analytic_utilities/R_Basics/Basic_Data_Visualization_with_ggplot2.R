###############################################################
## Data Visualization with ggplot2
##
## 
## Sameer M. Padhye
##
## 
###############################################################


# The functionality of ggplot2 is extensive and showing each and every aspect is beyond the scope of this exercise. This exercise is focused on obtaining basic plots

library(tidyverse)

#### Histogram and Density plot

#Data

hist_data <- data.frame(
    habitat=factor(rep(c("A","B","C"), each=300)),
    sp_count=round(c(rnorm(300, mean=40, sd=4), 
                     rnorm(300, mean=45, sd=4.5),
                     rnorm(300, mean=52, sd=5.5)))
)

#View the data

head(hist_data)


#Plot


#1. Simple plot

ggplot(hist_data,
       aes(x=sp_count))+   # aes denotes the aesthetic argument where the x and y variables are assigned
    geom_histogram(color='black',  # geom denotes the type of plot which is histogram here along with the arugment color (outline) and fill (color of each bing)
                   fill='grey')+
    ggtitle("Histogram")+ # title of the plot
    theme_bw() + # white background
    xlab("Species_count")+ # x axis label
    ylab ("Count")  # y label

#2. Coloring based on the type of habitat

ggplot(hist_data,
       aes(x=sp_count,
           fill=habitat))+
    geom_histogram(alpha=0.7)+
    ggtitle("Histogram")+
    theme_bw()

#3. Density plot of the above plot

ggplot(hist_data,
       aes(x=sp_count,
           fill=habitat))+
    geom_density(alpha=0.7)+
    ggtitle("Histogram")+
    theme_bw()




#### Barchart

# Data

barchart_data <- data.frame(population=c("pop_A", "pop_B", "pop_C"),
                            avg_length=c(15.5,17.5, 19.5))


#View the data

head(barchart_data)


#Plot


#1. Basic plot

bar_chart<-ggplot(barchart_data,
                  aes(x=population,
                      y=avg_length))+ 
    geom_bar(stat="identity",
             color="black",
             fill="forestgreen")+
    ggtitle("Barchart")+
    theme_bw() +
    xlab("Population")+ 
    ylab ("Average Length (mm)")

bar_chart

# Flipping the co-ordinates

bar_chart+coord_flip()


#2. Barchart with colors based on categories (here different populations)

bar_chart_2<-ggplot(barchart_data,
                    aes(x=population,
                        y=avg_length,
                        fill=population))+ # remember to add the category as 'fill' in the aes argument
    geom_bar(stat="identity")+
    ggtitle("Barchart")+
    theme_bw() +
    xlab("Population")+ 
    ylab ("Average Length (mm)") 

bar_chart_2

# Changing the barchart color (custom colors)

bar_chart_2+
    scale_fill_manual(values=c("forestgreen", 
                               "steelblue", 
                               "orange"))


# The important thing to remember is that the color of the bar is changed by changing the 'fill' argument and NOT the 'color' argument.

#3. Stacked barchart

ggplot(barchart_data,
       aes(x="",
           y=avg_length,
           fill=population))+ 
    geom_bar(stat="identity",
             width = 1)+
    ggtitle("Barchart")+
    theme_bw() +
    xlab("Population")+ 
    ylab ("Average Length (mm)")

# Adding text information on the barcharts

bar_chart+
    geom_text(aes(label=avg_length), 
              vjust=-0.3, 
              size=3.5)




#### Piechart

# Data used for barchart will be used here

ggplot(barchart_data,
       aes(x="",
           y=avg_length, # the numerical variable is added as the y variable
           fill=population))+ # the category is added in the fill section
    geom_bar(stat="identity",
             width = 1) +
    coord_polar("y", start=0)+
    ggtitle("Piechart")+
    theme_bw()




#### Scatterplot and Line chart

# Correlation and Regression data will be used here

correlation_data<-data.frame(
    var_1=c(10,12,17,29,35,34,56,89,112,156),
    var_2=seq(160,1,length.out = 10), 
    var_3=runif(10),  
    var_4=sample(c(1:200),10,replace = T)) 


# Scatterplot

ggplot(correlation_data,aes(x=var_1,
                            y=var_2))+
    geom_point(size=5, # size of the points
               color='steelblue')+  # Here color determines the color of points by default
    ggtitle("Scatterplot")+
    xlab("Variable 1")+
    ylab("Variable 2")+
    theme_bw()


# Line chart

ggplot(correlation_data,aes(x=var_3,
                            y=var_4))+
    geom_line(size=1, # size of the points
               color='black')+  # Here color determines the color of points by default
    ggtitle("Linechart")+
    xlab("Variable 3")+
    ylab("Variable 4")+
    theme_bw()




#### Box plots

# Here, we will use the histogram dataset (hist_data)


#1. Basic plot

ggplot(hist_data,aes(x=habitat,
                     y=sp_count))+
    geom_boxplot(fill='grey')+
    ggtitle("Boxplot")+
    xlab("Habitats")+
    ylab("Species count")+
    theme_bw()


#2. Colors based on type of habitats

boxplot_col<-ggplot(hist_data,aes(x=habitat,
                     y=sp_count,
                     fill=habitat))+
    geom_boxplot()+
    ggtitle("Boxplot")+
    xlab("Habitats")+
    ylab("Species count")+
    theme_bw()+
    scale_fill_manual(values = c("orange","steelblue","forestgreen")) # custom colors 


#Flipping the axix

boxplot_col+coord_flip()
