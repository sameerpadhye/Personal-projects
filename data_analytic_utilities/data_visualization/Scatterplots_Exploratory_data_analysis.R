## Making simple scatter plots of environmental variables for exploratory data analysis (WIP)

#libraries required
library(tidyverse)
library(purrr)
library(magrittr)
library(gridExtra)
#Importing the data (selecting the necessary environmental variables)
data_for_plot<-read.csv(paste0(getwd(),"/data_for_analysis.csv"))%>%dplyr::select(Altitude,
                  pH:Salinity)# This can be modified as per requirement

#A simple scatterplot for exploring the relationship between two environmental variables Altitude and pH

data_for_plot%>%
    ggplot(aes(x=Altitude,
               y=pH))+
    geom_point(size=5,
               col='black',
               pch=21,
               fill="orange")+
    geom_smooth(method="loess",
                se=FALSE,
                color="black")+
    theme_bw(base_size = 18)+
    ylab("Altitude")+
    xlab("pH")

# Since the there could be any specific combinations we can choose from for our EDA, we need to write a function for the above. This can be done using map() function from purrr package

# Suppose we want to check how pH, salinity and temperature change w.r.t. Altitude, we will keep Altitude as a constant independent variables (i.e. as an X variable)

#we will then create a dependent variable names vector
dep_variables<-names(subset(data_for_plot,
                      select=-c(Altitude)))

# Function using the above plot code for using multiple dependent variables
env_plots<-function(data,xvar,yvar) {
        ggplot(data,aes_string(x=xvar,y=yvar))+
        geom_point(size=5,
                   col='black',
                   pch=21,
                   fill="orange")+
        theme_bw(base_size = 18)

}
# This function is then used to loop over the rest of the dependent variables using the vector 'dep_variables' with Altitude as the independent variable followed (with a pipe) by arranging the plots by grid.arrange from gridExtra package (yvar below is '.x' below)

plots<-map(dep_variables,~env_plots(data_for_plot,"Altitude",.x))%>%
    grid.arrange(grobs = .)


