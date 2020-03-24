# Using geom_ribbon and qplots

# Libraries used

require(readxl)
require(tidyverse)


#Path for the data file (data_for_visualization.xlsx provided in the data can also be used for practice)

data_path<-paste0(getwd(),"/data_for_visualization.xlsx")

#Importing data for analysis

data_for_viz<-read_excel(data_path,
                         sheet=1)%>%
    mutate_if(is.character,
              as.factor)

#exploring the structure of the data

str(data_for_viz)

# Easy plotting using qplots 

# Scatter

qplot(var_1, # variable 1
        var_2,  # variable 2
        data = data_for_viz, # data 
        col = Factor,  # color variable
        geom = "point") # type of plot

# Barchart

qplot(
    Factor,
    data = data_for_viz,
    geom = "bar")


# Using geom_ribbon

data_for_viz %>% 
    ggplot(aes(var_1, 
               var_2)) + 
    geom_ribbon(aes(ymin = var_2 - 10, # Thickeness of ribbon (upper limit of the Y axis)
                    ymax = var_2 + 10), # Thickeness of ribbon (lower limit of the Y axis)
                fill = "forestgreen",
                alpha=0.4)+
    theme_bw(base_size = 18)

# Using the geom_ribbon with line plot

# Using geom_ribbon with line plot

data_for_viz %>% 
    ggplot(aes(var_1, 
               var_2)) + 
    geom_ribbon(aes(ymin = var_2 - 10, # Thickeness of ribbon (upper limit of the Y axis)
                    ymax = var_2 + 10), # Thickeness of ribbon (lower limit of the Y axis)
                fill = "forestgreen",
                alpha=0.4)+
    theme_bw(base_size = 18)+
   geom_line(color = "orangered",  # the line plot
             size = 1)
