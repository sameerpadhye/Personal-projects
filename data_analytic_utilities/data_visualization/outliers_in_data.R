# Exploring the outliers in the data

# For more information please read
# http://uc-r.github.io/gda
# https://www.r-bloggers.com/how-to-remove-outliers-in-r/

#libraries used

library(readxl)
library(tidyverse)


#Path for the data file

data_path<-"C:/Users/samee/Desktop/R data/sample_datasets/data_for_visualizations.xlsx"

#Importing data for analysis

data_for_viz<-read_excel(data_path,
                         sheet=1)%>%
    mutate_if(is.character,
              as.factor)

# Obtaining the outliers

install.packages('outliers')

library(outliers)

outliers::outlier(data_for_viz%>%select(var_1:var_4))


# Visualizing the outliers (here shown for one column of the dataframe. It can be replicated similarly for other columns)

box_plot<-data_for_viz%>%
    ggplot(aes("variable1",var_1))+
    geom_boxplot(fill='grey50',
                 color = "black",
                 outlier.alpha = .5)+
    theme_bw()+
    ylab('values')
# The points on the either side of whiskers in the plot are potential outliers

dot_plot<-data_for_viz%>%
    ggplot(aes(x=var_1))+
    geom_dotplot(method = "histodot", 
                 binwidth = 5)+
    geom_vline(xintercept = c(50,60), 
               color = "red",   # the value range is selected based on the outlier value/s of variable 1 obtained above
               lty = "dashed") +
    theme_bw()+
    xlab("variable1 value")+
    ylab("frequency")

require(gridExtra)

gridExtra::grid.arrange(box_plot,
                        dot_plot,
                        nrow=1)
