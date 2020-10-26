# Creating interactive heatmaps using heatmaply

#https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html#introduction

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

#exploring the structure of the data

str(data_for_viz)


#install.packages('heatmaply')

library(heatmaply)
data("mtcars")
View(mtcars)
heatmaply(data_for_viz, k_row = 3, k_col = 2)
?heatmaply
