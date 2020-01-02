## Creating Treemaps 

# Common libaries used

require (tidyverse)
require(readxl)

#Path for the data file


data_path<-"C:/Users/samee/Desktop/R data/sample_datasets/data_for_visualizations.xlsx"

#Importing data for analysis


data_for_viz<-read_excel(data_path,
                         sheet=1)%>%
    mutate_if(is.character,
              as.factor)

# Creating treemaps using treemap package

#install.packages('treemap')

library(treemap)

# Plot

treemap_data<-treemap(data_for_viz, 
        index=c("Factor"),  
        vSize = "var_1",  
        type="index", 
        palette = "Greens",  
        title="Variable 3 across all the Factors", 
        fontsize.title = 14)

treemap_data

# Making an interactive version of the treemap using d3treeR package


install.packages("remotes")
remotes::install_github("d3treeR/d3treeR")
library(d3treeR)


d3tree2(treemap(data_for_viz, 
                index=c("Factor"),  
                vSize = "var_1",  
                type="index", 
                palette = "Greens",  
                title="Variable 3 across all the Factors", 
                fontsize.title = 14),
        rootname = "Variable 3 across all the Factors")
