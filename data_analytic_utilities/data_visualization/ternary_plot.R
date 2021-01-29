# Plotting ternary plots


# libraries used

install.packages('ggtern')
library(ggtern)
library(readxl)
library(tidyverse)

# Importing data

data=read_excel(file.choose(),sheet=1)


View(data)


# Basic ternary plot

names(data)

ggtern(data=data, 
       aes(x=Calcium,
           y=Magnesium, 
           z=Na_K,
           color=RO_Type)) +
    geom_point(size=2,
               alpha=0.7) +
    theme_bw(base_size = 16)+
    scale_color_manual(values = c("forestgreen","orange2","steelblue"))
