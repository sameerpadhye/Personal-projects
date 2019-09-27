## Basic Non metric dimensional scaling (nMDS) analysis

#libraries used

library(vegan)
library(tidyverse)
library(readxl)
library(magrittr)


# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-paste0(getwd(),"/nMDS_data.xlsx")


#Importing data for analysis (nMDS dataset used here)

data_analysis<-read_excel(data_path,
                          sheet=1)%>%
    mutate_at('Groups',
              as.factor)


#Exploring the data

head(data_analysis,5)


#nMDS is performed using vegan package 

data_nmds<-vegan::metaMDS(data_analysis%>%
                              dplyr::select_if(is.numeric), # selecting the numeric data
                          distance = 'bray', #bray is used here since the data are abundance data 
                          k=2, # number of dimensions
                          trymax = 199) # number of random starts


#Stressplot of the nMDS

stressplot(data_nmds)

#goodness of fit for each of the locality points

goodness(data_nmds)


# nMDS plot

# Defining colors and symbols (vectors) based on the groups in the dataset for better visualization

# Finding the levels

levels(data_analysis$Groups)

# Defining the colors based on the levels

color_vector<-c("forestgreen","steelblue","grey20")

# Defining the symbols based on the levels

symbol_vectors<-c(21,21,22)


# NMDS plot

#1. blank plot

ordiplot(data_nmds,
         type="n")

#2. sites data

# Plotting the sites data using points function and adding color and symbols based on the groups

points(data_nmds,
       display = "sites",
       col = "black",
       pch = symbol_vectors[data_analysis$Groups],
       bg = color_vector[data_analysis$Groups],
       cex=1.4)


# Plotting the sites data using orditorp function and adding color and text based on the groups

orditorp(data_nmds,
         display="sites",
         cex=1,
         air=0.5,
         col = color_vector[data_analysis$Groups]
)

#3. convex hulls

#Ordihull type 1

# ordihull(data_nmds,
#          groups=data_analysis$Groups,
#          draw="polygon",
#          col=color_vector,
#          label=T)

#Ordihull type 2 (adapted from http://rpubs.com/CPEL/NMDS)

ordihull(
    data_nmds,
    groups=data_analysis$Groups,
    display = "sites",
    draw = c("polygon"),
    col = NULL,
    border = color_vector,
    lty = c(1, 2, 3),
    lwd = 2.5,
    label = TRUE
)

#Adding title

title("Non metric dimensional sclaing")

#4. sites data

orditorp(data_nmds,
         display="sites",
         cex=0.9,
         air=0.01,
         col=c(rep("steelblue",
                   table(data_analysis$Groups)['A']),
               rep("forestgreen",
                   table(data_analysis$Groups)['B']),
               rep("orange",
                   table(data_analysis$Groups)['C'])))


# Visualization using ggplot2 (adapted from https://chrischizinski.github.io/rstats/vegan-ggplot2/)

#Species scores 

sp_scores<-vegan::scores(data_nmds,"species")%>%
    data.frame(.)%>%
    rownames_to_column(.)%>%
    dplyr::rename("species"="rowname")

#View result

View(sp_scores)

#Site scores

site_scores<-vegan::scores(data_nmds)%>%
    data.frame(.)%>%
    rownames_to_column(.)%>%
    dplyr::rename("sites"="rowname")%>%
    dplyr::mutate(site_groups=data_analysis$Groups)

# View result

View(site_scores)


# Plot

ggplot() + 
    geom_point(data=site_scores,aes(x=NMDS1,
                                    y=NMDS2,
                                    shape=site_groups,
                                    colour=site_groups),
               size=5) + 
    geom_text(data=sp_scores,
              aes(x=NMDS1,
                  y=NMDS2,
                  label=species),
              vjust=0.7,
              hjust=0.5) + 
    scale_colour_manual(values=c("A" = "orange", "B" = "steelblue", "C" = "forestgreen")) +
    coord_equal() +
    theme_bw(base_size = 18)+
    ggtitle("nMDS plot")


