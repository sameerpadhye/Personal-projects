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


# nMDS plot

# to obtain the number of observations per Groups factor

table(data_analysis$Groups)['A']

table(data_analysis$Groups)['B']

table(data_analysis$Groups)['C']


# NMDS plot

#1. blank plot

ordiplot(data_nmds,
         type="n")

#2. species data

orditorp(data_nmds,
         display="species",
         col="red",
         air=0.01)

#3. sites data

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

#4. convex hulls

ordihull(data_nmds,
         groups=data_analysis$Groups,
         draw="polygon",
         col="grey50",
         label=F)


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


