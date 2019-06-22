# Species richness and diversity analyses

#libraries used

library(vegan)
library(tidyverse)
library(readxl)
library(magrittr)

# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-paste0(getwd(),"/species_richness_data.xlsx")


#Importing data for analysis (Species abundance dataset used here)

data_analysis<-read_excel(data_path,
                          sheet=1)


#Exploring the data

head(data_analysis,5)


#Obtaining the total number of species per site

sp_number<-vegan::specnumber(data_analysis[,-1])

sp_number


#Generating species accumulation curves using vegan package

species_acc<-specaccum(data_analysis%>%
                           select_if(is.numeric),
                       method = "random",
                       permutations = 999,
                       conditioned = TRUE,
                       gamma = "jack1")


#plot of accumulation curves using base R

plot (species_acc,
      col = 'blue',
      ci=2,
      main="Species accumulation curve using Jackknife1",
      xlab='Sites',
      ylab='Species')


#plotting the accumulation plots using ggplot2

#Extracting the data from the specaccum object generated using vegan package (use_series from magrittr specifically selects the essential data from the object using pipes)

sp_accu_sites<-species_acc%>%
    magrittr::use_series(sites) 

sp_accu_richness<-species_acc%>%
    magrittr::use_series(richness)

sp_accu_sd<-species_acc%>%
    magrittr::use_series(sd)

sp_richess_data<-cbind(sites=sp_accu_sites,
                       richness=sp_accu_richness,
                       std_dev=sp_accu_sd)%>%
    as.data.frame()

#plot (Additional arguments can be passed as per user requirements)

sp_richess_data%>% 
    ggplot(aes(x=sites,
               y=richness))+
    geom_point(color='black',
               fill='steelblue',
               pch=21)+
    geom_line(color = 'steelblue',
              lwd=1.2)+
    theme_bw(base_size = 16)+
    xlab('Sites')+
    ylab ('Species richness (Jack1)')+
    labs(title='Species accumulation curve')+
    theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
    geom_errorbar(aes(ymin=richness-std_dev,
                      ymax=richness+std_dev),
                  color = "steelblue",
                  alpha=0.3)


# Summarizing individual species diversity indices

#Shannon diversity of the sites

shannon_div<-vegan::diversity(data_analysis%>%
                                  select_if(is.numeric),'shannon')

shannon_div

#Shannon diversity of the sites

simpson_div<-vegan::diversity(data_analysis%>%
                                  select_if(is.numeric),'simpson')

simpson_div

#Evenness index

evenness_div<-shannon_div/sp_number

evenness_div
