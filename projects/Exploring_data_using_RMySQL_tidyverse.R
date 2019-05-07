#Exploring (visualizing) the 'world' sql dataset using RMySQL and tidyverse\
# (WIP)

#libraries
library(DBI)
library(RMySQL)
library(dplyr)
library(RColorBrewer)
library(tidyverse)

#Establishing a connection with server (local)

world_db<-dbConnect(MySQL(), 
                    user='root', 
                    password='gambitsam19*%', 
                    dbname='world', 
                    host='localhost')

#obtaining the list of different tables in the dataset
dbListTables(world_db)

#selecting the 'country' table
dbListFields(world_db,'country')

# To obtain a SQL query counterpart of the dplyr code
dplyr::tbl(world_db,"country")%>%
    dplyr::select(Code,
                  Name,
                  Continent,
                  SurfaceArea)%>%
    group_by(Continent)%>%
    summarise(mean_surf_area=mean(SurfaceArea,na.rm=T))%>%
show_query()

# Extracting the 'country' table
country_data<-dbReadTable(world_db,"country")

# Transforming the wide dataformat to a long one

long_format_data<-country_data%>%
    gather(Traits,
           values,
           Population,GNP)%>%
    mutate_if(is.character,
              as.factor)

## Different visualizations of the dataset highlighting certain aspects of the data attributes

#1. Number of countries in each continent
country_data%>%
    dplyr::select(Name,
                  Continent)%>%
    dplyr::filter(Continent!='Antartica')%>%
    droplevels(.)%>%
    na.omit(.)%>%
    mutate_all(as.factor)%>%
    group_by(Continent)%>%
    tally(.)%>%
    arrange(n)%>%
    ggplot(aes(x=Continent,
               y=n))+
    geom_bar(stat="identity",
             fill="orange",
             position = position_dodge(width=0.9))+ 
    theme_bw(base_size = 16)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    ylab("Number of countries")


#2. Visualizing the contrast between GNP and Population trends of the different continents 

long_format_data%>%
    ggplot(aes(x=Continent,
               y=values))+
    geom_bar(stat="identity",
             fill="orange",
             position = position_dodge(width=0.9))+
    geom_jitter(aes(x = Continent), 
                position = position_jitter(width = .25), 
                size=3,
                pch=21,
                alpha = 0.3,
                fill='forestgreen')+
    theme_bw(base_size = 16)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    facet_wrap(~Traits,scales = 'free')


#3. Visualizing the trends in Independence years of countries in all the continents (except Antartica)

country_data%>%
    dplyr::select(Name,
                  Continent,
                  IndepYear)%>%
    dplyr::filter(Continent!='Antartica')%>%
    droplevels(.)%>%
    na.omit(.)%>%
            ggplot(aes(x=IndepYear,
                       fill=Continent)) + 
        geom_histogram()+ 
    theme_bw(base_size = 16)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    scale_fill_brewer(palette="Dark2")+
    facet_wrap(~Continent,
               scales = 'free')

# More data visualizations will be added subsequently..

