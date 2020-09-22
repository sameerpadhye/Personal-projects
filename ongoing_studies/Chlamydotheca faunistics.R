# Data summary, visualization and GIS mapping of Chlamydotheca species from India


#libraries used


library(vegan)
library(tidyverse)
library(readxl)
library(magrittr)
library(leaflet.providers)


# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)


data_path<-"C:/Users/samee/Downloads/Chlamydotheca/chlamydotheca.xlsx"


# Chlamydotheca data

chlamydo<-read_excel(data_path,
                            sheet=2)

head(chlamydo,3)


# Data summary of the dataset


library(psych)


data_summ_chlamydo<-psych::describe(chlamydo%>%select_if(is.numeric),
                                    na.rm = T)


View(data_summ_chlamydo)


# Converting from wide to long form 

chlamydo_long<-chlamydo%>%
    dplyr::select(pH,
                  temp,
                  sal,
                  total_sp_community)%>%
    dplyr::rename('Temperature'='temp',
                  'Salinity'='sal',
                  'Total_species'='total_sp_community')%>%
    tidyr::gather(env_var,values, 
                  pH:Temperature,
                  Salinity,
                  Total_species)

# plot for env var

chlamydo_long%>%
    ggplot(aes(x=env_var,
               y=values))+
    geom_boxplot(col='black',
                 fill='forestgreen',
                 lwd=1)+
        theme_bw(base_size = 19)+
    facet_wrap(~env_var,
               scales = 'free')+
    labs(x="Environmental variables",
         y="Value")


# Local distribution of Chlamydotheca using leaflet

require(leaflet)

# getting the data from the main dataset

locality_data<-chlamydo%>%
    dplyr::select(long,
                  lat,
                  Locality,
                  type,
                  Alt,
                  veg,
                  temp,
                  cond,
                  sal)

# Color palette for the map

pal <- colorFactor(
    palette = c('steelblue', 
                'forestgreen', 
                'black',
                'purple', 
                'orange',
                'grey60'),
    domain = locality_data$Locality)

# Map

chlamydo_map<-leaflet(locality_data) %>%
     addProviderTiles("Stamen.Terrain") %>%
    addCircleMarkers(
        color = ~pal(Locality),
        opacity = 1,
        stroke = TRUE,
        lng = ~long, 
        lat = ~lat,
        label = ~as.character(Locality),
        radius = 4)
    
# Adding scale bar

addScaleBar(chlamydo_map,
            position = 'topright')%>%
    addProviderTiles(providers$Esri.WorldStreetMap) %>%
    addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE)
