# Using ggplot for plotting GIS data

#Libraries used

library(raster)
library(ggmap)
library(tidyverse)
library(sp)
library(rgdal)
library(rasterVis)
library(ggpubr)

## Data import (Maps used from local machine and would need to be downloaded from the web)

# world map

world_shape<-readOGR("C:/Users/samee/Downloads/HWSD_RASTER/world_shapefile/ne_50m_admin_0_countries.shp")

proj4string(world_shape)<- CRS("+proj=longlat +datum=WGS84")

# India outline map

india_1<-readOGR("C:/Research_data/GIS_data/GIS_layers/India_DataLayers/IND_adm/INDIA_region.shp")

proj4string(india_1)<- CRS("+proj=longlat +datum=WGS84")


# Sample GIS points

data_path<-"C:/Users/samee/Desktop/R data/sample_datasets/sample_GIS_data.csv"

sample_GIS<-read.csv(data_path)


## Plot using ggplot

main_map<-sample_GIS%>%
    ggplot(aes(Longitude,
               Latitude))+
    geom_polygon(data=india_1,
                 aes(x=long, 
                     y = lat,
                     group=group),
                 fill='grey50',
                 color='black',
                 alpha=0.5)+
    coord_quickmap()+
    theme_bw(base_size = 16)+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())+
    geom_point(pch=21,
               fill='orange',
               color = "black",
               size = 4)

main_map

#overview map

overview_map<-ggplot(world_shape,
                     aes(Longitude,
                         Latitude))+
    geom_polygon(data=world_shape,
                 aes(x=long, 
                     y = lat,
                     group=group),
                 fill='grey40',
                 color='black') +
    geom_polygon(data=india_1,
                 aes(x=long, 
                     y = lat,
                     group=group),
                 fill='forestgreen',
                 color='black')+
    coord_quickmap()+
    theme_bw(base_size = 15)+
    theme( axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank())


overview_map

##plate of both maps

ggarrange(main_map,
          overview_map, 
          font.label = list(size = 14, 
                            color = "black", 
                            face = "bold", 
                            family = NULL), 
          ncol = 2, 
          nrow = 1,
          widths=c(5,3),
          align = 'hv')

# WIP
