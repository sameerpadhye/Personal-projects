# Using Stamen maps with tmap for spatial data

# Libraries used

library(tmap)
library(raster)
library(sp)
library(rgdal)
library(rasterVis)

# The shapefiles can be downloaded from the following link (https://www.diva-gis.org/Data)

# World

world_shape<-readOGR("C:/Users/samee/Downloads/HWSD_RASTER/world_shapefile/ne_50m_admin_0_countries.shp")

proj4string(world_shape)<- CRS("+proj=longlat +datum=WGS84")

# India

india_map<-readOGR("C:/Research_data/GIS_data/GIS_layers/India_DataLayers/IND_adm/INDIA_region.shp")

proj4string(india_map)<- CRS("+proj=longlat +datum=WGS84")

# Additional GIS data points can also be imported for plotting (Here, not used)

# plot maps with conventional 'plot' view (tmap_mode('plot'))

tmap_mode('plot')

#plot

tm_shape(world_shape)+
    tm_borders()+
    tm_fill(col='grey80',
            alpha = 0.2)+
    tm_shape(india_map)+
    tm_borders()+
    tm_style("col_blind")+
    tm_grid(n.x=4,
            n.y=4,
            lwd=0.4,
            alpha = 0.5,
            col='grey70',
            labels.size = 0.8)+
    tm_compass(position = c(.65, .15), 
               color.light = "grey90")+
    tm_scale_bar(position=c("center", "bottom")) 


## Switching to interactive view (for Stamen maps) (for more options please visit http://maps.stamen.com/#watercolor/12/37.7706/-122.3782)

tmap_mode("view")

tm_basemap(leaflet::providers$Stamen.Terrain) + # Stamen Terrain used here
    tm_tiles(paste0("http://services.arcgisonline.com/arcgis/rest/services/Canvas/","World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"), group = "Labels")

# The GIS points can be added with tm_shape and tm_symbols

# Please note that here the shapefiles are not loaded
