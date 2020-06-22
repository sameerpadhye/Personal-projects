## Cropping the shapefiles and masking grid files based on the cropped files


# library

require(raster)
require(sp)
require(rgdal)
library(tidyverse)


# Importing the Western Ghats shapefile
 
Western_Ghats<-readOGR("C:/Research_data/GIS_data/GIS_layers/WG new outline shapefiles/Western GHats outline.shp")

proj4string(Western_Ghats)<- CRS("+proj=longlat +datum=WGS84")


# Importing the Altitude grid

india_alt<-raster("C:/Research_data/GIS_data/GIS_layers/worldclim data/Zone 28/altitude/alt_28.gri")

proj4string(india_alt)<-CRS("+proj=longlat +datum=WGS84")


# Cropping the altitude grid based on the extent of Western Ghats (latitude and longitude)

elevation.wg <-crop(x = india_alt, 
                    y = as(Western_Ghats,
                           "Spatial"))

# Visualize the cropped layer

plot(elevation.wg)

# Masking the cropped layer based on the Western Ghats shapefile

elevation.wg2<-elevation.wg%>%
    mask(Western_Ghats)
 
# Plot

plot(elevation.wg2)
    
