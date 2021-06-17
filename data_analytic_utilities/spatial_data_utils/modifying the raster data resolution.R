library(raster)
library(rgdal)
library(tidyverse)

altitude<-raster("C:/Users/samee/Downloads/new_GIS_data/altitude/ETOPO1_Ice_g_geotiff.tif")

proj4string(altitude)<- CRS("+proj=longlat +datum=WGS84")

maha_map<-readOGR(file.choose())

plot(maha_map)

maha_map@bbox

proj4string(maha_map)<- CRS("+proj=longlat +datum=WGS84")

# Crop the geotiff (cutting the raster file based on the extent of the shapefile)

cropped_file <-crop(altitude, 
                    maha_map)

plot(cropped_file)

# Mask the cropped file (this will specifically select the region of the raster bound by the shapefile)

masked_file<-mask(cropped_file,
                  maha_map)

plot(masked_file,
     col=terrain.colors(10), 
     alpha=0.6,
     legend=TRUE,  
     main="Raster map",
     axes=TRUE) 

# Aggregating the raster info from a lower resolution to higher resolution
# The original dataset is made from the original 1x1 km to 5x5 km

altitude_agg <- aggregate(masked_file, 
                          fact=5)

plot(altitude_agg)
