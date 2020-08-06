# Importing and using geotiff files 

library(raster)
library(rasterVis)
library(rgdal)
library(tidyverse)


# Importing data file

raster_tiff<-raster("C:/Users/samee/Downloads/Decadal_LULC_India_1336/Decadal_LULC_India_1336/data/LULC_2005.tif")

# Defining the projection for the geotiff file

proj4string(raster_tiff)<- CRS("+proj=longlat +datum=WGS84")

raster_tiff@crs

# Plot the geotiff by itself

plotRGB(raster_tiff,
        scale=255)

# base plot with single color points 
 
points(bryospilus_points,
     pch = 20,
     legend = levels(bryospilus_points$Species),
     col = "black",
     cex = 1.2)

# Defining custom color vector

col_vec<-c("steelblue","black","orange","brown")

# Plot using the custom colors

plotRGB(raster_tiff,
        r=1, g=2, b=3,
        interpolate=TRUE)

# points

points(bryospilus_points,
       pch = 21,
       col = "black",
       cex = 1.5,
       bg=col_vec, 
       lwd=1.2)

# text

text(bryospilus_points$Longitude,
     bryospilus_points$Latitude,
     labels=bryospilus_points$Species,
     pos=4, 
     offset=0.8)

# Adding legend

legend("bottomright",
    legend = unique(bryospilus_data$Species),
    col = col_vec,
    pch = 19, 
    cex = 0.8)

