#install.packages('dismo')
library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")
library(tmap)
library(rgeos)
library(raster)
# Bioclim data

bioclim.data <- getData(name = "worldclim",
                        var = "bio",
                        res = 5,
                        path = "C:/Users/samee/Desktop/R data/bioclim")

plot(bioclim.data$bio1)

# Shapefile

india_state_map

india_state_map<-rgdal::readOGR(file.choose())

proj4string(india_state_map)<- CRS("+proj=longlat +datum=WGS84")

# Crop the files (cutting the raster file based on the extent of the shapefile)

cropped_file <-crop(bioclim.data, 
                    india_state_map)

plot(cropped_file)

# Mask the cropped file (this will specifically select the region of the raster bound by the shapefile)

masked_file<-mask(cropped_file,
                  india_state_map)

plot(masked_file$bio1)


# GIS data points

GIS_samples<-read.csv(file.choose())

GIS_samples

coordinates<-GIS_samples[,c("Longitude","Latitude")]

sampling_points<-SpatialPointsDataFrame(coords = GIS_samples[,c("Longitude","Latitude")],data = GIS_samples,proj4string = CRS("+proj=longlat +datum=WGS84"))

sampling_points # Spatialpointsdf

plot(india_state_map, 
     axes=TRUE, 
     col="light yellow")

points(GIS_samples$Longitude, 
       GIS_samples$Latitude,
       col='black', pch=20, cex=0.75)

# Combining the information from the shapefile for the GIS points

ovr <- over(sampling_points, india_state_map)

ovr

set.seed(1963)
bg <- randomPoints(masked_file, 100)

plot(bg)

# Determine geographic extent of our data

max.lat <- ceiling(max(obs.data$latitude))
min.lat <- floor(min(obs.data$latitude))
max.lon <- ceiling(max(obs.data$longitude))
min.lon <- floor(min(obs.data$longitude))

x <- circles(sampling_points, d=50000, lonlat=TRUE)
## Loading required namespace: rgeos
pol <- polygons(x)
plot(pol, axes=TRUE)

# sample randomly from all circles
samp1 <- spsample(pol, 250, type='random', iter=25)
# get unique cells
cells <- cellFromXY(masked_file, samp1)
length(cells)
## [1] 250
cells <- unique(cells)
length(cells)
## [1] 161
xy <- xyFromCell(masked_file, cells)

plot(pol, axes=TRUE)
points(xy, cex=0.75, pch=20, col='blue')
