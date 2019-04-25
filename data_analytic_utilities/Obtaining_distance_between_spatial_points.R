## Obtaining distances between geospatial points
# Distances here are given in km.

#packages used
library(sp)
library(rgdal)
library(tidyverse)

#Spatial points data file

data_file<-paste0(getwd(),"/spatial_points.csv")

#Importing a GIS data of the localities

locality_data<-read.csv(data_file)

#converting the data into a spatial object 

spatial_points<-SpatialPointsDataFrame(coords = locality_data[,c("Longitude","Latitude")],
                                        data = locality_data,
                                        proj4string = CRS("+proj=longlat +datum=WGS84"))

# calculate inter-point distance matrix
distance_data<- sp::spDists(spatial_points,longlat=TRUE)

#Obtaining a long format of the distance matrix
long_distance_matrix<-melt(distance_data)

#A simple visualization of the distance matrix (using a heatmap)
dim <- ncol(distance_data)

#heatmap
image(1:dim, 1:dim, distance_data, axes = FALSE, xlab="", ylab="")

#adding axes with names
axis(1, 1:dim, chislopi_loc$Country, cex.axis = 0.6, las=3)
axis(2, 1:dim, chislopi_loc$Country, cex.axis = 0.6, las=1)

#adding distance data on the plot
text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", distance_data), cex=0.6)

#adding a title
title("Heatmap of distances")
