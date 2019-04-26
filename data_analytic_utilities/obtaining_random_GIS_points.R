#generating random spatial points in and around the available locality data within a grid of 100k sq m.

#Grid data has been downloaded from the following link (http://earth-info.nga.mil/GandG/coordsys/gislayers/gislayers.html)

#Many times, random spatial data points are needed in spatial data analysis to avoid a sampling bias in analyses

#One way to generate such random data points is to use reference grids where each cell has a definite area. This avoids biases based on geographic sizes (such as comparing Singapore and India)

#GIS data of the localities (Data here would be the Latitude, Longitude of the localities and other data each in a separate column)

#packages used
library(tidyverse)
library(sp)
library(raster)

#data file 
data_point_file<- paste0(getwd(),"/gis_data.csv")

#importing the file
data_points<-read.csv(data_point_file)%>%
    dplyr::select(Longitude,
                  Latitude,
                  everything())

#converting the data into a spatial object 

gis_points<-SpatialPointsDataFrame(coords = data_points[,c("Longitude","Latitude")],
                                         data = data_points,
                                         proj4string = CRS("+proj=longlat +datum=WGS84")) # projection can be changed as per requirement, though, should be consistent for the grid as well as the gis_point file


# Importing grid shapefile (100k sq.km)paste0(getwd(),"/mgrs_region.shp.shp")

grid_file<-"C:/Users/samee/Downloads/HWSD_RASTER/world_mgrs/mgrs_region.shp"

grids_100k<-readOGR(grid_file)

proj4string(grids_100k)<- CRS("+proj=longlat +datum=WGS84")

#extracting the grids specific to the localities
grids_for_points<-raster::intersect(grids_100k,gis_points)

#To obtain the name and number of grids for reference

name_no_grids<-data.frame(grid_name=grids_for_points@data[,c("GRID1MIL")],grid_no=seq_along(grids_for_points@data$GRID1MIL))

#Using the function to get data for the specific grid
random_spatial_pts<-function(x,grid_no,sample_no){
    
    points=x[grid_no,]%>%
        sp::spsample(.,n=sample_no,"random")%>% ## random sample generation
        .@coords%>%  ## extracting the co-ordinate data
        as.data.frame(.)%>%
        cbind(grid_no=x$GRID1MIL[[grid_no]]%>% 
                  rep(.,times=sample_no))%>% ## to get the grid number for reference
        droplevels(.)
    
    return(points)
    
}

random_spatial_pts(grids_for_points,1,5)

