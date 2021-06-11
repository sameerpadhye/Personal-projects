library(tidyverse)
library(dismo)
library(mapview)

species<-gbif("Streptocephalus",
              "dichotomus",
              download=T,
              geo=T,
              sp=F)%>%
    dplyr::select(lon,lat)%>%
    drop_na(.)


# w<-which(is.na(species$lon))
# 
# w
# 
# species<-species[-w,]
# 
# species$lon
# 
# w2<-which(is.na(species$lat))
# 
# w2
# 
# species<-species[-w2,]
# 
# species$lat
# 
# species$sp<-1
# 
# species<-species[,c("lon","lat","sp")]

# sampling_points<-SpatialPointsDataFrame(coords = GIS_samples[,c("Longitude","Latitude")],data = GIS_samples,proj4string = CRS("+proj=longlat +datum=WGS84"))

# %>%
#     dplyr::mutate(species=1)

# Importing the downloaded data

files <- list.files(path="C:/Users/samee/Desktop/R data/bioclim/wc5",pattern='bil',  full.names=TRUE)

bioclim_data<-stack(files)

india_state_map<-rgdal::readOGR(file.choose())

proj4string(india_state_map)<- CRS("+proj=longlat +datum=WGS84")

# Crop the files (cutting the raster file based on the extent of the shapefile)

cropped_file <-crop(bioclim_data, 
                    india_state_map)

plot(cropped_file)

# Mask the cropped file (this will specifically select the region of the raster bound by the shapefile)

masked_file<-mask(cropped_file,
                  india_state_map)

plot(masked_file$bio1)

# Pseudo-absences

library(dismo)

# backgrd_pt<-randomPoints(masked_file,100)

# backgrd_pt

max.lat <- ceiling(max(species$lat))

min.lat <- floor(min(species$lat))

max.lon <- ceiling(max(species$lon))

min.lon <- floor(min(species$lon))

geographic.extent <- extent(x = c(min.lon, 
                                  max.lon, 
                                  min.lat, 
                                  max.lat))

bg2 <- randomPoints(masked_file, 
                    200, 
                    ext=geographic.extent)%>%
    data.frame(.)%>%
    rename('lon'='x',
           'lat'='y')

plot(masked_file[[1]])

points(bg2, cex=0.5)

pres_abs_vec<-c(rep(1,nrow(species)),
                rep(0,nrow(bg2)))

species_data<-rbind(species,bg2)%>%
    data.frame(.)%>%
    mutate(sp_pre_abs=pres_abs_vec)

View(species_data)

# # For getting a circular area around the sampled points
# 
# x <- circles(species_data, 
#              d=50000, 
#             lonlat=TRUE)
# 
# x_polygon<-polygons(x)
# 
# samp1 <- spsample(x_polygon, 100, type='random')
# 
# class(samp1)
# 
# plot(samp1)
# 
# cells <- cellFromXY(masked_file, 
#                     samp1)%>%
#     unique(.)
# 
# length(cells)
# 
# xy <- xyFromCell(masked_file, cells)
# 
# class(xy)
# 
# xy
# 
# plot(x_polygon)
# 
# points(xy,pch=20)
# 
# View(species_data)


library(usdm) # for checking collinearity

v1<-vifstep(masked_file) # can put a dataframe of the raster data as well

v2<-vifcor(masked_file,th=0.8)

v1

v2

biom<-exclude(masked_file,v2)

class(biom)

plot(biom[[1]])

coordinates(species_data) <- ~lon + lat

head(species_data)

proj4string(species_data)<-projection(raster())

mapview(species_data)

library(sdm)

#installAll()

d<-sdmData(sp_pre_abs~.,species_data,
           predictors = biom, 
           bg= list(n=1000,
                    method = 'gRandom'))

d

getmethodNames()

d2 <- sdm(sp_pre_abs~.,d,methods=c('glm','rpart'),
           replication = c('boot'),n=4)

gui(d2)

p<- predict(d2,biom,'pred.img')

plot(p)

en<- ensemble(d2,
              newdata = biom,
              'preds.img',
              setting=list(method='weighted',
                           stat='AUC'))

plot(en)

mapview(en)


### TO CHECK

# Make SpatialPointsDataFrame:
pop_imag <- sp[sp$sp=='Populus_imagines',1:2]
coordinates(pop_imag) <- ~x+y
projection(pop_imag) <- CRS('+proj=longlat +datum=WGS84')

# Then, place a buffer of 200 km radius around our presence points
x <- buffer(pop_imag,width=200000)

# Set all raster cells outside the buffer to NA
x <- mask(mask,x)

# Randomly select background data within the buffer
bg_rand4 <- randomPoints(x, 500)

# Plot the map and data
plot(mask,col='grey',legend=F)
plot(x, legend=F, add=T)
points(sp[sp$sp=='Populus_imagines',1:2],pch='+',col='red')
points(bg_rand4,pch=19,cex=0.3)

## CHECK: https://damariszurell.github.io/EEC-SDM/5_pseudoabsence.html#3_BackgroundPseudo-absence_data_selection
