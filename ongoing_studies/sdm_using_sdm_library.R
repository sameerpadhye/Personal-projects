library(tidyverse)
library(dismo)
library(mapview)

species<-gbif("Streptocephalus",
              "dichotomus",
              download=T,
              geo=T,
              sp=F)

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

species_data<-species%>%
    dplyr::select(lon,lat)%>%
    drop_na(.)%>%
    dplyr::mutate(species=1)

coordinates(species_data)<- ~lon + lat

class(species_data)

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

library(usdm) # for checking collinearity

v1<-vifstep(masked_file) # can put a dataframe of the raster data as well

v2<-vifcor(masked_file,th=0.8)

v1

v2

biom<-exclude(masked_file,v2)

class(biom)

plot(biom[[1]])

proj4string(species_data)<-projection(raster())

mapview(species_data)

library(sdm)
#installAll()

d<-sdmData(species~.,species_data,
           predictors = biom, 
           bg= list(n=1000,
                    method = 'gRandom'))

d

getmethodNames()

d2 <- sdm(species~.,d,methods=c('glm','rpart'),
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
