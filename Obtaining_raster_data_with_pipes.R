##Obtaining raster data from a list of raster objects using pipes

#Code sequence (in the pipe)
#1. to get raster file names
#2. using the  map function in purrr package to get raster objects of all the listed files
#3. extracting whatever data is present in the raster files of the spatial points (provided as a Spatialdataframe object)
#4. converting into a dataframe
#5. renaming the columns


raster_data<-list.files(list.files(paste0(getwd())), 
                         full.names = T)%>%#1
    purrr::map(raster)%>% #2
    purrr::map(.,~raster::extract(.,SpatialPoints_df_object))%>% #3
    bind_cols(.)%>% #4
    dplyr::rename(name_1=V1, #5
                  name_2=V2,
                  name_3=V3,
                  name_4=V4,
                  name_5=V5) # this number will vary as per the number of raster files imported
   
    
        

