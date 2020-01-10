# Chloropleth maps using custom information 

# libraries used

require(tidyverse)
require(sp)
require(rgdal)

# data path

data_path<-"C:/Data/GIS_data/India_states_latest/Admin2.shp"

india_state_map<-readOGR(data_path)
proj4string(india_state_map)<- CRS("+proj=longlat +datum=WGS84")

# Importing the pollution data

data_file_path<-paste0(getwd(),"/India_states_pollution_summary.csv")

state_pollution_data<-read.csv()

#Exploring the data

head(state_pollution_data,5)

#Merging the pollution data with sp object 

india_merged_data<-sp::merge(india_state_map,
                             state_pollution_data, 
                             by.x="ST_NM",
                             by.y="state")

# Observing the merged data

head(india_merged_data@data,5)

# Defining the label which can be seen on the map

state_popup <- paste(
    "State: ", india_merged_data@data$ST_NM,"<br/>", 
    "mean_NO2: ", india_merged_data@data$so2)%>%
    lapply(htmltools::HTML)

# Geospatial map using leaflet package

require (leaflet)

leaflet(data=india_merged_data) %>% 
    addTiles()  %>% 
    addPolygons( 
        label = ~state_popup,
        fillColor = ~colorBin("YlOrRd",so2)(so2),
        stroke=T,
        color = "black",
        weight = 0.3,
        fillOpacity = 0.9)%>%
    addLegend(pal=colorBin("YlOrRd",domain = india_merged_data@data$so2),
              values=~so2, 
              opacity=0.8, 
              title = "SO2 values", 
              position = "bottomleft" )
