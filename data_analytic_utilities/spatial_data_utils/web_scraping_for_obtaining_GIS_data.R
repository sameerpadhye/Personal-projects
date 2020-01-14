# Web scraping for obtaining GIS dataset of Indian states

# Code adopted from http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html

#Common libraries used

library(tidyverse)

# Webpage is scraped using the rvest package

#install.packages('rvest')

library(rvest)

# URL (with Indian states GIS data)

webpage <- read_html("http://www.quickgs.com/latitudinal-and-longitudinal-extents-of-india-indian-states-and-cities/")

webpage

#1. Using separate lines of codes to obtain the tables from the webpage

tables <- html_nodes(webpage, 
                   "table")
head(tables)

html_table(tables,
           fill = TRUE)

# OR 

#2. as a pipe

tables_list <- webpage %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

str(tables_list)

table_df<-tables_list[1]%>%
  data.frame(.)

#Observing the structure of the dataframe

str(table_df)

# Since the dataframe has latitude and longitude as character vectors due to the °N and °E with the numbers. Hence the data will be cleaned for further transformations

# Separating the °N and °E from the numbers followed by selecting only the state name and lat long

clean_table<-table_df%>%
  separate(Latitude,
           c("lat","deg_N"),## separating the column to separate "°" from numbers
           sep="°")%>%
   separate(Longitude,
           c("lon","deg_E"),
           sep="°")%>% ## separating the column to separate "°" from numbers
  separate(lon,c("lon_d",
                 "lon_m"),sep="\\.")%>% ## separating the numeric column to separate degrees and minutes
  separate(lat,c("lat_d",
               "lat_m"),
         sep = "\\.")%>% ## separating the numeric column to separate degrees and minutes
    dplyr::select(State,
                  lat_d,
                  lat_m,
                  lon_d,
                  lon_m)%>%
  mutate_at(vars(contains('lat'),
                 contains('lon')),
            as.numeric)


# Observing the cleaned dataset

str(clean_table)

# The lat long data is in degrees and hence is converted to decimals 

clean_table2<-clean_table%>%
  dplyr::transmute(lat_dec=lat_d + lat_m/60,
            long_dec=lon_d + lon_m/60)%>%
  dplyr::mutate(state=clean_table$State)


View(clean_table2)


india_data_type_summ%>%
  left_join(clean_table2,by=c("state"))
