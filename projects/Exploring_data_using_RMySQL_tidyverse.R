#Exploring (visualizing) the 'world' sql dataset using RMySQL and tidyverse\
# (WIP)

#libraries
library(DBI)
library(RMySQL)
library(dplyr)
library(RColorBrewer)
library(tidyverse)

#Establishing a connection with server (local)

world_db<-dbConnect(MySQL(), 
                    user='root', 
                    password='gambitsam19*%', 
                    dbname='world', 
                    host='localhost')

#obtaining the list of different tables in the dataset
dbListTables(world_db)

#selecting the 'country' table
dbListFields(world_db,'country')

# To obtain a SQL query counterpart of the dplyr code
dplyr::tbl(world_db,"country")%>%
    dplyr::select(Code,
                  Name,
                  Continent,
                  SurfaceArea)%>%
    group_by(Continent)%>%
    summarise(mean_surf_area=mean(SurfaceArea,na.rm=T))%>%
show_query()

# Extracting the 'country' table
country_data<-dbReadTable(world_db,"country")

# Transforming the wide dataformat to a long one

long_format_data<-country_data%>%
    gather(Traits,
           values,
           Population,GNP)%>%
    mutate_if(is.character,
              as.factor)

## Different visualizations of the dataset highlighting certain aspects of the data attributes

#1. Number of countries in each continent
country_data%>%
    dplyr::select(Name,
                  Continent)%>%
    dplyr::filter(Continent!='Antartica')%>%
    droplevels(.)%>%
    na.omit(.)%>%
    mutate_all(as.factor)%>%
    group_by(Continent)%>%
    tally(.)%>%
    arrange(n)%>%
    ggplot(aes(x=Continent,
               y=n))+
    geom_bar(stat="identity",
             fill="orange",
             position = position_dodge(width=0.9))+ 
    theme_bw(base_size = 16)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    ylab("Number of countries")


#2. Visualizing the contrast between GNP and Population trends of the different continents 

long_format_data%>%
    ggplot(aes(x=Continent,
               y=values))+
    geom_bar(stat="identity",
             fill="orange",
             position = position_dodge(width=0.9))+
    geom_jitter(aes(x = Continent), 
                position = position_jitter(width = .25), 
                size=3,
                pch=21,
                alpha = 0.3,
                fill='forestgreen')+
    theme_bw(base_size = 16)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    facet_wrap(~Traits,scales = 'free')


#3. Visualizing the trends in Independence years of countries in all the continents (except Antartica)

country_data%>%
    dplyr::select(Name,
                  Continent,
                  IndepYear)%>%
    dplyr::filter(Continent!='Antartica')%>%
    droplevels(.)%>%
    na.omit(.)%>%
            ggplot(aes(x=IndepYear,
                       fill=Continent)) + 
        geom_histogram()+ 
    theme_bw(base_size = 16)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    scale_fill_brewer(palette="Dark2")+
    facet_wrap(~Continent,
               scales = 'free')

#4. Categorizing and visualizing the LifeExpectancy  data into lower and higher than avg LifeExpectancy respectively using case_when from dplyr

country_data%>%
    mutate(LifeExp_cat=case_when(LifeExpectancy<=mean(all_country_data$LifeExpectancy,na.rm=T)~'Lower than average',
                                 LifeExpectancy>=mean(all_country_data$LifeExpectancy,na.rm=T)~'Higher than average',))%>%
    filter(!is.na(LifeExp_cat))%>%
    ggplot(aes(LifeExp_cat))+
    geom_bar(fill='forestgreen')+
    theme_bw(base_size = 16)+ 
    ylab('Number of countries')+
    xlab('Life Expectancy categories')+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    facet_wrap(~Continent)


#5.Visualizing the number of countries categorized based on the SurfaceArea using plotly

country_data%>%
    mutate(SurfaceArea_cat=case_when(SurfaceArea<=mean(all_country_data$SurfaceArea,na.rm=T)~'Smaller than average',
                                     SurfaceArea>=mean(all_country_data$SurfaceArea,na.rm=T)~'Higher than average',))%>%
    filter(!is.na(SurfaceArea_cat))%>%
    group_by(SurfaceArea_cat)%>%
    tally(.)%>%
    plot_ly(x = ~SurfaceArea_cat,y=~n,type = "bar")%>%
    layout(title = 'Surface_Area categories',
           xaxis = list(title = "Surface area categories"),
           yaxis = list(title = "Country No."))


#6.Population vs GNP scatterplot of the 10 most populous countries using plotly

library(plotly)

country_data%>%
    dplyr::select(Name,Population,GNP)%>%
    filter(Population>mean(Population))%>%
    arrange(desc(Population))%>%
    slice(1:10)%>%
    plot_ly(x=~Population, 
            y=~GNP, 
            mode="markers" , 
            marker=list(color="blue" , 
                        size=20 , 
                        opacity=0.6,
                        line = list(color = 'black',
                                    width = 1.5)), 
            text = ~Name, 
            textposition = 'middle right',
            textfont = list(color = 'grey80', 
                            size = 12))%>%
    layout(title = 'Population vs. GNP',
           xaxis = list(title = "Population (in billion)"),
           yaxis = list(title = "GNP"))


#7. Visualizing the top 10 largest countries by SurfaceArea by a Lollipop plot

data_for_plot<-country_data%>%
    dplyr::select(Name,Population,SurfaceArea)%>%
    arrange(desc(SurfaceArea))%>%
    slice(1:10)

#Re-ordering the countries based on size

data_for_plot$Name<-fct_reorder(data_for_plot$Name, 
                                -data_for_plot$SurfaceArea) 

# plot for data visualization

data_for_plot%>%
    ggplot(aes(x=Name,
               y=SurfaceArea))+
    geom_segment(aes(x=Name, 
                     xend=Name, 
                     y=0, 
                     yend=SurfaceArea),
                 color='grey50',
                 size=1.5)+
    geom_point(color='grey50',
               size=5)+
    theme_classic(base_size = 14)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1))



# Adding GIS information (by country) to the world data using 'geocode' from ggmap

library(ggmap)

GIS_countries<-as.data.frame(ggmap::geocode(as.character(unique(country_data$Name)),source = 'dsk'))%>%
    mutate(location=unique(country_data$Name))%>%
    drop_na(.) # Here points with NA's is omitted

#Combining the GIS and the world dataset

all_country_data<-merge(country_data,
                        GIS_countries,
                        by.x='Name',
                        by.y='location')


#converting the  GIS data into a spatial object using sp package

country_points<-SpatialPointsDataFrame(coords = all_country_data[,c("lon","lat")],
                                       data = all_country_data,
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))


#Obtaining world shapefile

world_shape<-readOGR("C:/Users/samee/Downloads/HWSD_RASTER/world_shapefile/ne_50m_admin_0_countries.shp")

proj4string(world_shape)<- CRS("+proj=longlat +datum=WGS84")


# mapping the points using tmap

tm_shape(world_shape)+
    tm_borders()+
    tm_fill(col='grey',
            alpha = 0.6)+
    tm_shape(country_points)+
    tm_dots(size=0.3, 
            col="red", 
            border.col="black")+
    tm_grid(n.x=4,
            n.y=4,
            lwd=0.4,
            alpha = 0.6,
            col='grey60',
            labels.size = 0.8)

#for switching between static and interactive map
ttm()


# Next, will be merging country data to the world shapefile using the 'merge' function from sp package

world_new_shpfile <- sp::merge(world_shape, 
                               all_country_data%>% # required data selected
                                   dplyr::select(Code:GNP,
                                                 lon,
                                                 lat), 
                               by.x="NAME",
                               by.y="Code")


