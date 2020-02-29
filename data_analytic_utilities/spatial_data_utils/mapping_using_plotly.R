#Plotting maps using plotly

#libraries used

library(tidyverse)
library(plotly)



#The world map is taken from the package rnaturalearth and converted into a sf object by sf package which then can be plotted in plotly

if(!require(sf))install.packages('sf')
if(!require(rgeos))install.packages('rgeos')
if(!require(rnaturalearth))install.packages("rnaturalearth")

# Creating the map file

world_map <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")


#1. Base map of the world

plot_ly(world_map,
        stroke = I("black"),
        hoverinfo='text',
        color = I("gray50")
        )%>%
  layout(title = "World map")


#2. Splitting based on the regions (regions of the world as provided in the world_map object. it can be explored by using View(world_map))

plot_ly(world_map,
        stroke = I("black"),
        split = ~region_un,
        color = ~region_un
)%>%
  layout(title = "World map")


#3. Selecting a specific country and mapping

#selecting the country

India <- ne_states(country = "India", 
                   returnclass = "sf")

#plot

plot_ly(India, 
        stroke = I("black"),
        split = ~name, 
        color = ~woe_name)%>%
  layout(title = "Map of India",
         showlegend=FALSE)

#WIP
