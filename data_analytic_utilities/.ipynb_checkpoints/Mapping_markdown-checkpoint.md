GIS map using tmap
================
Sameer Padhye
2018-11-05

Loading the shapefiles or polygons for plotting
===============================================

The shapefiles should be imported using the *readOGR* function from the *sp* package and provided with an appropriate **CRS** (Coordinate Reference System).

``` r
# India map with states
india_state_map<-readOGR('C:/Data/GIS_layers/IND_adm/IND_adm1.shp')
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "C:\Data\GIS_layers\IND_adm\IND_adm1.shp", layer: "IND_adm1"
    ## with 35 features
    ## It has 16 fields

``` r
proj4string(india_state_map)<- CRS("+proj=longlat +datum=WGS84")

# Western Ghats shapefile
Western_Ghats<-readOGR("C:/Data/GIS_layers/WG new outline shapefiles/Western GHats outline.shp")
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "C:\Data\GIS_layers\WG new outline shapefiles\Western GHats outline.shp", layer: "Western GHats outline"
    ## with 4 features
    ## It has 4 fields

``` r
proj4string(Western_Ghats)<- CRS("+proj=longlat +datum=WGS84")

#Alternatively, we can specify the path to the shapefile separately using the 'dsn' arguement in the readOGR function. The code would be: readOGR(dsn="C:/mapdata",layer='IND_adm1')
```

Exploring the data
==================

A *Spatialpolygondataframe* object is generated which can then be explored for more details. Note that this object is an S4 object and has *slots* which can be viewed using the \*\*@\*\* operator.

``` r
head(india_state_map@data,
     3)
```

    ##   ID_0 ISO NAME_0 ID_1              NAME_1
    ## 0  105 IND  India 1287 Andaman and Nicobar
    ## 1  105 IND  India 1288      Andhra Pradesh
    ## 2  105 IND  India 1289   Arunachal Pradesh
    ##                                                                                              VARNAME_1
    ## 0 Andaman & Nicobar Islands|Andaman et Nicobar|Iihas de Andama e Nicobar|Inseln Andamanen und Nikobare
    ## 1                                                                                                 <NA>
    ## 2                       Agence de la Frontisre du Nord-Est(French-obsolete)|North East Frontier Agency
    ##   NL_NAME_1 HASC_1 CC_1         TYPE_1       ENGTYPE_1 VALIDFR_1 VALIDTO_1
    ## 0      <NA>  IN.AN <NA> Union Territor Union Territory      <NA>      <NA>
    ## 1      <NA>  IN.AP <NA>          State           State      <NA>      <NA>
    ## 2      <NA>  IN.AR <NA>          State           State      <NA>      <NA>
    ##   REMARKS_1 Shape_Leng Shape_Area
    ## 0      <NA>   33.51405  0.6304842
    ## 1      <NA>   58.91631 23.2812969
    ## 2      <NA>   24.85199  7.5293014

Extracting specific polygons from the main map
==============================================

After exploring the object, the specific polygon file to be mapped can be extracted from the main object using the *$* operator. Here, I have extracted Maharashtra and Goa state polygons from the India map

``` r
#Maharashtra state polygon
is_mh_state <- india_state_map$NAME_1== "Maharashtra"
maharashtra_state<-india_state_map[is_mh_state,]

#Goa state polygon
is_goa_state <- india_state_map$NAME_1== "Goa"
goa_state<-india_state_map[is_goa_state,]
```

Importing GIS data of the samples
=================================

Data points to be mapped must also be converted into a *Spatialpolygondataframe* object

``` r
# Importing the data
GIS_samples<-read.csv(file="C:/Data/Research data/Large Branchiopoda/Large branchipod distribution data paper/L_branchiopod data.csv",
                 header=T)
```

The names of the field for the Latitude and Longitude are as per the specific dataset, though, the order should not change. The **CRS** should be the same as used for the map.

``` r
# converting the data into a spatialdataframe object. 
sampling_points<-SpatialPointsDataFrame(coords =GIS_samples[,c("Longitude","Latitude")],                                     data = GIS_samples,
                                        proj4string = CRS("+proj=longlat +datum=WGS84"))
```

Plotting the map
================

Plots can then be made as requirement (Here, I have used the *tmap* package, though, maps can also be made using many other libraries like *ggmap*).

![](Mapping_markdown_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
#2. Mapping the sample GIS data on the outline map

sample_map<-outline_map+
  tm_shape(sampling_points)+
  tm_dots(size=0.4, 
          col="red")

sample_map
```

![](Mapping_markdown_files/figure-markdown_github/unnamed-chunk-7-1.png)

Additional features such as compass, scales and grids can then be added on the map.

``` r
sample_map+
  tm_compass(position = c(.75, .15), 
             color.light = "grey90")+
  tm_layout(inner.margins=c(.11,.05, .04, .04))+ 
  tm_scale_bar(position=c("right", 
                          "bottom"))+
  tm_grid(n.x=4,
          n.y=4,
          lwd=0.4,
          alpha = 0.6,
          col='grey60',
          labels.size = 0.8)
```

![](Mapping_markdown_files/figure-markdown_github/unnamed-chunk-8-1.png)

Using tmap function 'ttm()' map view can be shifted to an interactive from static and vice versa.

This gives a basic flowchart to generate simple maps. There are many other custom function that can be used to change/modify or beautify the maps. One example is using Raster files along with the shapefiles for providing additional information

END
---
