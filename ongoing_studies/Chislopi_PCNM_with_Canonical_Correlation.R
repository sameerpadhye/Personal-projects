##PCNM analysis using C.hislopi spatial data followed by a canonical correlation with environmental data

#GIS data of the samples
c_hislopi_loc<-read_excel("C:/Data/Research data/Large Branchiopoda/Cyclestheria hislopi world distribution/C.hislopi localities.xlsx",
                          sheet=1)%>%
    mutate_at(vars(1:3),as.factor)%>%
    dplyr::select(Longitude,
                  Latitude,
                  everything())
c_hislopi_loc$Realm<-fct_recode(c_hislopi_loc$Realm,
                                Neotropics="Neotropic")
# calculate inter-point distance matrix

distance_data<- dist(c_hislopi_loc[,c("Longitude","Latitude")])
distance_data
#PCNM (vegan)
pcnm_spatial_data<-vegan::pcnm(distance_data)%>%
    .$vectors%>%
    data.frame(.)%>%
    dplyr::mutate(codes=c_hislopi_loc$Codes)%>%
    column_to_rownames(.,'codes')

## Obtaining the environnental data
C.hislopi_all.data<-c.hislopi_bioclim%>%
    cbind(.,geochem_data)%>%
    as.data.frame(.)%>%
    dplyr::mutate(code=c_hislopi_loc$Codes,
                  localities=c_hislopi_loc$Localities,
                  country=c_hislopi_loc$Country,
                  altitude=c.hislopi_alt,
                  realm=eco_data.chislopi$Realm,
                  ecoregions=eco_data.chislopi$ecoregion)%>%
    dplyr::select(code,
                  country,
                  localities,
                  everything())%>%
    #dplyr::select_at(vars(starts_with('therm')))
    # na.omit(.)%>%
    #filter(realm!='Palearctic')%>%
    droplevels(.)%>%
    dplyr::select(-c("therm_cap0_",
                     "therm_cap10_",
                     "therm_cap100_",
                     "therm_cap50_"))%>%
    column_to_rownames(.,'code')
    na.omit(.)

C.hislopi_all.data$realm<-fct_recode(C.hislopi_all.data$realm,
                                     Neotropics="Neotropic")

#matching rows of PCNM and Env data since rows with NA values in env should be deleted from the PCNM dataset as well
same_rows <- match(rownames(C.hislopi_all.data),
                   rownames(pcnm_spatial_data))

pcnm_sel_data <- pcnm_spatial_data[same_rows,]

#Confirm if the rows have been deleted
match(rownames(pcnm_sel_data),rownames(C.hislopi_all.data))


#Canonical correlation analysis using pcnm_sel_data 
install.packages('CCA')
library(CCA)

#selecting the numerical data of environment
chislopi_sel.data<-C.hislopi_all.data[,c(3:36)]

#Visualization of correlation
correl<-matcor(chislopi_sel.data,pcnm_sel_data)
img.matcor(correl, type = 2)

#Canonical correlation

correl<-cc(chislopi_sel.data,pcnm_sel_data)

#Results
correl$xcoef

correl$cor

correl$scores

#Plot Canonical correlation
plt.cc(correl, var.label = TRUE)


#2.Canonical correlation using 'vegan' package (W.I.P.)

canonical_correlation_2<-CCorA(chislopi_sel.data,pcnm_sel_data,
                               stand.X = TRUE)

#Value for significance of the correlation
canonical_correlation_2$Pillai

#Biplot of the CCA
biplot(canonical_correlation_2,which=1:2)

#Mantel test to check association of spatial and env factors

## Obtaining the environnental data
C.hislopi_all.data2<-c.hislopi_bioclim%>%
    cbind(.,geochem_data)%>%
    as.data.frame(.)%>%
    dplyr::mutate(code=c_hislopi_loc$Codes,
                  localities=c_hislopi_loc$Localities,
                  country=c_hislopi_loc$Country,
                  altitude=c.hislopi_alt,
                  realm=eco_data.chislopi$Realm,
                  ecoregions=eco_data.chislopi$ecoregion)%>%
    dplyr::select(code,
                  country,
                  localities,
                  everything())%>%
    # na.omit(.)%>%
    #filter(realm!='Palearctic')%>%
    droplevels(.)%>%
    dplyr::select(-c("therm_cap0_",
                     "therm_cap10_",
                     "therm_cap100_",
                     "therm_cap50_"))%>%
    column_to_rownames(.,'code')
    
#selecting the numerical data of environment
chislopi_sel.data2<-C.hislopi_all.data2[,c(3:36)]

#Spatial distance (as a distance matrix)
distance_data2<- sp::spDists(c.hislopi_points,longlat=TRUE)
    # data.frame(.)%>%
    # dplyr::mutate(codes=c_hislopi_loc$Codes)%>%
    # column_to_rownames(.,'codes')

#Chislopi env data distance
chislopi_sel.data

chislop_env_dist<-vegdist(chislopi_sel.data2,method = 'gower',na.rm = TRUE)
chislop_env_dist

#Mantel (vegan)
vegan::mantel(distance_data, chislop_env_dist, method="spearman",permutations = 4999) 

#Mantel (ade4)
mantel.rtest(distance_data, chislop_env_dist, nrepet = 9999)


#Mantel (ecodist)
install.packages('ecodist')
library(ecodist)

ecodist::mantel(formula = distance_data~chislop_env_dist, data = sys.parent(), nperm = 1000,
       mrank = FALSE, nboot = 500, pboot = 0.9)



##Moran's I for spatial autocorrelation

#check spatial autocorrelation using Moran's I
# if there is spatial autocorrelation
# perform Mantel stats using this spatial autocorrelation provided in adespatial package

library(adespatial)
library(ade4)
library(spdep)
install.packages('spdep')

Moran.I()
