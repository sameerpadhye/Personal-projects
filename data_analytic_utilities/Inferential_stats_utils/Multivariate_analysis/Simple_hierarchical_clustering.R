##Basic Hierarchical clustering 

# In biodiversity studies, clustering is used to visualize beta diversity patterns between different localities using species (abundance or occurrence data)

#Libraries used
library(tidyverse)
library(readxl)
library(ggdendro)
library(vegan)
library(dendextend)

#data_file (path can be changed to where dataset is saved)
file_clustering<-paste0(getwd(),"/data_clustering.csv")

#importing data     
data_for_cluster<-read.csv(file_clustering)

#Exploring the structure of data
head(data_for_cluster,5)

#For calculating the betadiversity, subset of data which is numerical is selected 
numdata_cluster<-data_for_cluster%>%
    dplyr::select_if(is.numeric)

#Converting the dataset into a distance matrix
distance_cluster<-vegan::vegdist(t(numdata_cluster),
                                 index="jaccard", # distance index can be changed as per requirement 
                                 binary = T,na.rm=T)# this can vary based on the type of input data (i.e. whether data is binary or not)

#Converting the distance matrix into a 'hclust' followed by 'dendrogram' object for plotting
object_for_plot<-hclust(distance_cluster,
                        method="average")%>% # method can be changed as per requirement
    as.dendrogram(.)

#To find out the group membership of each node, cutree function with specific k value can be used; 'k' specifies the number of clusters.
cutree(object_for_plot,k=3)

#Plot using base R
plot_cluster<-plot(object_for_plot,
                   cex=1,
                   main="Heirarchichal clustering",
                   ylab="Similarity",
                   xlab=NULL,
                   edgePar = list(col = c("steelblue"), 
                                  lwd = 2,
                                  lty=1),
                   horiz=F)

#plotting a triangle plot
plot(object_for_plot,type="triangle")

#Plot using ggdendro
ggdendro::ggdendrogram(object_for_plot, 
                       rotate = TRUE, 
                       theme_dendro = TRUE,
                       size=5)+
    theme_bw(base_size=18)+
    xlab("Xlabel")+
    ylab("Name of the distance index used")

# Radial plot using the package dendextend
library(dendextend)
ggplot(object_for_plot,horiz = T)+coord_polar(theta="x")

