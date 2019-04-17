##Basic Heirarchichal clustering 

#Libraries used
library(tidyverse)
library(readxl)
library(ggdendro)
library(vegan)

#importing data (if data stored in the working directory;otherwise specific path can be pasted)

data_for_cluster<-read_excel(paste0(getwd(),"/data_clustering.xls"),
                           sheet=1)%>%
    dplyr::mutate_at(vars(contains('Family')),
                     as.factor)

#Exploring the structure of data
str(data_for_cluster)

#For calculating the betadiversity, subset of data which is numerical is selected 
numdata_cluster<-data_for_cluster%>%
    dplyr::select_if(is.numeric)

#Converting the dataset into a distance matrix
distance_cluster<-vegdist(t(numdata_cluster),
                           index="jaccard", # distance index can be changed as per requirement 
                           binary = T)# this can vary based on the type of input data (i.e. whether data is binary or not)

#Converting the distance matrix into a 'hclust' followed by 'dendrogram' object for plotting
object_for_plot<-hclust(distance_cluster,
                        method="average")%>% # method can be changed as per requirement
    as.dendrogram(.)

#Plot using base R
plot(object_for_plot,
     cex=1,
     main="Similarity between regions of Indian subcontinent",
     ylab="Similarity",
     xlab=NULL,
     edgePar = list(col = c("steelblue"), 
                    lwd = 2,
                    lty=1),
     horiz=F)

#Plot using ggdendro
ggdendro::ggdendrogram(object_for_plot, 
                       rotate = TRUE, 
                       theme_dendro = TRUE,
                       size=5)+
theme_bw(base_size=18)+
    xlab("Xlabel")+
    ylab("Name of the distance index used")
