# Performing Mantel test

# Common libraries used

require(tidyverse)
require(vegan)

# The data used here are mite dataset (species abundances, environmental variables, spatial details of the sites)

# Abduances

data("mite")

sp_data<-mite

head(sp_data)

# Environment

data("mite.env")

env_data<-mite.env

head(env_data)

# Spatial data

data("mite.xy")

spatial_data<-mite.xy

head(spatial_data)


# Simple Mantel test using Species abundances and Geographical distances

# Obtaining distance matrices

sp_dist<-vegan::vegdist(sp_data,
                        method = "bray")

geo_dist<-vegan::vegdist(spatial_data,
                         method = "euclidean")

env_dist<-vegan::vegdist(subset(env_data,
                                select=c(SubsDens,
                                         WatrCont)),
                         method = "euclidean")

# Performing the test

simple_mantel<-mantel(sp_dist,
                      geo_dist,
                      method = "spearman",
                      permutations = 999,
                      na.rm = TRUE)

# Results

simple_mantel


# Partial Mantel using all the three types of data


partial_mantel<-mantel.partial(sp_dist,
                               env_dist,
                            geo_dist, 
                                method = "pearson",
                                permutations = 999,
                                na.rm = TRUE)

partial_mantel
