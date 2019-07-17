#Evaluation the species faunistics and diversity of Pashan water reservoir before and after its beautification


#libraries used

library(vegan)
library(tidyverse)
library(readxl)
library(magrittr)


# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-"C:/Users/samee/Desktop/R data/sample_datasets/pashan_data.xlsx"


#Pashan pre 2016 data.

pashan_pre_2016<-read_excel(data_path,
                            sheet=1)


#Pashan 2016 collection data

pashan_2016<-read_excel(data_path,
                        sheet=2)


#two groups will be used one for 2016 samples which symbolize the samples collected after beautification and 2008-2010 samples which represent the samples collected before the beautification. Therefore some of the samples from the pashan_pre_2016 will be removed for the analysis. These years have been selected since the sampling strategy was similar to the 2016 sampling.

pashan_pre_2016_final<-pashan_pre_2016%>%
    dplyr::select('Species':'2010_10')


#Pashan full data

pashan_full_data<-pashan_pre_2016_final%>%
    left_join(pashan_2016%>%
                  dplyr::select(-Family),
              by='Species')%>%
    replace(., is.na(.), 0)


#Exploring the data

names(pashan_full_data)


#converting the data from wide to long format using both datasets (pre 2016 and 2016 respectivley)

pashan_long<-pashan_full_data%>%
    dplyr::select('Species':'2010_10',
                  '2016_2_1':'2016_12_1')%>%
    gather(Years,
           values,
           '2008_11':'2016_12_1')


#For the long dataset, two groups will be used one for 2016 samples which symbolize the samples collected after beautification and 2008-2010 samples which represent the samples collected before the beautification

#1. Generating a index to select the samples collected in the year 2016

index_2016<-str_detect(pashan_long$Years,'2016')

# finding out the number of rows with 2016 index (and pre 2016 rows respectivley)

post_no<-nrow(pashan_long[index_2016,])

pre_no=nrow(pashan_long)-post_no


#2. Updating the Pashan dataset with the new grouping column

pashan_long<-pashan_full_data%>%
    dplyr::select('Species':'2010_10','2016_2_1':'2016_12_1')%>%
    gather(Years,values,'2008_11':'2016_12_1')%>%
    dplyr::mutate(collection_grp=rep(c("pre","post"),
                                     times=c(pre_no,
                                             post_no)))


#Exploring the data

View(pashan_long)

# Re-order the collection period groups for based on collection periods (i.e. pre and post respectivley)

pashan_long$collection_grp<-fct_relevel(pashan_long$collection_grp,"pre")


########## Pashan data exploration and visualization ###############

#1. Total species in the pre and post beautification collections

## The pashan_long data contains multiple entries of the same species based on the samples collected. Hence, the data are modified to have uniqiue species identities with corresponding presence/absence (0/1) based on the collection groups

#Obtaining the summarized data

pashan_sp_data<-pashan_long%>%
    group_by(Family,
             Species,
             collection_grp)%>%
    summarise(species_tot=sum(values))%>%
    mutate(sp_pre_abs=ifelse(species_tot>0,1,0))


#plot 

pashan_sp_data%>%
    group_by(collection_grp)%>%
    summarise(sp_tot=sum(sp_pre_abs))%>%
    ggplot(aes(x=collection_grp,
               y=sp_tot))+
    geom_col(fill='steelblue')+
    theme_bw(base_size = 15)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1))+
    labs(x="Collection period",
         y="Total species",
         title = "Total species observed during two periods of collection")


#2. Family wise distribution of species in pre and post beautification periods

pashan_sp_data%>%
    ggplot(aes(x=Family,
               y=sp_pre_abs))+
    geom_col(fill='orange')+
    facet_wrap(~collection_grp)+
    theme_bw(base_size = 15)+
    labs(x="Familes",
         y="Total species",
         title = "Total species observed in Pashan")+
    coord_flip()


#3. Individual species occurrences (counts) in the pre and post collections 

pashan_species<-pashan_long%>%
    mutate_if(is.character,
              as.factor)%>%
    group_by(Family,
             Species,
             collection_grp)%>%
    summarize(total_occ=sum(values))%>%
    arrange(desc(total_occ))

#Re-order the species based on total occurrences

pashan_species$Species<-fct_reorder(pashan_species$Species,
                                    pashan_species$total_occ)

#plot

pashan_species%>%
    ggplot(aes(x=Species,
               y=total_occ))+
    geom_col(fill='orange')+
    facet_wrap(~collection_grp)+
    theme_bw(base_size = 15)+
    labs(x="Familes",
         y="Occurrences",
         title = "Species observed in Pashan")+
    coord_flip()


#Species richness estimates and other related summaries

#The data required for the analysis need to be in wide format and hence the original data that was imported will be used. This has been done for the pre and post beautification period samples respectively

##1. Pre beautification species richness estimates

names(pashan_pre_2016_final)


#Transposing the data for further calculations

pashan_pre_2016t<-pashan_pre_2016_final %>%
    select_if(is.numeric)%>%
    t(.)%>%
    data.frame(.)


#Obtaining the total number of species per sample

sp_number_pre<-vegan::specnumber(pashan_pre_2016t)%>%
    data.frame(.)%>%
    rownames_to_column()%>%
    dplyr::rename('sp_no'='.',
                  'collection_years'='rowname')


#scatter plot showing the species numbers in respective years considered as pre beautification

sp_number_pre%>%
    ggplot(aes(x=collection_years,
               y=sp_no))+
    geom_point(col='forestgreen',
               size=4)+
    theme_bw(base_size = 15)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1))+
    labs(x="Collection period",
         y="Total species",
         title = "Total species observed in pre beautification period")


#Generating species richness estimates and accumulation curves of the pre beautification samples (using vegan)

sp_rich_all_pre<-poolaccum(pashan_pre_2016t,
                           permutations = 999)

#View the result

sp_rich_all_pre

#plot of accumulation curves using base R

plot(sp_rich_all_pre)


##2. Post beautification species richness estimates

names(pashan_2016)


#Transposing the data for further calculations

pashan_2016t<-pashan_2016 %>%
    select_if(is.numeric)%>%
    t(.)%>%
    data.frame(.)


#Obtaining the total number of species per sample

sp_number_post<-vegan::specnumber(pashan_2016t)%>%
    data.frame(.)%>%
    rownames_to_column()%>%
    dplyr::rename('sp_no'='.',
                  'collection_years'='rowname')


#scatter plot showing the species numbers in respective years considered as post beautification

sp_number_post%>%
    ggplot(aes(x=collection_years,
               y=sp_no))+
    geom_point(col='forestgreen',
               size=4)+
    theme_bw(base_size = 15)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1))+
    labs(x="Collection period",
         y="Total species",
         title = "Total species observed in post beautification period")


#Generating species richness estimates and accumulation curves of the pre beautification samples (using vegan)

sp_rich_all_post<-poolaccum(pashan_2016t,
                            permutations = 999)

#View the result

sp_rich_all_post

#plot of accumulation curves using base R

plot(sp_rich_all_post)


##Comparative account of species number samplewise distribution (boxplots) of pre and post beautification periods using the sp_number_pre and sp_number_post datasets

sp_numb_dist<-sp_number_pre%>% # pre beautification sp numbers
    dplyr::bind_rows(sp_number_post)%>% # post beautification sp numbers
    dplyr::mutate(collection_grp=rep(c("pre","post"), # adding the beautification period categories
                                     times=c(nrow(sp_number_pre),
                                             nrow(sp_number_post))))


#Recoding the collection group factor levels

sp_numb_dist$collection_grp<-fct_relevel(sp_numb_dist$collection_grp,"pre")


# explore the result

View(sp_numb_dist)


#plot to visualize the result

sp_numb_dist%>%
    ggplot(aes(x=collection_grp,
               y=sp_no))+
    geom_boxplot(col='black',
                 fill='forestgreen')+
    theme_bw(base_size = 15)+
    labs(x="Collection period",
         y="Total species",
         title = "Species number distribution for two collection periods")
