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


##Non metric dimensional scaling to assess the species association of the pre and post collection periods of Pashan

# Obtaining the data for analysis.Since the analysis would require the pre and post levels, pashan_long dataset is used

pashan_nmds_data<-pashan_long%>%
    dplyr::select(Species,
                  Years:collection_grp)%>%
    tidyr::spread(Species,
                  values)%>%
    column_to_rownames('Years')


#nMDS 

pashan_nmds<-metaMDS(pashan_nmds_data%>%
                         dplyr::select_if(is.numeric), # selecting the numeric data
                     distance = 'jaccard', #jaccard is used here in order 
                     k=2,
                     trymax = 99)


#Stressplot of the nMDS

stressplot(pashan_nmds)


# nMDS plot

# to obtain the number of observations of pre and post period respectivley

table(pashan_nmds_data$collection_grp)['pre']

table(pashan_nmds_data$collection_grp)['post']

#plot

#blank plot
ordiplot(pashan_nmds,
         type="n")

#species data
orditorp(pashan_nmds,
         display="species",
         col="red",
         air=0.01)

#sites data
orditorp(pashan_nmds,
         display="sites",
         cex=0.9,
         air=0.01,
         col=c(rep("steelblue",
                   table(pashan_nmds_data$collection_grp)['pre']),
               rep("forestgreen",
                   table(pashan_nmds_data$collection_grp)['post'])))

#convex hulls
# ordihull(pashan_nmds,
#          groups=pashan_nmds_data$collection_grp,
#          draw="polygon",
#          col="grey50",
#          label=F)


## Using PERMANOVA to check whether the differences in the species communities in the pre and post beautificationp periods are significant or not. The modified dataset used for nmds is used here as well

str(pashan_nmds_data)

# Before performing PERMANOVA, the data are converted into distances

pashan_perm_dist<-vegdist(subset(pashan_nmds_data,
                                 select=-collection_grp), 
                          method = "jaccard",
                          binary = T,
                          na.rm = T)


#This is followed by checking the beta dispersion of the distance data for multivariate spread (dispersion) of data using the pre and post beautification groups within the data

data_betadis<-betadisper(pashan_perm_dist, 
                         pashan_nmds_data$collection_grp)%>%
    anova(.)


#results of the beta dispersion

paste0("The P value for beta dispersion is:", data_betadis$`Pr(>F)`[1])

#Since the p value is more than 0.05, null hypothesis cannot be rejected (which means data have a homogeneous multivariate spread (equivalent to homogeneity of variances))


#PERMANOVA (Using Jaccard index)

data_permanova<-adonis(subset(pashan_nmds_data,
                              select=-collection_grp) ~ collection_grp,
                       data = pashan_nmds_data, 
                       permutations = 5000, 
                       method = "jaccard")

#PERMANOVA results

data_permanova$aov.tab


############### Functional diversity analysis ###############################

#Importing functional trait data

functional_traits<-read_excel("C:/Users/samee/Desktop/R data/sample_datasets/func_traits_species.xlsx", 
                              sheet = 1)


#Pashan nmds data is used for functional diversity analysis since it has the pre and post beautification periods added to the dataset. 

#Data modification is crucial before proceeding for functional diversity analysis because:

#1. Species only present in the Pashan lake need to be extracted out of the functional trait dataset which has more than 50 species data

#2.Rownames and columnames should match exactly corresponding to species names in the species diversity and functional dataset respectively

#3. Analyses has to be carried out for two collection periods. If the sample sequence is preserved, analysis can be carried out one time and then separated (which has been done below)

#Since pashan nmds data has been selected for the analysis, its species names sequence will be used for all further analysis. Thus this dataset is first modified in order to perform a join with the functional trait dataset which will be the species trait data in the same order as the modified pashan nmds data


#1. Obtaining the functional trait data

# Species names of the nmds dataset is used and then joined with the functional traits dataset to get the necessary species order

pashan_spnames_for_join<-pashan_nmds_data%>%
    dplyr::select(-collection_grp)%>%
    t(.)%>%
    data.frame(.)%>%
    rownames_to_column()%>%
    dplyr::rename('Species'='rowname')%>%
    dplyr::select_if(is.character)


func_traits_pashan<-pashan_spnames_for_join%>%
    left_join(functional_traits, 
              by = "Species")%>%
    dplyr::select(-Family,
                  -Salinity,
                  -Helmet,
                  -Water_Zone,
                  -Presence_Ocellus)%>%
    mutate_if(is.character, 
              as.factor)%>%
    column_to_rownames('Species')


#The species richness dataset (pashan nmds dataset is used as Pre and post periods are present and the sample order is preserved).

pashan_species_data<-pashan_nmds_data%>%
    dplyr::select(-collection_grp)%>%
    as.matrix(.)


#Checking if functional traits data of all species are in same order

#indices for 1. pashan species and 2. functional traits

species_order_pashan<-colnames(pashan_species_data)

species_order_fun_traits<-row.names(func_traits_pashan)

#checking the order

intersect(species_order_fun_traits,species_order_pashan)

match(species_order_fun_traits,species_order_pashan)


# Calculating functional diversity indices

#1. functional composition

if(!require(FD))install.packages('FD') 

func_comp_pashan<-functcomp(func_traits_pashan, pashan_species_data, CWM.type = c("dom"))


#2. functional diversity

pashan_fun_traits<-dbFD(func_traits_pashan, 
                        pashan_species_data, 
                        calc.FRic = T, 
                        calc.FDiv = T, 
                        ord = "podani", 
                        calc.FGR = T, 
                        clust.type = "average", 
                        stand.FRic = T, 
                        w.abun = F,
                        corr = "cailliez")

# Functional redundancy for each sample has been calculated using SYNCSA package

if(!require(SYNCSA))install.packages('SYNCSA') 

#calculation

pashan_func_red<-rao.diversity(pashan_species_data, 
                               traits = func_traits_pashan, 
                               phylodist = NULL, 
                               checkdata = TRUE, 
                               ord = "podani", 
                               put.together = NULL)


# Extracting the individual indices from the results.The sample names and collection periods are also added to the dataset since the order has been preserved already

pashan_fun_indices<-data.frame(Feve=pashan_fun_traits$FEve,
                               Fdis=pashan_fun_traits$FDis,
                               Fdiv=pashan_fun_traits$FDiv,
                               Fric=pashan_fun_traits$FRic,
                               Fred=pashan_func_red$FunRedundancy,
                               collection_grps=pashan_nmds_data$collection_grp)%>%
    dplyr::select(collection_grps,
                  everything())


#Functional group richness for each Pashan sample extracted from the dbFD result

pashan_fun_frp_rich<-pashan_fun_traits$FGR 


# Species categorized as per the number of groups extracted from the dbFD result

pashan_fun_grp_assign<-data.frame(sp.fun.cat=pashan_fun_traits$spfgr)
