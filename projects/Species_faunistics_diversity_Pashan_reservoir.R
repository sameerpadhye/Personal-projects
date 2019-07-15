#Evaluation the species faunistics and diversity of Pashan water reservoir before and after its beautification


#libraries used

library(vegan)
library(tidyverse)
library(readxl)
library(magrittr)


# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-"C:/Users/samee/Desktop/R data/sample_datasets/pashan_data.xlsx"


#Pashan pre 2016 data

pashan_pre_2016<-read_excel(data_path,
                          sheet=1)


#Pashan 2016 collection data

pashan_2016<-read_excel(data_path,
                        sheet=2)


#Pashan full data

pashan_full_data<-pashan_pre_2016%>%
    left_join(pashan_2016%>%
                  dplyr::select(-Family),
              by='Species')%>%
    replace(., is.na(.), 0)


#Exploring the data

View(pashan_full_data,5)


#converting the data from wide to long format

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


# Pashan data exploration and visualization 

#1. Family wise distribution of species in pre and post beautification periods

# Re-order the collection period groups for better plots

pashan_long$collection_grp<-fct_relevel(pashan_long$collection_grp,"pre")

#Obtaining the summarized data

pashan_family_summ<-pashan_long%>%
    mutate_if(is.character,
              as.factor)%>%
    group_by(Family,collection_grp)%>%
    summarize(total_sp=sum(values))

#Re-order the families based on total species number

pashan_family_summ$Family<-fct_reorder(pashan_family_summ$Family,
                                       pashan_family_summ$total_sp)


#plot

pashan_family_summ%>%
    ggplot(aes(x=Family,
               y=total_sp))+
    geom_col(fill='orange')+
    facet_wrap(~collection_grp)+
    theme_bw(base_size = 15)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1))+
    labs(x="Familes",
         y="Total species",
         title = "Total species observed in Pashan")

#WIP
