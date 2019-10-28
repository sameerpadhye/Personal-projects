# Species richness and diversity analyses

#libraries used

library(vegan)
library(tidyverse)
library(readxl)
library(magrittr)

# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-paste0(getwd(),"/species_richness_data.xlsx")


#Importing data for analysis (Species abundance dataset used here)

data_analysis<-read_excel(data_path,
                          sheet=1)


#Exploring the data

head(data_analysis,5)


### Visualizing the abundance data of all species


#a. Modifying the original dataset for the plot


species_data_heatmap<-data_analysis%>%
  column_to_rownames('Sites')%>%
  colSums(.)%>%
  data.frame(.)%>%
  rename('Sp_abundance'='.')%>%
  rownames_to_column(.)%>%
  arrange(desc(Sp_abundance))%>%
  mutate_at(vars(1),as.factor)


#b. Visualization


species_data_heatmap%>%
  ggplot(aes(rowname,
             Sp_abundance,
             size=Sp_abundance))+
  geom_point()+
  theme_bw(base_size = 18)+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1))+
  xlab("Species")+
  ylab("Species abundance")


#Obtaining the total number of species per site

sp_number<-vegan::specnumber(data_analysis[,-1])

sp_number


#Generating species accumulation curves using vegan package

species_acc<-specaccum(data_analysis%>%
                           select_if(is.numeric),
                       method = "random",
                       permutations = 999,
                       conditioned = TRUE,
                       gamma = "jack1")


#plot of accumulation curves using base R

plot (species_acc,
      col = 'blue',
      ci=2,
      main="Species accumulation curve using Jackknife1",
      xlab='Sites',
      ylab='Species')


#plotting the accumulation plots using ggplot2

#Extracting the data from the specaccum object generated using vegan package (use_series from magrittr specifically selects the essential data from the object using pipes)

sp_accu_sites<-species_acc%>%
    magrittr::use_series(sites) 

sp_accu_richness<-species_acc%>%
    magrittr::use_series(richness)

sp_accu_sd<-species_acc%>%
    magrittr::use_series(sd)

sp_richess_data<-cbind(sites=sp_accu_sites,
                       richness=sp_accu_richness,
                       std_dev=sp_accu_sd)%>%
    as.data.frame()

#plot (Additional arguments can be passed as per user requirements)

sp_richess_data%>% 
    ggplot(aes(x=sites,
               y=richness))+
    geom_point(color='black',
               fill='steelblue',
               pch=21)+
    geom_line(color = 'steelblue',
              lwd=1.2)+
    theme_bw(base_size = 16)+
    xlab('Sites')+
    ylab ('Species richness (Jack1)')+
    labs(title='Species accumulation curve')+
    theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
    geom_errorbar(aes(ymin=richness-std_dev,
                      ymax=richness+std_dev),
                  color = "steelblue",
                  alpha=0.3)


#Species richness esimates using all estimates at once

sp_rich_all<-poolaccum(data_analysis[,-1],
                       permutations = 999)

sp_rich_all


#Plot for above

plot(sp_rich_all)


#obtaining summary of specific index

summary(sp_rich_all, 
        display = "chao")


# Summarizing individual species diversity indices

#Shannon diversity of the sites

shannon_div<-vegan::diversity(data_analysis%>%
                                  select_if(is.numeric),'shannon')

shannon_div

#Shannon diversity of the sites

simpson_div<-vegan::diversity(data_analysis%>%
                                  select_if(is.numeric),'simpson')

simpson_div

#Evenness index

evenness_div<-shannon_div/sp_number

evenness_div


##Function for obtaining species richness results of individual indices in a dataframe for easy plotting using ggplot2

species_richess_data<-function(x,gamma=c("jack1",
                                         "jack2", 
                                         "chao", 
                                         "boot",
                                         "Species"))
{ 
  
  species_acc<-specaccum(x%>%
                           select_if(is.numeric),
                         method = "random",
                         permutations = 999,
                         conditioned = TRUE,
                         gamma = gamma)
  
  sp_accu_sites<-species_acc%>%
    magrittr::use_series(sites) 
  
  sp_accu_richness<-species_acc%>%
    magrittr::use_series(richness)
  
  sp_accu_sd<-species_acc%>%
    magrittr::use_series(sd)
  
  sp_richess_data<-cbind(sites=sp_accu_sites,
                         richness=sp_accu_richness,
                         std_dev=sp_accu_sd)%>%
    as.data.frame()
  
  print(sp_richess_data)
}


##Rarefaction
#Here a slightly modified dataset has been used (from the sample_dataset)

#Data file path (assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-paste0(getwd(),"/rarefaction_data.xlsx")


#Importing data for analysis

data_analysis<-read_excel(data_path,
                          sheet=1)


#Finding the sample (site) having minimum number of individuals 

min_ind_sample<-data_analysis[,-1]%>%
  rowSums(.)%>%
  min(.)


#Rarefaction (using the min_ind_sample as a sample value in the rarefaction analysis)

rarefaction<-rarefy(data_analysis[,-1],
                    sample = min_ind_sample)

rarefaction

#plot using rarefied values and species_number

plot(sp_number, 
     rarefaction, 
     xlab = "Species number (Observed)", 
     ylab = "Species number (Rarefied)",
     main = " Observed species no. vs. Rarefied species no.")
