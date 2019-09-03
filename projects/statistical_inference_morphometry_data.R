##Exploring and testing the differences in morphometric trait ratio of a commonly occurring freshwater zooplankter, Ceriodaphnia cornuta from a few localities in tropical India

#Libraries used

require(tidyverse)
require(vegan)
require(psych)
require(FSA)
require(ggpubr)
require(ggfortify)
require(car)

#Importing data (the data file is assumed to be in the working directory)

morphometry_file_path<-"C:/Users/samee/Desktop/R data/sample_datasets/morphometry_data.csv"

morphometry_raw_data<-read.csv(morphometry_file_path)


# Selecting and modifying data for analysis

morphometry_data<-
    dplyr::select(morphometry_raw_data,
                  site,
                  longitudo_corporis,
                  Altitudo_carapacis)%>%
    dplyr::rename('tot.len'='longitudo_corporis',
                  'tot.wid'='Altitudo_carapacis')%>%
    arrange(site)%>%
    mutate(ratio=tot.len/tot.wid)%>%
    dplyr::select(site,ratio)


#Exploring the data

head(morphometry_data,5)


#Checking the data for normality and variance homogeneity

#1. Normality (Shapiro Wilk test)

shapiro.test(morphometry_data$ratio)

#2. Homogeneity of variances (Levene's Test)

leveneTest(ratio ~ site, 
           data=morphometry_data)

#Since the data are non normal, Kruskal Wallis test has been used

#Krusal Wallis test for non normal data

kruskal.test(ratio~site,data=morphometry_data)

#post hoc test for Kruskal

dunnTest(ratio ~ site,data=morphometry_data,method="bh")

# plotting Kruskal test

ggboxplot(morphometry_data, 
          x = "site", 
          y = "ratio",
          color = "site",
          palette="jco",
          add = "jitter",
          shape = "site")+
    theme_bw(base_size = 16)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1))+
    xlab("Sites")+
    ylab("Ratio")

#Plotting interactive plot with plotly

require(plotly)

#boxplot with jitter

morphometry_data%>%
    plot_ly(x = ~site,
            y=~ratio,
            type = "box",
            boxpoints = "all",
            color = ~site)%>%
    layout(title = 'Boxplot of ratio values across all the sites',
           axis = list(title = "Collection sites"),
           yaxis = list(title = "Value"))

## Using permutation ANOVA for inferring statistical significance


#require for permutation ANOVA

require (lmPerm)


#Anova model

cerio_model<-aovp(ratio ~ site,seqs=T,perm="",data=morphometry_data)

cerio_model_summary<-summary(cerio_model)


#To export the results

write.csv(cerio_model$`Error: Within`[[1]],"cerio_model.csv")


#Exploring the multivariate associations of the traits with sites using Principal Components Analysis


# PCA analysis

pca_morpho_analysis<- morphometry_raw_data%>%
    dplyr::select_if(is.numeric)%>%
    dplyr::select(-specimen.no)%>%# only numeric data should be selected
    data.frame(.)%>% # converted to dataframe since tibble is returned
    prcomp(.,scale. = F) # scale is TRUE when column descriptors are of different scales (E.g. one column contains pH values while the second contains Temperature)

#extracting the PCA co-ordinates

pca_analysis_values<-pca_morpho_analysis$x

#extracting PCA vector values of the first two axes 

pca_analysis_vector<-pca_morpho_analysis$rotation[,c(1,2)]


#plot for PCA

#1.autoplot function from the package ggplot2 used to visualize the result s of the PCA analysis

autoplot(pca_morpho_analysis, 
         scale = 0, 
         data = morphometry_raw_data, 
         colour='site',
         label=T,
         #label.label = "grid_no",
         size = 5, 
         shape='site',
         frame=T,
         frame.colour = 'site',
         loadings = TRUE, 
         loadings.colour = 'black', 
         loadings.label = TRUE, 
         loadings.label.size = 4, 
         loadings.label.hjust = 0.5, 
         loadings.label.vjust = 1.2)+
    # theme
    theme_bw(base_size = 18)+
    # scale_color_manual(values=c("orange",'forestgreen',"blue","grey50"))+
    scale_size_manual(values =2)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
## There are mulitple arguments not passed here that can be added as per requirement.    


##Scree plot for PCA (used to visualize the contribution of each eigenvector in explaining the variation)

require(factoextra)

fviz_eig(pca_morpho_analysis)


## Performing MANOVA to test whether there is a difference between traits and between different localities

#Performing MANOVA

data_manova<-manova(cbind(longitudo_corporis,
                          longitudo_capitis,
                          longitudo_carapacis,
                          Altitudo_capitis,
                          Altitudo_carapacis
) ~ site, 
data = morphometry_raw_data)

#Exploring the results of MANOVA

summary(data_manova)


#ANOVA's of individual response variables to the grouping variable (posthoc test)

post_aov_data<-summary.aov(data_manova)

#Exploring the results of MANOVA

post_aov_data


# Visualizing the differences in trait values accross the sites

require(plotrix)

data_plot<-morphometry_raw_data%>%
    gather(traits,
           values,
           longitudo_corporis:Altitudo_carapacis)%>%
    mutate_at(vars(contains('traits')),
              as.factor)%>%
    group_by(site,
             traits)%>%
    summarize(means=mean(values,
                         na.rm = T),
              std_error=plotrix::std.error(values,
                                           na.rm = T))%>%
    ggplot(aes(site,
               means,
               group=site))+
    geom_point(color='steelblue',
               size=4,
               position=position_dodge(0.5))+
    geom_errorbar(aes(ymin=means-std_error,
                      ymax=means+std_error),
                  width=.2,
                  position=position_dodge(0.5))+
    theme_bw(base_size = 18)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1),
          axis.title.x = element_blank())+
    facet_wrap(~traits,
               scales = 'free')+
    ylab('Mean')

data_plot 
