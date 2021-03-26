## Distribution and functional composition of Indian tropics


#packages

library(reshape2)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(psych)
library(readxl)
library(ggfortify)
library(ggdendro)
library(dendextend)
library(phia)
library(ez)
library(vegan)
library(betapart)
library(VennDiagram)
library(classInt)
library (cluster)
library(picante)
library(adespatial)
library(FD)
library(ape)
library(ade4)
library(RColorBrewer)
library(plotly)
library(GGally)

#Importing data

cladocera<-read_excel("C:/Data/Research data/Cladocera/Cladocera distribution of western ghats data/All data.xlsx",
                      sheet=1)

# Observing the structure of the dataset

str(cladocera)

#converting characters into factors

cladocera$Habitat_type<-as.factor(cladocera$Habitat_type)

cladocera$Aq_Veg<-as.factor(cladocera$Aq_Veg)

# split data into species + environment and species + habitats

#Species+env

cladocera_env=cladocera[1:156,]

names(cladocera_env)

#Species+habitats

cladocera_habitat<-dplyr::select(cladocera,
                                 Name,
                                 Habitat_type,
                                 Total_sp.,
                                 D_excisum:C_sphaericus)


cladocera_habitat_no=xtabs(~Habitat_type,
                           cladocera_habitat)

cladocera_habitat_no


# converting to a long format

cladocera_habitat2<-gather(cladocera_habitat,
                           Species,
                           Occurences,
                           D_excisum:C_sphaericus)

cladocera_occurences<-xtabs(~Species+Occurences,
                            cladocera_habitat2)

cladocera_occurences

# descriptive plot of species found in 4 habitat types

#plot

ggplot(cladocera_habitat2,
       aes(x=Habitat_type,
           y=Total_sp.))+
  geom_boxplot(fill="steelblue",
               position=position_dodge(0.6),
               width=0.5)+
  theme_bw(base_size = 17)+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) + 
  xlab("Habitat type")+
  ylab("Total species")+
  scale_x_discrete(labels=c("pool" = "Pools", 
                            "pond" = "Ponds",
                            "lake" = "Reservoirs",
                            "river"="Rivers"))                              

##Taxonomic species richness per habitats

clado_local_all=read_excel("C:/Data/Research data/Cladocera/Cladocera distribution of western ghats data/All data.xlsx",
                           sheet=8)

names(clado_local_all)

#selecting and separating mean and SD's (in long format)

cladocera_sprich_mean<-dplyr::select(clado_local_all,
                              Pool_Chao_2_Mean,
                              Pond_Chao_2_Mean,
                              Lake_Chao_2_Mean,
                              River_Chao_2_Mean)

## plotting rarefaction values

clado_local_rare<-read_excel("C:/Data/Research data/Cladocera/Cladocera distribution of western ghats data/All data.xlsx",
                             sheet=9)

cladocera_rarefac<-clado_local_rare[1:18,]

View(cladocera_rarefac)

#selecting and separating mean and SD's (in long format)

cladocera_rare_mean=dplyr::select(cladocera_rarefac,
                           Lake,
                           Pond,
                           Pool,
                           River)%>%
  gather(Habitats,
         Richness,
         Lake:River)%>%
  mutate(Samples=rep(1:18,times=4))%>%
  dplyr::select(Samples,
         everything())

View(cladocera_rare_mean)

#re-ordering and renaming factors (habitat types)

cladocera_rare_mean$Habitats<-as.factor(cladocera_rare_mean$Habitats)


levels(cladocera_rare_mean$Habitats)

cladocera_rare_mean$Habitats<-factor(cladocera_rare_mean$Habitats,
                                     levels(cladocera_rare_mean$Habitats)[c(3,2,1,4)])

#renaming levels

levels(cladocera_rare_mean$Habitats)

levels(cladocera_rare_mean$Habitats) <- c("Pools","Ponds","Reservoirs","Rivers")

#selecting required data

cladocera_rare_SD=dplyr::select(cladocera_rarefac,
                         Lake_SD,
                         Pond_SD,
                         Pool_SD,
                         River_SD)%>%
  gather(Habitats,
         SD,
         Lake_SD:River_SD)%>%
  mutate(Samples=c(1:72))%>%
  dplyr::select(Samples,
         everything())

#plot

rare_plot=ggplot(cladocera_rare_mean, 
                 aes(x=Samples, 
                     y=Richness,
                     color=Habitats,
                     shape=Habitats)) + 
  geom_point(size=3)+
  xlim(0,20)

rare_plot+
  geom_smooth(method="auto", 
              se=FALSE, 
              fullrange=TRUE)+
  scale_color_manual(values=c("steelblue",
                              "red",
                              "forestgreen",
                              "gray50"))+ 
  xlab("Samples")+ylab("Richness")+
  theme_minimal(base_size = 16)

######### For random subset of samples w.r.t. lowest sample count 15-4-18

### using random dataset from the 156 samples data with env variables

#getting the necessary dataset

clado_env_data=subset(cladocera[1:156,])

clado_env_data2=select(clado_env_data,
                       Habitat_type,
                       Aq_Veg,
                       Altitude,
                       Total_sp.,
                       Temperature,
                       Salinity:C_sphaericus)

View(clado_env_data2)

# getting the number of samples per habitat

table(clado_env_data2$Habitat_type)

##function for selecting the habitat types

sel_habitats=function(x,habitats=c("pool","pond","lake","river")){
  for (i in 1:nrow(x)){
    filt=filter(x,Habitat_type==habitats)
    return(as.data.frame(filt))
  }
}

#making subsets habitatwise using the function

cladocera_pool_env<-sel_habitats(clado_env_data2,
                                habitats="pool")%>%
  sample_n(17)%>%
  mutate(sample=sprintf("pools%d",
                        seq=(1:17)))%>%
  select(sample,
         everything())

cladocera_pond_env<-sel_habitats(clado_env_data2,
                                habitats="pond") %>%
  sample_n(17) %>%
  mutate(sample=sprintf("ponds%d",
                        seq=(1:17)))%>%
  select(sample,
         everything())

cladocera_lake_env<-sel_habitats(clado_env_data2,
                                habitats="lake") %>%
  sample_n(17)%>%
  mutate(sample=sprintf("lakes%d",
                        seq=(1:17)))%>%
  select(sample,
         everything())

cladocera_river_env<-sel_habitats(clado_env_data2,
                                 habitats="river")%>%
  sample_n(17)%>%
  mutate(sample=sprintf("rivers%d",
                        seq=(1:17)))%>%
  select(sample,
         everything())

##Full_join of random subsamples of the dataset (for CCA)

clado_r_1_env=full_join(cladocera_pool_env,
                        cladocera_pond_env)

clado_r_2_env=full_join(cladocera_lake_env,
                        cladocera_river_env)

clado_env_data3=as.data.frame(full_join(clado_r_1_env,
                                        clado_r_2_env))

names(clado_env_data3)

#changing colnames

colnames(clado_env_data3)[7]="Temperature"

colnames(clado_env_data3)[3]="Aquatic_vegetation"

#converting aquatic vegetation variable to numeric

clado_env_data3$Aquatic_vegetation=as.numeric(clado_env_data3$Aquatic_vegetation)

#removing total species from the dataset

clado_env_data3=clado_env_data3[,-5]

#renaming levels for the dataset

levels(clado_env_data3$Habitat_type)

levels(clado_env_data3$Habitat_type)=c("Reservoirs","Ponds","Pools","Rivers")

str(clado_env_data3)

#obtaining the environmental and species occurrence data for CCA

cca_clado_fin=dplyr::select(clado_env_data3,
                     Aquatic_vegetation:I_ganapati)

cca_clado_env=dplyr::select(cca_clado_fin,Aquatic_vegetation:Salinity)

cca_clado_sp=dplyr::select(cca_clado_fin,D_excisum:I_ganapati)%>%
  select_if(colSums(cca_clado_sp)>0)

##CCA

clado_cca=cca(cca_clado_sp,
              cca_clado_env,
              na.action=na.omit)

#finding if descriptors are collinear

vif.cca(clado_cca)

anova(clado_cca)

# data summary of CCA

summary_cca=summary(clado_cca)

summary_cca

#plotting CCA final - 25-4-18

# plot sites

plot(clado_cca, 
     display=c("sites", "bp"), 
     type="n", 
     main="Sites", 
     scaling="sites")

points(clado_cca,
       col=colvec, 
       scaling="sites",
       pch=20,
       cex=2)

text(clado_cca, 
     display="bp", 
     col="black",
     lwd=2,
     lty=1,
     cex=1.1)

#legend for the plot

with(clado_cca, 
     legend("bottomright", 
            legend = levels(clado_env_data3$Habitat_type), 
            bty = "n",
            col = colvec, 
            pch = 21, 
            pt.bg = colvec))

# plot species

plot(clado_cca, 
     display=c("species", "bp"), 
     type="n",
     ylab="", 
     main="Species", 
     scaling="species")

text(clado_cca, 
     display="species", 
     col="red",
     cex=0.9,
     pos=3,
     adj=0,
     font=3)

points(clado_cca, 
       display="species",
       col="red",
       pch=20)

text(clado_cca, 
     display="bp", 
     col="black",
     lwd=3,
     lty=1)

#Scree

barplot(clado_cca$CA$eig/clado_cca$tot.chi, 
        names.arg = 1:clado_cca$CA$rank, 
        cex.names = 0.5, 
        ylab="Proportion of variance explained", 
        xlab="CCA axis",
        ylim=c(0,0.1))


# beta diversity of random subset of 230 samples from W.ghats

#getting the subsets

pool_total_r=colSums(select(cladocera_pool,
                            D_excisum:C_sphaericus))
pond_total_r=colSums(select(cladocera_pond,
                            D_excisum:C_sphaericus))
lake_total_r=colSums(select(cladocera_lake,
                            D_excisum:C_sphaericus))
river_total_r=colSums(select(cladocera_river,
                             D_excisum:C_sphaericus))

#combining the above # cbind for all the totals of all species

tot_cladocera_habitats_r=as.data.frame(cbind(pool_total_r,
                                             pond_total_r,
                                             lake_total_r,
                                             river_total_r,
                                             make.row.names = TRUE))
head(tot_cladocera_habitats_r,5)

#removing rows with zero total

tot_cladocera_habitats_r2<-tot_cladocera_habitats_r[rowSums(tot_cladocera_habitats_r) > 0,]

rowSums(tot_cladocera_habitats_r2)

colnames(tot_cladocera_habitats_r2)[1]="species"


## Beta diversity between the different types of habitats

cladocera_beta_r<-t(select(cladocera_habitats_nMDS_r,
                           pool_total_r:river_total_r))

clado_beta_r=vegdist(cladocera_beta_r,
                     index="jaccard")

plot_beta_r=hclust(clado_beta_r,
                   method="average")

#converting the plot to dendrogram via as.dendrogram

plot_beta_r2=as.dendrogram(plot_beta_r)

#plot

plot(plot_beta_r2,
     cex=1.1,
     main="Beta diversity between habitat types",ylab="Similarity",
     xlab=NULL,
     edgePar = list(col = c("steelblue"), 
                    lwd = 3,
                    lty=1),
     horiz=T)

####### using random dataset from the 156 samples data with env variables

#getting the necessary dataset

names(cladocera)

clado_env_data=subset(cladocera[1:156,])

clado_env_data2=dplyr::select(clado_env_data,
                              Habitat_type,
                              Aq_Veg,
                              Altitude,
                              Total_sp.:temp,
                              Salinity:C_sphaericus)

# Species data

clado_env_sp=select(clado_env_data2,
                    Habitat_type,
                    D_excisum:C_sphaericus)

# Environment data

clado_env_env=select(clado_env_data2, 
                     Habitat_type:Salinity)


##function for selecting the habitat types

sel_habitats=function(x,
                      habitats=c("pool","pond","lake","river")){
  for (i in 1:nrow(x)){
    filt=filter(x,
                Habitat_type==habitats)
    return(as.data.frame(filt))
  }
}


#making subsets habitatwise using the function

cladocera_pool_env=sel_habitats(clado_env_data2,
                                habitats="pool")%>%
  sample_n(17)%>%
  mutate(sample=sprintf("pools%d",
                        seq=(1:17)))%>%
  select(sample,
         everything())

cladocera_pond_env=sel_habitats(clado_env_data2,
                                habitats="pond") %>%
  sample_n(17)%>%
  mutate(sample=sprintf("ponds%d",
                        seq=(1:17)))%>%
  select(sample,
         everything()) 

cladocera_lake_env=sel_habitats(clado_env_data2,
                                habitats="lake") %>%
  sample_n(17)%>%
  mutate(sample=sprintf("lakes%d",
                        seq=(1:17)))%>%
  select(sample,
         everything())

cladocera_river_env=sel_habitats(clado_env_data2,
                                 habitats="river")%>%
  sample_n(17)%>%
  mutate(sample=sprintf("rivers%d",
                        seq=(1:17)))%>%
  select(sample,
         everything())

##Full_join of random subsamples of the dataset (for CCA)

clado_r_1_env=full_join(cladocera_pool_env,cladocera_pond_env)

clado_r_2_env=full_join(cladocera_lake_env,cladocera_river_env)

clado_env_data3=as.data.frame(full_join(clado_r_1_env,clado_r_2_env))

View(clado_env_data3)


#selecting the species whose total is more than 0

clado_comm_env=select(clado_env_data3,D_excisum:C_sphaericus)

clado_comm_env2=clado_comm_env[,colSums(clado_comm_env)>0]

View(clado_comm_env2)

#changing col name D.sarsi to D_sarsi to match with functional trait data

which(colnames(clado_comm_env2)=="D.sarsi")

colnames(clado_comm_env2)[2]<-"D_sarsi"

## Permanova of habitat types and species incidences 19-4-18 (using random samples from 156 sample dataset)
# dataset for the analysis

clado_occ=select(clado_random_data,
                 D_excisum:C_sphaericus)

clado_env=select(clado_random_data,
                 Habitat_type:Salinity)

# checking multivariate spread (condition for permanova) of the data

clad_vegdist<-vegdist(clado_occ,
                      method = "jaccard")

clado_disp=betadisper(clad_vegdist,
                      clado_env$Habitat_type)

anova(clado_disp)

plot(clado_disp)

permutest(clado_disp, pairwise = TRUE) 

boxplot(clado_disp, ylab = "Distance to centroid")

#PERMANOVA

clado_perm=adonis(clado_occ ~Habitat_type, 
                  data=clado_env, 
                  permutations=999,
                  method="jaccard")

clado_perm$aov.tab


#getting species names for matching

clado_random_sp_env=as.data.frame(colnames(clado_comm_env2))

#changing the col name to 'species' for match with functional traits

colnames(clado_random_sp_env)[1]<-"Species"

#join functional traits and species 

new_func_traits_env=semi_join(clado_func_traitr_r, 
                              clado_random_sp_env,
                              by="Species")

#converting community data into matrix

clado_fun_comm_env=as.matrix(clado_comm_env2)

View(clado_fun_comm_env)

clado_func_trait_env=as.matrix(new_func_traits_env)

View(clado_func_trait_env)

#converting the species column to row names

rownames(new_func_traits_env)<-new_func_traits_env[,1]

View(new_func_traits_env)

new_func_traits_env<-new_func_traits_env[,-1]

#functional diversity indices

clado_func_div_env=dbFD(new_func_traits_env,
                        clado_fun_comm_env,
                        calc.FRic=T,
                        calc.FDiv=T,
                        ord = "podani",
                        calc.FGR=T,
                        clust.type="average",
                        stand.FRic=T)

#Extracting the indices and adding habitat type factor to indices

funct_indices_r_env<-data.frame(Frich=clado_func_div_env$FRic,
                                Feve=clado_func_div_env$FEve,
                                Fdisp=clado_func_div_env$FDis,
                                RaoQ=clado_func_div_env$RaoQ)%>% mutate(habitat_type=clado_env_data3$Habitat_type)%>%
  select(habitat_type,everything())

#write.csv(funct_indices_r_env,"funcdiv_indices_random.csv")

View(funct_indices_r_env)

#functional composition

func_comp_clado_env=functcomp(new_func_traits_env,
                              clado_fun_comm_env, 
                              CWM.type = c("dom"), 
                              bin.num = NULL)  

View(func_comp_clado_env)

#adding habitat types to the above dataset

clado_func_comp_env=mutate(func_comp_clado_env,
                           habitat_type=clado_env_data3$Habitat_type)

levels(clado_func_comp_env$Presence_Ocellus)

#using unclass to convert cateogories to numerics

clad_env_fac=sapply(func_comp_clado_env,
                    is.factor)

clad_env_fac_val=sapply(clado_func_comp_env[clad_env_fac],
                        unclass)

clad_env_fac_val=as.data.frame(clad_env_fac_val)%>%
  select(-habitat_type)

names(clado_func_comp_env)

#adding habitat type to above dataframe of integers

clad_env_fun=mutate(clad_env_fac_val, 
                    Habitat_type=clado_env_data3$Habitat_type,
                    Body_length=clado_func_comp_env$MBL,
                    SWA=clado_func_comp_env$SWA_Length,
                    Egg_clutch=clado_func_comp_env$Average_egg_clutch, Eye_size=clado_func_comp_env$Eye_Size...)%>%
  select(Habitat_type,
         -Family,
         everything())

View(clad_env_fun)

# Renaming column vegetation

colnames(clado_env_data3)[2]<-"Aq_Veg"

#adding env data to above

clad_sel_env=select(clado_env_data3,
                    Aq_Veg:Salinity)

fin_comb=as.data.frame(cbind(clad_sel_env,
                             clad_env_fun))

colnames(fin_comb)[2]<-"Altitude"

fin_comb=fin_comb[c(7,1:6,9,11,13:16,18:21)]

head(fin_comb)

# converting final data into numerics

fin_comb_int=sapply(fin_comb,is.integer)

fin_comb[fin_comb_int]=lapply(fin_comb[fin_comb_int],as.numeric)

fin_comb$Aq_Veg=as.numeric(fin_comb$Aq_Veg)

#re-naming levels of habitat type

levels(fin_comb$Habitat_type)=c("Reservoirs",
                                "Ponds",
                                "Pools",
                                "Rivers")

#write.csv(fin_comb,"Functional_comp_clado.csv")

##PCA of the func traits and env data of the random dataset 20-4-18

clad_pca=rda(fin_comb[,-1],scale=T,na.omit=T)

colvec <- c("red2", "green4", "mediumblue","grey40")

plot(clad_pca, type = "n")

with(fin_comb, 
     points(clad_pca, 
            display = "sites", 
            col = colvec[Habitat_type],
            pch = 21, 
            bg = colvec[Habitat_type]))

text(clad_pca, 
     display = "species",
     cex = 1, 
     col = "darkcyan")

with(fin_comb, 
     legend("topright", 
            legend = levels(Habitat_type), 
            bty = "n",
            col = colvec, 
            pch = 21, 
            pt.bg = colvec))

## second type of plot for PCA

biplot(clad_pca,
       display = "species",
       lwd=2,
       cex=4,
       col="black")

points(clad_pca,
       display = "sites",
       col=colvec[fin_comb$Habitat_type],
       pch=20,
       cex=2)

with(fin_comb,
     legend("bottomright",
            legend = levels(Habitat_type), 
            bty = "n",
            col = colvec, 
            pch = 21, 
            pt.bg = colvec,
            cex=0.9))

## Final PCA using autoplot 23-4-18

plot_pca_clado=autoplot(prcomp(fin_comb[,-1],scale. = T),data = fin_comb,color=fin_comb$Habitat_type,loadings=T,loadings.colour = "red",loadings.label=T,size=3,loadings.label.size = 3,frame=F)
ggplotly(plot_pca_clado)
#scree plot using factoextra
clado_pca_scree=prcomp(fin_comb[,-1],scale. = T)
fviz_eig(clado_pca_scree,addlabels=TRUE, hjust = -0.3)+ylim(0,35)+theme_minimal()

#ggplots for functional diversity indices 21-4-18
edit(funct_indices_r_env)
str(funct_indices_r_env)

func_indices_plot=gather(funct_indices_r_env,Indices,Values,Frich:RaoQ)
names(func_indices_plot)
#re-ordering the levels of the factors for functional gradient
print(levels(func_indices_plot$habitat_type))
edit(func_indices_plot)
func_indices_plot$habitat_type = factor(func_indices_plot$habitat_type,levels(func_indices_plot$habitat_type)[c(3,2,1,4)])
#plot
gg_clado_r1=ggplot(func_indices_plot,aes(x=habitat_type,y=Values,fill=Indices))+geom_boxplot()+theme_bw(base_size = 16)
                   
gg_clado_r1


## function to show Rsquared on the ggplot
gplotRegression <- function (fit) {
                     require(ggplot2)
                     gplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
                       geom_point() +
                       stat_smooth(method = "lm", col = "red",se=TRUE) +                     labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),                                          "Intercept =",signif(fit$coef[[1]],5 ),                                          " Slope =",signif(fit$coef[[2]], 5),
                                        " P =",signif(summary(fit)$coef[2,4], 5)))
                   }
                   
                   # checking rate of increase in taxo_richness and func richness accross habitat types
                   #functional richness
                   mod_frich=lm(Frich~sample_n,data=funct_indices_r_env)
                   mod_frich
                   mod_frich_gg=ggplotRegression(mod_frich)
                   mod_frich_gg                   
                   #taxonomic richness
                   ##re-arranging the data for a size gradient
                   funct_indices_r_env$habitat_type=factor(funct_indices_r_env$habitat_type,levels(funct_indices_r_env$habitat_type)[c(3,2,1,4)])
                   clado_env_data3=mutate(clado_env_data3,sample_n=c(1:68))
                   mod_taxorich=lm(Total_sp.~sample_n,data=clado_env_data3)
                   summary(mod_taxorich)
                   mod_taxorich_gg=ggplotRegression(mod_taxorich)
                   mod_taxorich_gg
                   
                   
                   
                   ### functional diversity analysis with functional composition of each type of habitats
                   #pools
                   cladocera_pool_env=sel_habitats(clado_env_data2,habitats="pool")%>%
                     sample_n(17)%>%
                     mutate(sample=sprintf("pools%d",seq=(1:17)))%>%
                     select(sample,everything())
                   
                   #selecting the species whose total is more than 0
                   clado_comm_pool1=select(cladocera_pool_env,D_excisum:C_sphaericus)
                   clado_comm_pool2=clado_comm_pool1[,colSums(clado_comm_pool1)>0]
                   colSums(clado_comm_pool2)
                   
                   
                   #changing col name D.sarsi to D_sarsi to match with functional trait data
                   which(colnames(clado_comm_pool2)=="D.sarsi")
                   colnames(clado_comm_pool2)[1]<-"D_sarsi"
                   
                   #getting species names for matching
                   clado_random_sp_pool=as.data.frame(colnames(clado_comm_pool2))
                   #changing the col name to 'species' for match with functional traits
                   colnames(clado_random_sp_pool)[1]<-"Species"
                   #join functional traits and species 
                   new_func_traits_env_pool=semi_join(clado_func_traitr_r, clado_random_sp_pool,by="Species")
                   edit(new_func_traits_env_pool)
                   #converting community data into matrix
                   clado_fun_comm_env_pool=as.matrix(clado_comm_pool2)
                   edit(clado_fun_comm_env_pool)
                   clado_func_trait_env_pool=as.matrix(new_func_traits_env_pool)
                   ncol(clado_comm_pool2)
                   #converting the species column to row names
                   rownames(clado_func_trait_env_pool)<-clado_func_trait_env_pool[,1]
                   
                   clado_func_trait_env_pool<-clado_func_trait_env_pool[,-1]
                   
                   edit(clado_func_trait_env_pool)
                   #functional diversity indices
                   clado_func_div_env_pool=dbFD(clado_func_trait_env_pool,clado_fun_comm_env_pool,calc.FRic=T,calc.FDiv=T,ord = "podani",calc.FGR=T,clust.type="average",stand.FRic=T)
                   
                   
                   ## ponds
                   
                   cladocera_pond_env=sel_habitats(clado_env_data2,habitats="pond") %>%
                     sample_n(17)%>%
                     mutate(sample=sprintf("ponds%d",seq=(1:17)))%>%
                     select(sample,everything()) 
                   
                   #selecting the species whose total is more than 0
                   clado_comm_pond1=select(cladocera_pond_env,D_excisum:C_sphaericus)
                   clado_comm_pond2=clado_comm_pond1[,colSums(clado_comm_pond1)>0]
                   colSums(clado_comm_pond2)
                   
                   
                   #changing col name D.sarsi to D_sarsi to match with functional trait data
                   which(colnames(clado_comm_pond2)=="D.sarsi")
                   colnames(clado_comm_pond2)[1]<-"D_sarsi"
                   
                   #getting species names for matching
                   clado_random_sp_pond=as.data.frame(colnames(clado_comm_pond2))
                   #changing the col name to 'species' for match with functional traits
                   colnames(clado_random_sp_pond)[1]<-"Species"
                   #join functional traits and species 
                   new_func_traits_env_pond=semi_join(clado_func_traitr_r, clado_random_sp_pond,by="Species")
                   edit(new_func_traits_env_pond)
                   #converting community data into matrix
                   clado_fun_comm_env_pond=as.matrix(clado_comm_pond2)
                   edit(clado_fun_comm_env_pond)
                   clado_func_trait_env_pond=as.matrix(new_func_traits_env_pond)
                   ncol(clado_comm_pond2)
                   #converting the species column to row names
                   rownames(clado_func_trait_env_pond)<-clado_func_trait_env_pond[,1]
                   
                   clado_func_trait_env_pond<-clado_func_trait_env_pond[,-1]
                   
                   edit(clado_func_trait_env_pond)
                   #functional diversity indices
                   clado_func_div_env_pond=dbFD(clado_func_trait_env_pond,clado_fun_comm_env_pond,calc.FRic=T,calc.FDiv=T,ord = "podani",calc.FGR=T,clust.type="average",stand.FRic=T)
                   
                   
                   
                   ##lake
                   cladocera_lake_env=sel_habitats(clado_env_data2,habitats="lake") %>%
                     sample_n(17)%>%
                     mutate(sample=sprintf("lakes%d",seq=(1:17)))%>%
                     select(sample,everything())
                   
                   #selecting the species whose total is more than 0
                   clado_comm_lake1=select(cladocera_lake_env,D_excisum:C_sphaericus)
                   clado_comm_lake2=clado_comm_lake1[,colSums(clado_comm_lake1)>0]
                   colSums(clado_comm_lake2)
                   
                   
#changing col name D.sarsi to D_sarsi to match with functional trait data
 which(colnames(clado_comm_lake2)=="D.sarsi")
colnames(clado_comm_lake2)[2]<-"D_sarsi"
                   
   #getting species names for matching
 clado_random_sp_lake=as.data.frame(colnames(clado_comm_lake2))
#changing the col name to 'species' for match with functional traits
colnames(clado_random_sp_lake)[1]<-"Species"
#join functional traits and species 
 new_func_traits_env_lake=semi_join(clado_func_traitr_r, clado_random_sp_lake,by="Species")edit(new_func_traits_env_lake)
                   #converting community data into matrix
                   clado_fun_comm_env_lake=as.matrix(clado_comm_lake2)
                   edit(clado_fun_comm_env_lake)
                   clado_func_trait_env_lake=as.matrix(new_func_traits_env_lake)
                   ncol(clado_comm_lake2)
                   #converting the species column to row names
                   rownames(clado_func_trait_env_lake)<-clado_func_trait_env_lake[,1]
                   
                   clado_func_trait_env_lake<-clado_func_trait_env_lake[,-1]
                   
                   edit(clado_func_trait_env_lake)
                   #functional diversity indices
                   clado_func_div_env_lake=dbFD(clado_func_trait_env_lake,clado_fun_comm_env_lake,calc.FRic=T,calc.FDiv=T,ord = "podani",calc.FGR=T,clust.type="average",stand.FRic=T)
                   
                   ##river
                   
                   cladocera_river_env=sel_habitats(clado_env_data2,habitats="river")%>%
                     sample_n(17)%>%
                     mutate(sample=sprintf("rivers%d",seq=(1:17)))%>%
                     select(sample,everything())
                   
                   #selecting the species whose total is more than 0
                   clado_comm_river1=select(cladocera_river_env,D_excisum:C_sphaericus)
                   clado_comm_river2=clado_comm_river1[,colSums(clado_comm_river1)>0]
                   colSums(clado_comm_river2)
                   
                   
                   #changing col name D.sarsi to D_sarsi to match with functional trait data
                   which(colnames(clado_comm_river2)=="D.sarsi")
                   colnames(clado_comm_river2)[1]<-"D_sarsi"
                   
                   #getting species names for matching
                   clado_random_sp_river=as.data.frame(colnames(clado_comm_river2))
                   #changing the col name to 'species' for match with functional traits
                   colnames(clado_random_sp_river)[1]<-"Species"
                   #join functional traits and species 
                   new_func_traits_env_river=semi_join(clado_func_traitr_r, clado_random_sp_river,by="Species")
                   edit(new_func_traits_env_river)
                   #converting community data into matrix
                   clado_fun_comm_env_river=as.matrix(clado_comm_river2)
                   edit(clado_fun_comm_env_river)
                   clado_func_trait_env_river=as.matrix(new_func_traits_env_river)
                   ncol(clado_comm_river2)
                   #converting the species column to row names
                   rownames(clado_func_trait_env_river)<-clado_func_trait_env_river[,1]
                   
                   clado_func_trait_env_river<-clado_func_trait_env_river[,-1]
                   
                   edit(clado_func_trait_env_river)
                   #functional diversity indices
                   clado_func_div_env_river=dbFD(clado_func_trait_env_river,clado_fun_comm_env_river,calc.FRic=T,calc.FDiv=T,ord = "podani",calc.FGR=T,clust.type="average",stand.FRic=T)
                   
                   
                   #### RLQ and fourth corner analysis                                 
                   ## species data
                   
                   clad_sp_RLQ=data.frame(cbind(sample=clado_env_data3$sample,clado_comm_env2))
                   rownames(clad_sp_RLQ)<-clad_sp_RLQ[,1]
                   clad_sp_RLQ[1] <- NULL
                   ncol(clad_sp_RLQ)
                   ### environmental data
                   clado_env_RLQ=data.frame(cbind(sample=clado_env_data3$sample,cca_clado_env))
                   edit(clado_env_RLQ)
                   rownames(clado_env_RLQ)<-clado_env_RLQ[,1]
                   clado_env_RLQ[1] <- NULL  
                   names(clado_env_RLQ)
                   clado_env_RLQ=clado_env_RLQ[,-3]
                   edit(clado_env_RLQ)
                   ### functional trait data
                   # original data
                   clado_func_trait_env=as.data.frame(new_func_traits_env)
           
                         nrow(clado_func_trait_env)
                   # conversion into dataframe
                   names(clado_func_trait_env)
                   clado_trait_RLQ=as.data.frame(select(clado_func_trait_env,Species,Feeding_type:Presence_Ocellus,Color:Trophic_Regime))
                   rownames(clado_trait_RLQ)=clado_trait_RLQ$Species
                   clado_trait_RLQ[1] <- NULL
                   names(clado_trait_RLQ)
                   #unclass the factors to convert them into integers
                   clad_RLQ1_fac=sapply(clado_trait_RLQ,is.factor)
                   clad_trait_RLQ=sapply(clado_trait_RLQ[clad_RLQ1_fac],unclass)
                   clad_trait_RLQ=cbind(clad_trait_RLQ,clado_trait_RLQ$Average_egg_clutch,clado_trait_RLQ$MBL,clado_trait_RLQ$SWA_Length,clado_trait_RLQ$Eye_Size...)
                   edit(clad_trait_RLQ)
                   #assigning rownames to the data
                   rownames(clad_trait_RLQ)=clado_trait_RLQ$Species
                   
                   clad_trait_RLQ[1] <- NULL
                   #RLQ analysis
                   #Species
                   species_RLQ_clado <- dudi.coa(clad_sp_RLQ, scannf = FALSE)
                   species_RLQ_clado$lw
                   #environment
                   env_RLQ_clado <- dudi.hillsmith(clado_env_RLQ,scannf = FALSE,row.w = species_RLQ_clado$lw)
                   #traits
                   traits_RLQ_clado <- dudi.pca(clad_trait_RLQ,scannf = FALSE,row.w=species_RLQ_clado$cw)
                   
                   #RLQ analysis
                   rlq_clado <- rlq(env_RLQ_clado, species_RLQ_clado, traits_RLQ_clado,
                                    scannf = FALSE)
                   plot(rlq_clado,boxes=F)
                   
                   par(mfrow = c(3,1))
                   s.arrow(rlq_clado$l1)
                   s.arrow(rlq_clado$c1)
                   s.label(rlq_clado$lQ)
                   
                   summary()
                   
                   #fourth corner analysis
                   fourth_corner_clado=fourthcorner(clado_env_RLQ, clad_sp_RLQ,
                                                    clad_trait_RLQ, modeltype = 6, p.adjust.method.G = "none",
                                                    p.adjust.method.D = "none",nrepet = 999)
                   plot(fourth_corner_clado,stat = "D",alpha=0.08)
                   
                   
## Plot for Indialona 14-5-18 using the 156 samples envrionmental dataset
names(clado_env_data) 
clado_indialona=select(clado_env_data,Habitat_type,I_ganapati)
str(clado_indialona)
names(clado_indialona)

#aggregating data w.r.t habitat_types
indialona_data <-as.data.frame(aggregate(clado_indialona$I_ganapati, by=list(clado_indialona$Habitat_type), FUN=sum, na.rm=TRUE))
colnames(indialona_data)=c("Habitat_type","Occurrence")
edit(indialona_data)
indialona_data$Occurrence[[2]]=0
#re-ordering the factors
levels(indialona_data$Habitat_type)
indialona_data$Habitat_type=factor(indialona_data$Habitat_type,levels(indialona_data$Habitat_type)[c(3,2,1,4)])

#plot
indialona_plot=ggplot(indialona_data,aes(x=Habitat_type,y=Occurrence))+geom_bar(fill="steelblue",stat='identity',width=0.7,position=position_dodge())
indialona_plot+theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme_bw(base_size = 18)+scale_x_discrete(labels=c("pool" = "Pools", "pond" = "Ponds","lake" = "Reservoirs","river"="Rivers")) +xlab("Types of Habitats (Pune region)")+ylab("Occurrence (out of 156 samples)")
                
#functional richness and dispersion plot using data summary 17-5-18
#functional richness
names(funct_indices_r_env)

summ_Frich=describeBy(funct_indices_r_env$Frich,funct_indices_r_env$habitat_type, na.rm = T,mat=T)
View(summ_Frich)

scatter_Frich=ggplot(summ_Frich, aes(x=item, y=mean,color='#E69F00')) + geom_point(size=8,position = pos,color="black",pch=21,fill='#E69F00',stroke=1)+geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.1,position=pos,color='#999999',size=1.5)

scatter_Frich

final_Frich_plot=scatter_Frich+theme_bw(base_size = 16)+theme_bw(base_size = 17)+theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Habitat type")+ylab("Functional richness")+scale_x_discrete(labels=c("1" = "Pools", "2" = "Ponds","3" = "Reservoirs","4"="Rivers"))

final_Frich_plot

ggplotly(final_Frich_plot)


#functional dispersion

summ_Fdis=describeBy(funct_indices_r_env$Fdisp,funct_indices_r_env$habitat_type, na.rm = T,mat=T)

scatter_Fdis=ggplot(summ_Fdis, aes(x=item, y=mean,color='#E69F00')) + geom_point(size=6,position = pos,color="black",pch=21,fill='#E69F00',stroke=1)+geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=.2,position=pos,color='#999999',size=1)

scatter_Fdis

final_Fdis_plot=scatter_Fdis+theme_bw(base_size = 15)+theme_bw(base_size = 17)+theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Habitat type")+ylab("Functional dispersion")+scale_x_discrete(labels=c("1" = "Pools", "2" = "Ponds","3" = "Reservoirs","4"="Rivers"))+ylab(0,0.30)

final_Fdis_plot 

edit(clado_func_comp_env)
names(clado_func_comp_env)
table(clado_func_comp_env$habitat_type,clado_func_comp_env$Average_egg_clutch)


## OPTIONAL proportions for plot and finding the ratio of Chydoridae to Daphniidae accross the regions
#selecting Chydorid and Daphniid data and converting into % and rounding the values
clado_plot_reg1 = t(clado_plot_region_prop[2:3,])
#renaming the columns
colnames(clado_plot_reg1) = clado_plot_reg1[1,]
clado_plot_reg1 = clado_plot_reg1[-1,]
View(clado_plot_reg1)
row.names(clado_plot_reg1) <- NULL
clado_plot_reg2 = as.data.frame(cbind(clado_plot_reg1, Region = colnames(clado_plot_region_prop[, -1]))) %>%
    select(Region, everything())
View(clado_plot_reg2)
#converting the factors and numerical data
clado_plot_reg2$Region = as.factor(clado_plot_reg2$Region)
clado_plot_reg2$Chydoridae = as.numeric(as.character(clado_plot_reg2$Chydoridae))
clado_plot_reg2$Daphniidae = as.numeric(as.character(clado_plot_reg2$Daphniidae))
#re-ordering the factors
clado_plot_reg2$Region = factor(clado_plot_reg2$Region, levels(clado_plot_reg2$Region)[c(2, 3, 1, 6, 4, 5)])
levels(clado_plot_reg2$Region)
#getting the ratio
clado_plot_reg2 = mutate(clado_plot_reg2, Ratio = Chydoridae / Daphniidae)
View(clado_plot_reg2)
#plot
ggplot(clado_plot_reg2, aes(x = Region, y = Ratio)) + geom_point(size = 5, color = "steelblue", fill = "black") + ylim(0, 8) + theme_bw(base_size = 19) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Regions") + ylab("Ratio(Chydoridae/Daphniidae)") + scale_x_discrete(labels = c("Jammnu & Kashmir(India)", "Nepal", "Assam(India)", "Thailand", "NWG", "SriLanka"))

#plot using total species
# making custom color palette
colourCount = length(unique(clado_plot_region_3$Family))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
#plot (gaps between the bars adjusted using width argument)
cladocera_plot2 = ggplot(clado_plot_region_3, aes(x = Region, y = Species, fill = Family)) + geom_bar(stat = 'identity', width = 0.7, position = position_dodge())
cladocera_plot2 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme_bw(base_size = 17) + scale_fill_manual(values = getPalette(colourCount))
View(clado_plot_region_3)
