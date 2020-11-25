library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(psych)
library(readxl)
library(vegan)
library(betapart)
library(psych)



## plotting the MBL per geographic region (latitude gradient)

## importing the GIS data

regional_spp = read_excel("C:/Data/Research data/Cladocera/Cladocera distribution/All data.xlsx", sheet = 3)

# converting to data.frame

regional_spp = as.data.frame(regional_spp)

#removing Thailand from the dataset

regional_sp2 = select(regional_spp, Species:Assam, Rajasthan:MBL)

regional_sp2$Family = as.factor(regional_sp2$Family)

#removing all the rows with 0 rowsum

regional_spn = regional_sp2[rowSums(regional_sp2[, (3:10)]) != 0,]

##removing L_kindtii since its a very big species 

which(regional_spn == "L_kindtii")

regional_spn = regional_spn[-112,]

## sample number of species per family for all regions

#1. checking the least number of incidences amongst the regions
colSums(regional_spn[, c(3:10)])
View(regional_spn)

#2. Since its Kashmir with 30 species,get 25 randomly selected species per region which are always present(i.e having '1')

step1 = select(regional_spn, Species, NWG, MBL) %>%
    mutate_at(vars(NWG), funs(. * MBL)) %>%
    select(-MBL) %>%
    as.data.frame() %>%
    subset(NWG > 0) %>%
    sample_n(25, replace = T)

##since the above must be repeated 6 times, a function has been written

#function to get the 25 present species from the dataset
reg_sp_sel= function(x, loc = c("NWG", "Sri_lanka","Tamil_Nadu","West_Bengal" ,"Assam", "Nepal", "Rajasthan", "Kashmir")) {
    spp = select(x, Species, loc, MBL) %>%
    mutate_at(vars(loc), funs(. * MBL)) %>%
    select(-MBL) %>%
    as.data.frame()
    stp1 = apply(spp != 0, 1, all)##removing all zeros from the data
    stp2 = spp[stp1,] ##removing all zeros from the data
    stp3 = replicate(25, sample(stp2[, 2], 25, replace = T))
    col_names_stp3 = c(paste0(loc,as.character(1:25)))##to give sequencial numbers of each of 25 replicates
    colnames(stp3)=col_names_stp3
        return(stp3)
}

# dataset using the above function
NWG = reg_sp_sel(regional_spn, loc = "NWG")
Sri_Lanka = reg_sp_sel(regional_spn, loc = "Sri_lanka")
Assam= reg_sp_sel(regional_spn, loc = "Assam")
Nepal = reg_sp_sel(regional_spn, loc = "Nepal")
Rajasthan = reg_sp_sel(regional_spn, loc = "Rajasthan")
Kashmir = reg_sp_sel(regional_spn, loc = "Kashmir")
Tamil_Nadu = reg_sp_sel(regional_spn, loc = "Tamil_Nadu")
West_Bengal = reg_sp_sel(regional_spn, loc = "West_Bengal")
sp_mbl = as.data.frame(cbind(Sri_Lanka,Tamil_Nadu,NWG,West_Bengal,Assam,Nepal,Rajasthan, Kashmir))

# conversion into long format from wide
regional_mbl_plot = gather(sp_mbl, Regions, MBL) %>% mutate(countries = rep(c("Sri_Lanka", "Tamil_Nadu","NWG","West_Bengal","Assam", "Rajasthan", "Nepal", "Kashmir"), each =  25 * 25))
                             
# countries into factor
regional_mbl_plot$countries= factor(regional_mbl_plot$countries, levels = c("Sri_Lanka", "Tamil_Nadu","NWG","West_Bengal","Assam", "Rajasthan", "Nepal", "Kashmir"))

class(regional_mbl_plot$countries)
library(psych)



#Summarizing the 'mbl'per region of the above dataset
reg_sp_avg = describeBy(regional_mbl_plot$MBL, regional_mbl_plot$countries, na.rm = T, mat = T)

View(reg_sp_avg)
#plot for above
reg_sp_plot = ggplot(reg_sp_avg, aes(x = as.factor(group1), y = mean)) + geom_point(size = 8, position = position_dodge(0.5), color = "black", pch = 21, fill = 'steelblue', stroke = 1) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1,size=1, position = position_dodge(0.5), color = 'black') + scale_x_discrete(name = "Regions", limits = c("Sri_Lanka", "Tamil_Nadu","NWG","West_Bengal","Assam", "Rajasthan", "Nepal", "Kashmir")) + theme_bw(base_size = 20)+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_vline(xintercept = 6.5, color = "black", size = 1.5,linetype = "dotted",alpha=0.7) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ylab("Mean Body size(mm)")
reg_sp_plot

#plot2 (to check the scatter of mbl for all regions)

names(regional_mbl_plot)
ggplot(regional_mbl_plot, aes(x = Regions, y = MBL)) + geom_point(fill = "steelblue") + ylim(0, 4.5) + theme_bw(base_size = 19) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_smooth(method = lm)

#####  MBL per group formed in the cluster

# adding the grouping factor
regional_mbl_plot = mutate(regional_mbl_plot, group = as.factor(rep(c("A", "B"), times = c(3750, 1250))))

#since the number of observations for both groups is unequal, 1250 random observations selected from the group A

reg_mbl_aov_A = filter(regional_mbl_plot, group == "A") %>%
    sample_n(1250, replace = T)
# combining the 1250 observations of group B with newly formed group A

reg_mbl_aov = rbind(reg_mbl_aov_A, subset(regional_mbl_plot, group=="B"))

#Summarizing the 'mbl'per group

reg_sp_avg_grp = describeBy(reg_mbl_aov$MBL, reg_mbl_aov$group, na.rm = T, mat = T)

table(reg_mbl_aov$countries, reg_mbl_aov$group)
# checking statistical significance between body sizes per regions
library(lmPerm)
aov_regional_MBL = aovp(log10(MBL) ~ group, perm = "Exact", reg_mbl_aov)
summary(aov_regional_MBL)

# to check the familywise occurrence of species in different regions

#1. converting 'Family' column to a factor
regional_sp2$Family = as.factor(regional_sp2$Family)

#aggregating the incidences w.r.t. families
reg_agg <- aggregate(regional_sp2[, c(3:10)], by = list(regional_sp2$Family), FUN = sum, na.rm = TRUE)
View(reg_agg)

###########BETA DIVERSITY & ONE-WAY PERMANOVA #######################
install.packages("ggdendro")
install.packages('ggfortify')

library(vegan)
library(readxl)
library(ggdendro)
library(ggfortify)

#data with thailand and species with zero rowsums removed

regional_spn = regional_sp2[rowSums(regional_sp2[, (3:10)]) != 0,]
colnames(regional_spn)
reg_sp_mds2 = as.data.frame(t(regional_spn[, c(3:10)]))
View(reg_sp_mds2)
colnames(reg_sp_mds2) = regional_spn$Species

# to check how many clusters are formed for grouping

reg_sp_mds3 = vegdist(reg_sp_mds2, method = "jaccard",binary = T)

plot_beta_region = hclust(reg_sp_mds3, method = "average")

plot(plot_beta_region)

#adding the group factor to the data
reg_sp_mds2$Group = as.factor(c("A","A" ,"A","B", "A","B","A","A"))

View(reg_sp_mds2)

# checking multivariate spread (condition for permanova) of the data

clad_reg_mdsdis = betadisper(reg_sp_mds3, reg_sp_mds2$Group)

anova(clad_reg_mdsdis)

## the test is not significant hence the null hypothesis cannot be rejected therefore the data are not dispersed homogenously

boxplot(clad_reg_mdsdis, ylab = "Distance to centroid")

#Visualizing the clusters

plot_beta_reg_mds = hclust(reg_sp_mds3, method = "average")

plot_beta_regionn= as.dendrogram(plot_beta_region)

plot(plot_beta_regionn, cex = 1.1, main = "Similarity between regions of Indian subcontinent", ylab = "Similarity", xlab = NULL, edgePar = list(col = c("steelblue"), lwd = 3, lty = 1), horiz = T)

## bootstrapping
# using reg_sp_mds3 data calculated above

# bootstrapping packages
install.packages("fpc")
library(fpc)

#visualizing number of groups

plot_beta_region = hclust(reg_sp_mds3, method = "average")

#to see group assignment
cutree(plot_beta_region, k = 2)

rect.hclust(plot_beta_region, k = 2)

#bootstrapping to check the validity of the groups

clusterboot(reg_sp_mds3, B = 999, distances = T, bootmethod = "boot", clustermethod = disthclustCBI, k = 2, method = "average")

#######################################################################