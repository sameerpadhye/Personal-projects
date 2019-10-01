#Canonical Correspondence Analysis (CCA)

#libraries used

library(tidyverse)
library(vegan)


#Here the BCI data (BCI and BCI.env) has been used for species community, environmental from the vegan package

#1. Species

data("BCI")

species_data<-BCI


#2. Environment

data("BCI.env")

environ_data<-BCI.env


# Transforming the species data via log (x+1) transformation (This is optional and completely depends on the user data). Presence/absence data can also be used for this analysis. This transformation is carried out using 'decostand' function

species_data_log<-vegan::decostand(species_data,'log')


# Running CCA's to obtain significant environmental variables 

#1. rda of species vs environment with just intercept

CCA_intercept<-cca(species_data_log~1,
                       environ_data,
                   scale=TRUE)


#2. CCA of species with all environmenral variables

CCA_all<-rda(species_data_log~.,
                 environ_data,
             scale = TRUE)


#3. Using a forward selection procedure to obtain significant environmental variables

cca_step_forward<-ordistep(CCA_intercept,
                               scope=formula(CCA_all),
                               direction='forward',
                               perm.max=999,
                               pstep=999)


#To view the final selected environmental variables

cca_step_forward$call

#4. Running a CCA using the selected environmental variables obtained from the forward selection above (the formula from cca_step_forward$call is used below)

CCA_final<-cca(species_data_log ~ UTM.EW + Habitat + UTM.NS + 
                   Stream, 
               data = environ_data,
               scale=TRUE)


#5. Running a vif.cca to assess collinearity of the environmental variables in the final model run above (variables with values more than 10 are supposedly collinear)

vif.cca(CCA_final)


# Significance of the CCA model to assess the significance of the model

anova.cca(CCA_final)


# Testing the significance of terms (environmental variables)

anova.cca(CCA_final, 
          by="terms")

# Testing the significance of CCA axes

anova.cca(CCA_final, 
          by="axis")


##CCA plots

# Plot of species vs Environment RDA (rda_env_final)

#base plot with all the information (sites, species and environment)

plot(CCA_final, 
     display=c("sp","cn","wa"),
     main= "CCA plot")


#Screeplot of CCA axes

screeplot(CCA_final)


#Only sites (points)

plot(CCA_final,
     display=c("sites"),
     type = c("points"),
     main="CCA plot") # type = c("text") can also be used; default is both

#Adding text

text(CCA_final,
     cex = 0.8,
     col = "forestgreen")


#Only species (points)

plot(CCA_final,
     display=c("species"),
     main="CCA plot")

#Text can be added similarly as above

#Other plot arguments can be modified as per user specifications

