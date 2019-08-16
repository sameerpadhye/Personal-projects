#Variation partitioning to assess the contribution of envrionmental and spatial factors in species distribution

#libraries used

library(vegan)
library(readxl)
library(tidyverse)
library(magrittr)

#Here the mite data (mite, mite.env and mite.xy) has been used for species community, environmental and spatial data 

#1. Species

data("mite")

species_data<-mite


#2. Environment

data("mite.env")

environ_data<-mite.env


#3. Spatial variables

data("mite.xy")

spatial_data<-mite.xy


# Converting the species data into a hellinger distance

hell_data<-vegan::decostand(species_data,method = 'hellinger')


# Converting the spatial data into PCNM vectors (using vegan) using PCNM analysis

pcnm_data<-spatial_data%>%
  dist(.)%>%
  vegan::pcnm(.)%>%
  magrittr::use_series(vectors)%>% # for selecting the vectors
  data.frame(.)


# Running RDA's to obtain significant environmental variables and spatial vectors to be used for variation partitioning

#1. rda of species vs environment with just intercept

rda_env_intercept<-rda(hell_data~1,
                       environ_data)

#2. rda of species vs env with all variables

rda_env_all<-rda(hell_data~.,
             environ_data)

#3. using a forward selection of env variables

rda_env_step.forward<-ordistep(rda_env_intercept,
                               scope=formula(rda_env_all),
                               direction='forward',
                               perm.max=999,
                               pstep=999)

#4. run a rda of the selected env variables

rda_env_final<-rda(hell_data ~ WatrCont + Topo + SubsDens + Shrub + Substrate,
                   data=environ_data)

#5. run a vif.cca to assess the relative importance of each env variable

vif.cca(rda_env_final)


#### Similarly, RDA is run for the spatial variables

#1. rda of species vs environment with just intercept

rda_spatial_intercept<-rda(hell_data~1,
                           pcnm_data)

#2. rda of species vs env with all variables

rda_spatial_all<-rda(hell_data~.,
                     pcnm_data)

#3. using a forward selection of env variables

rda_spatial_step.forward<-ordistep(rda_spatial_intercept,
                               scope=formula(rda_spatial_all),
                               direction='forward',
                               perm.max=999,
                               pstep=999)

#4. run a rda of the selected env variables

rda_spatial_final<-rda(hell_data ~ PCNM2 + PCNM3 + PCNM8 + PCNM1 + PCNM6 + PCNM4 + PCNM37 + PCNM16 + PCNM9 + PCNM7 + PCNM10 + PCNM11 + PCNM20 + PCNM23 + PCNM5,
                   data=pcnm_data)


# Significant environmental and spatial variables obtained are then extracted into two separate datasets


#Function to obtain the names of the variables selected after forward selection (input here would be the rda$call object)

rda_names_extractor<-function(x){
  
  if(!require(tidyverse))install.packages('tidyverse')
  library(tidyverse)
  
  name_vec<-as.character(x)%>%
    paste(.,collapse = ",")%>%
    str_split(.,"[~,]")%>%
    unlist()%>%
    str_subset(.,"\\+")%>%
    gsub(" ","",.)%>%
    sapply(.,strsplit,"\\+",
           USE.NAMES = FALSE)%>%
    unlist(.)
  
  return(name_vec)
}


#1. env variables

rda_env_step.forward$call 

#a. extraction by conventional method

environ_final_data<-environ_data%>%
  dplyr::select(WatrCont,
                Topo,
                SubsDens,
                Shrub,
                Substrate)

#b. extraction using the function

env_var_final<-rda_names_extractor(rda_spatial_step.forward$call)

environ_final_data<-environ_data%>%
  select(., .dots =env_var_final)


#2. Spatial data

rda_spatial_step.forward$call

#a. extraction by conventional method

spatial_final_data<-pcnm_data%>%
  dplyr::select(PCNM4,
                PCNM37,
                PCNM16,
                PCNM9,
                PCNM7,
                PCNM10,
                PCNM11,
                PCNM20,
                PCNM23,
                PCNM5)

#b. extraction using the function

spatial_var_final<-rda_names_extractor(rda_spatial_step.forward$call)

spatial_final_data<-pcnm_data%>%
  select(.,.dots=spatial_var_final)
         

# A variation partitoning algorithm is then run using these two groups along with the species composition

var_part_data<-varpart(hell_data,
                       environ_final_data,
                       spatial_final_data)

#plot the above to see the venn diagrams

plot(var_part_data)

# ANOVA's are then performed to check significance of a.environment, b.space and c. environment + space using anova.cca in vegan package

#1. anova of species with env var (a+b)

anova.cca(rda(hell_data,
              environ_final_data),
          step=1000)

#2. anova of species wit spatial var (b+c)

anova.cca(rda(hell_data,
              spatial_final_data),
          step=1000)

#3. anova of all the vars combined (a+b+c)

env_spatial_data<-cbind(environ_final_data,
                        spatial_final_data)

anova.cca(rda(hell_data,
              env_spatial_data),
          step=1000)

#4. anova of component a (which is species, env and space)

anova.cca(rda(hell_data,
              environ_final_data,
              spatial_final_data),
          step=1000)

#5. anova of component c (which is species, space and env)

anova.cca(rda(hell_data,
              spatial_final_data,
              environ_final_data),
          step=1000)
