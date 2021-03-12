#Data for analysis

library(readxl)

library(tidyverse)

#install.packages('fuzzyjoin')

library(fuzzyjoin)

fuzzy_data<-read_excel("C:/Users/samee/Downloads/Compiled IAS lists.xlsx",sheet=2)

# Khuroo<-read_excel("C:/Users/samee/Downloads/Compiled IAS lists.xlsx",sheet=3)
#    
#  # mutate(species_names=iconv(Species, "latin1", "latin1"))%>%
#  #    drop_na(.)%>%
#  #    select(species_names)
# 
# Envis<-read_excel("C:/Users/samee/Downloads/Compiled IAS lists.xlsx",sheet=4)
# 
# # mutate(species_names=iconv(Species, "latin1", "latin1"))%>%
# #     drop_na(.)%>%
# #     select(species_names)
# 
# Deshpande<-read_excel("C:/Users/samee/Downloads/Compiled IAS lists.xlsx",sheet=5)
# 
# # mutate(species_names=iconv(Species, "latin1", "latin1"))%>%
# #     drop_na(.)%>%
# #     select(species_names)
# 
# Ghate<-read_excel("C:/Users/samee/Downloads/Compiled IAS lists.xlsx",sheet=6)
# 
# # mutate(species_names=iconv(Species, "latin1", "latin1"))%>%
# #     drop_na(.)%>%
# #     select(species_names)
# 
# Puri_Mahajan<-read_excel("C:/Users/samee/Downloads/Compiled IAS lists.xlsx",sheet=7)
# 
# # mutate(species_names=iconv(Species, "latin1", "latin1"))%>%
# #     drop_na(.)%>%
# #     select(species_names)
# 
# View(Puri_Mahajan)
    
trial_1<-Khuroo %>%
    dplyr::select(Species)%>%
         stringdist_left_join(Envis, 
                          method = "soundex", 
                          by="Species")%>%
    dplyr::rename("Species"="Species.y",
                  "ori_sp_khuroo"="Species.x")%>%
    stringdist_left_join(Deshpande, 
                        method = "soundex", 
                        by="Species")%>%
    dplyr::rename("Species"="Species.y",
                  "ori_sp_deshpande"="Species.x")%>%
    stringdist_left_join(Ghate, 
                         method = "soundex", 
                         by="Species")%>%
    dplyr::rename("Species"="Species.y",
                  "ori_sp_ghate"="Species.x")%>%
    stringdist_left_join(Puri_Mahajan, 
                         method = "soundex", 
                         by="Species")%>%
    dplyr::rename("Species"="Species.y",
                  "ori_sp_puri_mahajan"="Species.x")
    drop_na(.)

View(unique(trial_1$Species))

# Using the single dataframe 

colnames(fuzzy_data)[5]<-"Puri_Mahajan_1960"

# Converting nonascii characters to ascii characters

fuzzy_data<-apply(fuzzy_data,2,
                  function (x) iconv(x, "latin1", "latin1"))%>%
  data.frame(.)

trial_2<-fuzzy_data%>%
    dplyr::select(Khuroo_2012)%>%
    stringdist_left_join(fuzzy_data%>%dplyr::select(ENVIS_2008), 
                         method = "soundex", 
                         ignore_case=TRUE,
                         by=c(Khuroo_2012 = "ENVIS_2008"))%>%
    stringdist_left_join(fuzzy_data%>%dplyr::select(Deshpande_1993), 
                         method = "soundex", 
                         ignore_case=TRUE,
                         by=c(ENVIS_2008 = "Deshpande_1993"))%>%
    stringdist_left_join(fuzzy_data%>%dplyr::select(Ghate_1991), 
                         method = "soundex", 
                         ignore_case=TRUE,
                         by=c(Deshpande_1993 = "Ghate_1991"))%>%
    stringdist_left_join(fuzzy_data%>%dplyr::select(Puri_Mahajan_1960),
                         method="soundex",
                         ignore_case=TRUE,
                         by=c(Ghate_1991 = "Puri_Mahajan_1960"))
    drop_na(.)%>%
  dplyr::rename("final_sp_list"="Puri_Mahajan_1960")%>%
    distinct(final_sp_list)

View(trial_2)

trial_3<-fuzzy_data%>%
  select(Khuroo_2012)%>%
  stringdist_left_join(fuzzy_data%>%select(ENVIS_2008), 
                       method = "soundex", 
                       ignore_case=TRUE,
                       by=c(Khuroo_2012 = "ENVIS_2008"))
  


View(trial_3)

trial_4<-iconv(fuzzy_data$Khuroo_2012, "latin1", "latin1",mark = TRUE)

View(trial_4)

# getting the encoding of the character data

Encoding(fuzzy_data$Khuroo_2012)

Encoding(fuzzy_data$Khuroo_2012)%>%unique(.)

Encoding(enc2native(fuzzy_data$Puri_Mahajan_1960))

# Converting the encoding
library(magrittr)

Encoding(fuzzy_data$Khuroo_2012) <- "latin1"

trial_5<-apply(fuzzy_data,2,function (x) Encoding(x)<-"latin1")

View(fuzzy_data)
# using combn

combn(colnames(fuzzy_data),
      2,simplify = T)

stringdist_join(x,
y,
by = NULL,
max_dist = 2,
method = "soundex",
mode = "inner",
ignore_case = FALSE
)

agrep(as.character(fuzzy_data$Khuroo_2012[1]), 
      as.character(fuzzy_data$ENVIS_2008[1]), 
      ignore.case = FALSE,
      value = TRUE,
      max.distance = 1)

agrep("sam", "same")

agrep("sam", c("SAME", "Sa", "SAMple"), max = 2, ignore.case = TRUE)


install.packages('tabulizer')
library(tabulizer)

remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))

# Location of WARN notice pdf file

location <- "C:/Research_data/References/Ecology related papers/Cottenie_etal._2003_Ecology.pdf"

# Extract the table

out <- extract_tables(location)
out[1]
