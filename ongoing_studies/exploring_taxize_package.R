install.packages('taxize')
library(taxize)
library(tidyverse)

bold_search(name="Cyclestheria hislopi")

classification('Cyclestheria', db = 'pow')

col_children(name="Leptestheria")


col_downstream(name="Eulimnadia", downto="species")


col_search(name=c("Daphnia","Ceriodaphnia"))


eubon_search("Daphnia")


get_wormsid("Daphnia", 'scientific')


require(taxize)

class_0<-gnr_resolve("Streptocephalus dichotomus")


class_1<-classification("Streptocephalus dichotomus",db='eol')

class_2<-tax_name(query = "Streptocephalus dichotomus", get = "family", db = "ncbi")

class_2$family

## GBIF ID

class_3<-get_gbifid("Streptocephalus dichotomus")

class_3[1]

## EOL ID

class_4<-get_eolid("Streptocephalus dichotomus")

class_4

## 

class_5<-get_iucn("Streptocephalus dichotomus")

class_5

class_6<-get_tsn("Helianthus annuus",accepted = FALSE)

class_6

length(lapply(class_6,itis_acceptname))

synonyms("Artemia salina",db = "gnr")


get_ids("Artemia salina",db=c('itis', 'ncbi','eol'))


class_7<-get_uid("Artemia salina")%>%
    classification(.,rank_filter="phylum")%>%
    head(.)%>%
    data.frame(.)%>%
    dplyr::select(ends_with("rank"),
                  ends_with("name"))%>%
    dplyr::filter(.[[1]] %in% c("kingdom",
                                "phylum",
                                "subphylum",
                                "subclass",
                                "order",
                                "family"))%>%
    dplyr::rename("Rank"=1,
                  "Name"=2)

class_7


gnr_resolve("Leptestheria nobilis")
get_uid("Leptestheria nobilis")

?classification
class_8<-classification("Leptestheria nobilis",db=c("gbif"))
class_8$`Leptestheria nobilis`
species_map_data[which(species_map_data$species_name=='Branchinella_maduraensis'),"species_name"]<-'Branchinella_maduraiensis'

View(species_map_data)
 
which(species_map_data$species_name=='Branchinella_maduraensis')

species_list<-str_replace(unique(species_map_data$species_name),"_"," ")%>%
    sort(.)

species_list[3]




#get_uid(x,key = '49393c73c0644a20c0ac4d9c53243a074a08')
hierarchy_sp<- function (x){
classification(x,
               rank_filter="phylum",
               db="gbif")%>%
    head(.)%>%
    data.frame(.)%>%
    dplyr::select(ends_with("rank"),
                  ends_with("name"))%>%
    dplyr::filter(.[[1]] %in% c("kingdom",
                                "phylum",
                                "subphylum",
                                "subclass",
                                "order",
                                "family"))%>%
    dplyr::rename("Rank"=1,
                  "Name"=2)


}    

hierarchy_sp("Leptestheria sarsi")

