## Exploring the taxsize package

# Libraries used

#install.packages('taxize')

library(taxize)

require(tidyverse)


# Search species by name 

bold_search(name="Cyclestheria hislopi")

eubon_search("Daphnia")


# Heirarchical classification

classification('Cyclestheria', 
               db = 'pow')


# Get id from a specific database (here worms)

get_wormsid("Daphnia", 'scientific')


# Get specific information about a species

tax_name(query = "Streptocephalus dichotomus", 
         get = "family", 
         db = "ncbi")


# Get GBIF ID

get_gbifid("Streptocephalus dichotomus")


# Get multiple IDs (from different databases)

get_ids("Artemia salina",db=c('itis', 'ncbi','eol'))


# Obtain synonyms (if/any)

synonyms("Artemia salina",
         db = "gnr")


# Function to obtain heirarchical classification of any species 

hierarchy_sp<- function (x){
    
    get_uid(x,key = '49393c73c0644a20c0ac4d9c53243a074a08')%>%
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
    
    
}    

hierarchy_sp("Daphnia magna")
