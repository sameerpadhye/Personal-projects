#Country names extraction from pdfs files


#libraries used

library(tidyverse)
library(magrittr)


# data file path

txt_file<-"C:/Users/samee/Desktop/R data/sample_datasets/sample_pdfs/Rogers.2014.Anostracan Biogeography I. North America.pdf"


#Converting pdf to text using pdftools

if(!require(pdftools))install.packages('pdftools') 

text_data<-pdf_text(txt_file)


#Using quanteda to convert text to corpus object and tokenizing the words

if(!require(quanteda))install.packages('quanteda') 

#Using the stop_words from tidytext package

if(!require(tidytext))install.packages('tidytext')

#Corpus

text_corpus<-corpus(text_data)

#Corpus to tokens

word_token_dataset <- quanteda::tokens(text_corpus, 
                   remove_punct = TRUE, 
                   remove_numbers = TRUE)

word_token_data<-tokens_select(word_token_dataset,
                        names(data_int_syllables))%>% # standard dictionary from the package quanteda
    dfm(.)%>%
    t(.)%>%
    as.tibble(.)%>%
    dplyr::rename(word=document)%>%
    anti_join(stop_words,by='word')%>% # stop_words from tidytext package
    dplyr::rename(name=word)


# Lemmatization of the selected words

if(!require(textstem))install.packages('textstem') #lemmatization

if(!require(lexicon))install.packages('lexicon') # lexicon

#A dictionary needed before actual lemmatization using the textstem and lexicon package

lemmatized_dictionary <- textstem::make_lemma_dictionary(word_token_data$name, 
                                                         engine = 'lexicon')

#Obtaining the lemmatized words

lemm_word_data<-word_token_data%>%
    mutate(lemm_words=textstem::lemmatize_strings(name,
                                                  lemmatized_dictionary))


# Removing meaningless words from the data using dictionaries from the lexicon package

#generating the data required to obtain the necessary words

grady_data<-grady_augmented%>%
    as.tibble(.)%>%
    dplyr::rename(name=value)


#Obtaining the clean words

cleaned_words_data<-lemm_word_data%>%
    dplyr::rename('word'='name')%>%
    dplyr::select(word,
                  lemm_words)%>%
    inner_join(grady_data,
               by=c('lemm_words'='name'))

    
## Selecting proper nouns for extracting country names (devtools::install_github("bnosac/RDRPOSTagger"))
    
library(RDRPOSTagger)
    
unipostagger <- RDRPOSTagger::rdr_model(language = "English", 
                                            annotation = "UniversalPOS")   

#Extracting the country names by using the package countrycode
    
if(!require(countrycode))install.packages('countrycode') 

#Obtaining the country names

country_data<-rdr_pos(unipostagger, 
                      cleaned_words_data$word)%>%
    filter(pos=='PROPN')%>%
    use_series(token)%>%
    str_to_title()%>%
    data.frame()%>%
    dplyr::rename(name=names(.))%>%
    inner_join(countrycode::codelist,
               by=c("name"="country.name.en"))%>%
    dplyr::select(name,continent)%>%
    dplyr::distinct(name)
   

# Obtaining the GIS data of the countries using rworldmap and rgeos(centroid GIS data)

if(!require(rworldmap))install.packages('rworldmap')

if(!require(rgeos))install.packages('rgeos')


# Obtaining the world map centroids

world_centroids <- getMap(resolution="high")%>%
    gCentroid(.,byid=TRUE)%>%
    data.frame(.)%>%
    rownames_to_column(.)%>%
    dplyr::rename(name=rowname)


# Joining the GIS data with the extracted data

country_data_gis<- country_data%>%
    inner_join(world_centroids,
               by = 'name')%>%
    dplyr::rename(lon=x,
                  lat=y)
    

#Obtaining the metadata of the pdf to use it in the map as a reference for easy interpretation using tm package

if(!require(tm))install.packages('tm') 

eng<-readPDF(engine ="pdftools")

pdf_metadata <- eng(elem = list(uri = txt_file),
                    language = "en",
                    id = "id1")


# interactive map using leaflet

if(!require(leaflet))install.packages('leaflet') 

colour_combo <- colorFactor(c("orange", "green", "black"),
                            domain = unique(country_data_gis$name))


locality_map <- leaflet(country_data_gis) %>%
    addProviderTiles("Stamen.Terrain") %>%
    addCircleMarkers(
        color = ~colour_combo(name),
        opacity = 1,
        stroke = TRUE,
        lng = ~lon, 
        lat = ~lat,
        label = ~as.character(name),
        radius = 4)%>%
    addTiles() %>%
    addControl(pdf_metadata$meta$id, 
               position = "bottomright")

locality_map




##############Using pdfsearch package to directly convert pdf to tokens######

if(!require(pdfsearch))install.packages('pdfsearch') 

tokens_pdf<-convert_tokens(text_data,
                           split_pdf = TRUE,
                           remove_hyphen = T)

if(!require(tidytext))install.packages('tidytext') 

if(!require(textclean))install.packages('textclean') 

clean_words<-tokens_pdf%>%
    unlist(.)%>%
    data.frame(.)%>%
    dplyr::rename('word'='.')%>%
    anti_join(tidytext::stop_words) %>%
    # count(word, sort = TRUE)%>%
    mutate_if(is.factor,as.character)%>%
    mutate(words_edtd=tm::removeNumbers(word))%>%
    mutate(words_2=tm::removePunctuation(words_edtd))%>%
    dplyr::filter(words_2 != "") #Here drop_empty_row from textclean package can also be used instead of filter   
#Also this can be used using textclean as well: drop_row(clean_words,"words_edtd",'[[:punct:] ]+')

# The subsequent steps are same as give above for lemmatization and country GIS data extraction

# #Obtaining the lemmatized words
# 
# lemm_word_data<-clean_words%>%
#     mutate(lemm_words=textstem::lemmatize_strings(words_2,
#                                                   lemmatized_dictionary))
# 
# 
# # Removing meaningless words from the data using dictionaries from the lexicon package
# 
# #generating the data required to obtain the necessary words
# 
# grady_data<-grady_augmented%>%
#     as.tibble(.)%>%
#     dplyr::rename(name=value)
# 
# 
# #Obtaining the clean words
# 
# cleaned_words_data<-lemm_word_data%>%
#     dplyr::select(word,
#                   lemm_words)%>%
#     inner_join(grady_data,
#                by=c('lemm_words'='name'))
# 
# ## Selecting proper nouns for extracting country names (devtools::install_github("bnosac/RDRPOSTagger"))
# 
# library(RDRPOSTagger)
# 
# unipostagger <- RDRPOSTagger::rdr_model(language = "English", 
#                                         annotation = "UniversalPOS")   
# 
# #Extracting the country names by using the package countrycode
# 
# if(!require(countrycode))install.packages('countrycode') 
# 
# #Obtaining the country names
# 
# country_data<-rdr_pos(unipostagger, 
#                       cleaned_words_data$word)%>%
#     filter(pos=='PROPN')%>%
#     use_series(token)%>%
#     str_to_title()%>%
#     data.frame()%>%
#     dplyr::rename(name=names(.))%>%
#     inner_join(countrycode::codelist,
#                by=c("name"="country.name.en"))%>%
#     dplyr::select(name,continent)%>%
#     dplyr::distinct(name)
# 
# View(country_data)
# # Obtaining the GIS data of the countries using geocode function from ggmap package
# 
# if(!require(ggmap))install.packages('ggmap') 
# 
# country_data_gis<- country_data%>%
#     magrittr::use_series(name)%>%
#     geocode(., output = c("latlon"),
#             source = c("dsk"))%>%
#     mutate(names=country_data$name)
# 
# # interactive map using leaflet
# 
# if(!require(leaflet))install.packages('leaflet') 
# 
# colour_combo <- colorFactor(c("orange", "green", "black"),
#                             domain = unique(country_data_gis$names))
# 
# 
# locality_map <- leaflet(country_data_gis) %>%
#     addProviderTiles("Stamen.Terrain") %>%
#     addCircleMarkers(
#         color = ~colour_combo(names),
#         opacity = 1,
#         stroke = TRUE,
#         lng = ~lon, 
#         lat = ~lat,
#         label = ~as.character(names),
#         radius = 4)
# 
# locality_map

##ADD THE OCR CONVERSION AND TEXT EXTRACTION ASPECT 
