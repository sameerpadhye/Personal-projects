## Obtaining specific information from scanned pdfs (Here country names)

## The main reason for carrying out this work was to be able to extract locality information (here country) from old pdf's which are scanned and uploaded online
## This becomes useful in case of research papers focusing and distribution records or papers dealing with biogeography.

#The Process is divided mainly into:

#1. Converting the scanned pdf to image and an image to text (corpus)

#2. The text is then cleaned and lemmatized

#3. The tokens (words) are then matched with country names and extracted

#4. GIS data of the country is extracted and mapped 

# For some additional information:

#https://cran.r-project.org/web/packages/magick/vignettes/intro.html - magick package to convert pdf to images
#then use tesseract to convert them to text https://ropensci.org/blog/2016/11/16/tesseract/


# libraries used

if(!require(magick))install.packages('magick') 

if(!require(tesseract))install.packages('tesseract') 

if(!require(quanteda))install.packages('quanteda') 

if(!require(tidyverse))install.packages('tidyverse') 

if(!require(magrittr))install.packages('magrittr') 


#scanned pdf path (its assumed that the file is put in the working directory)

text_path2<-paste0(getwd(),'/scanned_pdf_test.pdf')


#Obtaining the scanned pdf as a image

scanned_pdfs <- image_read_pdf(text_path2, 
                               density = 100)


#Checking the number of pages 

length(scanned_pdfs)


### A function to get the tokens from scanned pdfs (This function is specifically written to take single pages so that its upto the user to state which pages need search for country names)

token_generator_pdf<-function(img){
    text <- img %>%
        image_write(format = 'png', 
                    density = '100x100')%>%
        tesseract::ocr() %>%
        quanteda::corpus(.)%>%
        quanteda::tokens(.,
                         remove_punct = TRUE, 
                         remove_numbers = TRUE)
    return(text)
}


#Obtaining the tokens of the scanned pdf (here the second page is selected)

pdf_page<-token_generator_pdf(scanned_pdfs[2])


# A lexicon is used for stop words from the tidytext package and 'data_int_syllables' is used for removing some of the meaningless words

if(!require(tidytext))install.packages('tidytext') 


#Obtaining the data

tokens_pdf<-tokens_select(pdf_page,
                          names(data_int_syllables))%>%
    dfm(.)%>%
    t(.)%>%
    as.tibble(.)%>%
    dplyr::rename(word=document)%>%
    anti_join(stop_words,by='word')%>%
    dplyr::rename(name=word)%>%
    dplyr::arrange(desc(text1))


#View the result

View(tokens_pdf)


# Lemmatization of the selected words: Words are converted into their roots 

# A dictionary is needed before actual lemmatization which is generated using the textstem and lexicon package

if(!require(textstem))install.packages('textstem') #lemmatization

if(!require(lexicon))install.packages('lexicon') # lexicon

#Obtaining dictionary 

lemmatized_dictionary <- textstem::make_lemma_dictionary(tokens_pdf$name, 
                                                         engine = 'lexicon')

#Obtaining the lemmatized words

lemm_word_data<-tokens_pdf%>%
    mutate(lemm_words=textstem::lemmatize_strings(name,
                                                  lemmatized_dictionary))


# A filter to remove meaningless words from the data is run again this time using dictionaries from the lexicon package. This is done in 2 steps:

#1. generating the data required to obtain the necessary words

grady_data<-grady_augmented%>%
    as.tibble(.)%>%
    dplyr::rename(name=value)


#2. Obtaining the clean words

cleaned_words_data<-lemm_word_data%>%
    dplyr::rename('word'='name')%>%
    dplyr::select(word,
                  lemm_words)%>%
    inner_join(grady_data,
               by=c('lemm_words'='name'))%>%
    group_by(lemm_words)%>%
    tally()%>%
    arrange(desc(n))%>%
    dplyr::rename("words"="lemm_words")

#View the result

View(cleaned_words_data)


## One more filter is used wherein only the proper nouns are selected from the data. This is done using RDRPOSTagger

## (devtools::install_github("bnosac/RDRPOSTagger"))

require(RDRPOSTagger)

unipostagger <- RDRPOSTagger::rdr_model(language = "English", 
                                        annotation = "UniversalPOS")   


#The country names are extracted by matching it with standard names using the package countrycode

if(!require(countrycode))install.packages('countrycode') 


#Obtaining the country names (with counts)

country_data<-rdr_pos(unipostagger, 
                      cleaned_words_data$words)%>%
    filter(pos=='PROPN')%>%
    use_series(token)%>%
    str_to_title()%>%
    data.frame()%>%
    dplyr::rename(name=names(.))%>%
    inner_join(countrycode::codelist,
               by=c("name"="country.name.en"))%>%
    dplyr::select(name,
                  continent)%>%
    dplyr::distinct(name)%>%
    group_by(name)%>%
    tally(.)

#View the result

country_data$name


#A simple wordcloud is run for the above data for easy visualization using the package wordcloud (The condition here being that there are names of countries in the selected page)

if(nrow(country_data)>0){
    
    if(!require(wordcloud))install.packages('wordcloud')
    
    country_data%$%
        wordcloud(name,
                  n,
                  scale = c(2,1),
                  colors = 'black',
                  random.color = TRUE)
    
} else {
    print("No country names available")
}


#If there is no name in the image, the analysis stops here. If there are names in the data, the countries are visualized using leaflet package.

#1. The country centroids (GIS values) are obtained 

#2. The centroid data is joined with the country data obtained above

# GIS data of the countries is obtained using rworldmap and rgeos(centroid GIS data) packages

if(!require(rworldmap))install.packages('rworldmap')

if(!require(rgeos))install.packages('rgeos')

if(!require(rworldxtra))install.packages('rworldxtra')


#1. Obtaining the world map centroids

world_centroids <- getMap(resolution="high")%>%
    gCentroid(.,byid=TRUE)%>%
    data.frame(.)%>%
    rownames_to_column(.)%>%
    dplyr::rename(name=rowname)


#2. Joining the GIS data with the extracted data

country_data_gis<- country_data%>%
    inner_join(world_centroids,
               by = 'name')%>%
    dplyr::rename(lon=x,
                  lat=y)


# This data is then visualized using leaflet package

if(!require(leaflet))install.packages('leaflet') 


#Color scheme for the GIS points

colour_combo <- colorFactor(c("orange", "green", "black"),
                            domain = unique(country_data_gis$name))


#Map

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
    addTiles()

#View the map

locality_map


#This project is still work in progress and will be edited from time to time
