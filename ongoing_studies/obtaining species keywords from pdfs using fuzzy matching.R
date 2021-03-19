
# Libraries

library(pdftools)
library(tm)
library(quanteda)
library(reshape2)
library(tidyverse)
library(tidytext)
library(magrittr)

#1. Import pdf

text_path<-choose.files()

#2. Convert the pdf to text

# corpus_txt<-pdf_text(text_path)%>%
#     quanteda::corpus(.)
# 
#     quanteda::tokens(.,
#                      remove_punct = TRUE, 
#                      remove_numbers = TRUE)%>%
#     #tokens_select(.,names(quanteda::data_int_syllables))%>%
#     dfm(.)%>%
#     t(.)%>%
#     data.frame(.)%>%
#     dplyr::select(doc_id)
#     
#     dplyr::rename(word=document)%>%
#     anti_join(tidytext::stop_words,by='word')%>%
#     gather(pages,counts,text1,starts_with('text'))%>%
#     dplyr::arrange(pages)

# using tm for corpus

corpus_data_path<-"C:/Users/samee/Desktop/R data/sample_datasets/sample_pdfs/Diversity and zoogeography of fairy shrimps on the Indian subcontinent.pdf"

library(tm)

corpus_txt<-Corpus(URISource(corpus_data_path),
       readerControl = list(reader = readPDF))%>%
    tm_map(., removePunctuation, ucp = TRUE)

corpus_txt

text_tdm<-TermDocumentMatrix(corpus_txt, 
                             control = 
                                 list(removePunctuation = TRUE,
                                      stopwords = TRUE,
                                      removeNumbers = TRUE))

inspect(text_tdm)

matrix_text<-as.matrix(text_tdm)%>%
    data.frame(.)%>%
    rownames_to_column(.)%>%
    dplyr::rename("species_name"="rowname")

View(matrix_text)

approx_names<-data.frame(species_name = c("dichotomus", "hardingi", "orientalis",
                                "spinifer", "simplex", "maduraensis"))


join_trial_1<-matrix_text%>%
    stringdist_left_join(approx_names, 
                         method = "soundex", 
                         by="species_name")%>%
    drop_na(.)


View(join_trial_1)

## For converting corpus to plain text

#install.packages('textreg')

# library(textreg)
# 
# corpus_text<-convert.tm.to.character(corpus_txt)%>%
#     stripWhitespace(.)%>%
#     gsub('[0-9]+', '',.)
# 
# corpus_text


#https://rdrr.io/cran/textreg/man/convert.tm.to.character.html - to convert the corpus to text
#https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/#:~:text=The%20first%20argument%20to%20Corpus,vector%20is%20a%20URI%20source.


## FOR MULTIPLE PDFS

# data_path<-"C:/Users/samee/Desktop/R data/sample_datasets/sample_pdfs/pdf_files"

data_path<-"C:/Users/samee/Downloads/trial_pdfs_text_data_shalini"

pdf_files<-list.files(data_path,pattern = '*.pdf')%>%
    paste0(data_path,"/",.)

pdf_files

crpus_to_tokens<-function(x){
    
    x%>%
        pdftools::pdf_text(.)%>%
        quanteda::corpus(.)%>%
        quanteda::tokens(.,
                         remove_punct = TRUE, 
                         remove_numbers = TRUE)
    # try to run the entire process within the function and use map to get the results for each pdf
}


token_text<-purrr::map2_df(pdf_files,crpus_to_tokens)

class(pdf_files)

list_of_words<-data.frame(word_list=c("Pandemic", "Urban", "Foraging", "Wild Plants","Lockdown", "Gathering", "Covid-19", "Wild plants", "Food", "Green spaces", "wild edibles", "isolation", "alternative food"))
