## Obtaining species names from pdf files

library(tidyverse)
library(ngram)
library(pdftools)


text_path<-file.choose()

trial_1<-pdftools::pdf_text(text_path)%>%
    gsub("\t|\n|\r\n|[0-9]+|[[:punct:]]|\\(.*\\)", "",.)

trial_1

ngrams<-ngram::ngram(trial_1,n=10,sep = " ")%>%
    get.ngrams(.)%>%
    data.frame()%>%
    dplyr::rename('word'='.')%>%
    anti_join(tidytext::stop_words,by='word')%>%
    count(word)

View(ngrams)

two_spaced_words<-grep("  ",ngrams$word)
two_spaced_words    
   
gsub("  "," ",ngrams[two_spaced_words,])    

View(ngrams2)
    
separate(word,c("1st_word","2nd_word"),sep='')

    
View(ngrams)

trial_1.1<-pdftools::pdf_text(text_path)%>%
    gsub("\t|\n|\r\n|[0-9]+|[[:punct:]]|\\(.*\\)", " ",.)
    quanteda::corpus(.)


quanteda::tokens(.,
                     remove_punct = TRUE, 
                     remove_numbers = TRUE)%>%
    tokens_select(.,names(quanteda::data_int_syllables))%>%
    dfm(.,ngrams=2)%>%
    t(.)%>%
    data.frame(.)%>%
    dplyr::rename(word=document)
View(trial_1.1%>%
         arrange(desc(text1)))


## Trial 02-04-2021

# data path

data_path<-"C:/Research_data/Publications/Indian Cladocera Monograph.pdf"

# ## FOR MULTIPLE PDFS
# 
# # data_path<-"C:/Users/samee/Desktop/R data/sample_datasets/sample_pdfs/pdf_files"
# 
# data_path<-"C:/Users/samee/Downloads/trial_pdfs_text_data_shalini"
# 
# pdf_files<-list.files(data_path,pattern = '*.pdf')%>%
#     paste0(data_path,"/",.)

# list of reference words

library(readxl)

list_of_words<-read_excel(file.choose(),
                          sheet=1)%>%
    dplyr::rename("word_list"="species_names")

# function to obtain matrix of words

crpus_to_tokens<-function(x){
    
    step_1= x%>%
        pdftools::pdf_text(.)%>%
        quanteda::corpus(.)%>%
        quanteda::tokens(.,
                         remove_punct = TRUE, 
                         remove_numbers = TRUE)%>%
        dfm(.)%>%
        t(.)%>%
        data.frame(.)%>%
        dplyr::rename("word_list"="doc_id")
    
    
    return(step_1)
    
    # try to run the entire process within the function and use map to get the results for each pdf
}

# looping over all the pages of the text

token_text<-purrr::map(data_path,crpus_to_tokens)

View(token_text[[1]])


# obtaining ref names from online resources

library(taxize)

trial<-downstream("Cladocera", db ='itis', downto = 'species')
cladocera_ref<-trial$Cladocera%>%
    dplyr::select(tsn,taxonname)%>%
    dplyr::rename("word_list"='taxonname')

View(cladocera_ref)
# for giving universal names with increasing number and same text   

#sprintf("text%d",seq=(1:length(token_text)))

library(reshape2)

text_pdfs<-melt(token_text)
dplyr::rename("words_from_pdfs"="word_list.x",
              "reference_words"= "word_list.y",
              "word_count"="value",
              "name_of_pdf"="L1")%>%
    select(-variable)%>%
    arrange(name_of_pdf,word_count)

View(text_pdfs)

step_2 = text_pdfs%>%
    select(word_list:value)%>%
    stringdist_inner_join(cladocera_ref, 
                          method = "soundex", 
                          by="word_list")


View(step_2)
