## Obtaining species names from pdf files


library(tidyverse)
library(ngram)
library(pdftools)


text_path<-file.choose()

trial_1<-pdftools::pdf_text(text_path)%>%
    gsub("\t|\n|\r\n|[0-9]+|[[:punct:]]|\\(.*\\)", "",.)

trial_1

str_detect(ngrams$word,pattern = ".*leydigiopsis*.")
str_extract(ngrams$word,pattern = ".*salina*.")


ngrams<-ngram::ngram(trial_1,n=5,sep = " ")%>%
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
