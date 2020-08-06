# Obtaining all the Genus/Species mentions in the pages of the research paper

require(quanteda)

text_path<-file.choose()

#

corpus_txt<-pdf_text(text_path)%>%
    quanteda::corpus(.)%>%
    quanteda::tokens(.,
                     remove_punct = TRUE, 
                     remove_numbers = TRUE)%>%
    dfm(.)%>%
    t(.)%>%
    data.frame(.)%>%
    dplyr::rename(word=document)%>%
    anti_join(tidytext::stop_words,
              by='word')%>%
    gather(pages,
           counts,
           text1,
           starts_with('text'))%>%
    dplyr::arrange(word)%>%
    filter(counts>0)

corpus_txt$pages<-gsub("text","page ",corpus_txt$pages) 

View(corpus_txt)

corpus_txt[grep("ilyocryptus",corpus_txt$word),]


## make a second text file of the same pdf     
## get the genus name and then the word immediately next to it can be selected
## This will increase the chances of getting binomial name

View(corpus_txt)


