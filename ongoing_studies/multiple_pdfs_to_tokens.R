
require(pdftools)
require(tidyverse)
require(purrr)
library(tm)
library(quanteda)

fraud_data_path<-"C:/Research_data/Research data/Other groups/Fraud Faunistics correspondence/Faunistics checklist fraud pprs/fraud_pprs"

good_data_path<-"C:/Research_data/Research data/Other groups/Fraud Faunistics correspondence/Faunistics checklist fraud pprs/good_pprs"

file_list_1<-list.files(fraud_data_path,pattern = '*.pdf')%>%
    paste0(fraud_data_path,"/",.)

file_list_1[1]    
fraud_text<-purrr::map(file_list_1,crpus_to_tokens)

View(fraud_text[[1]])

crpus_to_tokens<-function(x){
    
    x%>%
        pdftools::pdf_text(.)%>%
    quanteda::corpus(.)%>%
    quanteda::tokens(.,
                     remove_punct = TRUE, 
                     remove_numbers = TRUE)

}
