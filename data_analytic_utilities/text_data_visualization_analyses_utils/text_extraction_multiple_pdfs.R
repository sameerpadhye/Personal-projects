# Text extraction and token generation using multiple pdf files

# For more information please visit https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/

#libraries used

library(tidyverse)


#Path for the directory wherein the pdfs are stored (assumed to be in the working directory)

dir<-getwd()

#Generating the file paths of the pdfs

files_path<-paste(dir,'/',list.files(dir,pattern = "*.pdf"),sep = "")


# Two ways of obtaining tokens have been provided below


##1.Using tm package######

#Generating a corpus of the pdfs using tm package

if(!require(tm))install.packages('tm') 

#Corpus

corpus <- Corpus(URISource(files_path),
               readerControl = list(reader = readPDF))%>%
    tm_map(.,removePunctuation)%>%
    tm_map(.,stripWhitespace)


#Generating a document term matrix followed by a matrix

txt_matrix<-DocumentTermMatrix(corpus,
                               control = list(removePunctuation = TRUE,
                                              stopwords = TRUE,
                                              tolower = TRUE,
                                              stemming = TRUE,
                                              removeNumbers = TRUE))%>%
    as.matrix(.)%>%
    t(.)


#View result

View(txt_matrix)

#The text data still contains meaningless words 

#The text data still contains meaningless words 

#to remove meaningless words, grady_augmented lexicon from lexicon package is used

if(!require(lexicon))install.packages('lexicon') 

#obtaining the words

grady_data<-grady_augmented%>%
    as.tibble(.)%>%
    dplyr::rename(name=value)

#Words from txt matrix is extracted followed by joining with the above dataset

word_data_pdfs<-txt_matrix%>%
    data.frame(.)%>%
    rownames_to_column(.)%>%
    inner_join(grady_data,
               by=c('rowname'='name'))

#Viewing the result

View(word_data_pdfs)


##2.Using pdftools package######

if(!require(pdftools))install.packages('pdftools')


#Function to generate cleaned text data of multiple pdfs using pdftools, stringr and tm package

mult_pdf_txt_gen<-function(x){
    
    txt_data=pdf_text(x)%>%
        stringr::str_replace_all(",","")%>%
        stringr::str_squish()%>%
        tm::removeNumbers(.)
    
    return(txt_data)
}


#1. Using map from purrr package to loop the above function over all the pdfs

final_text<-map(files_path,
                mult_pdf_txt_gen)

#OR

#2. Using lapply

final_text2<-files_path%>%
    lapply(.,mult_pdf_txt_gen)


#View the result

final_text

final_text2


#Editing and tokenizing the text data using tidytext package

if(!require(tidytext))install.packages('tidytext') 

#Function to obtain tokens for all pdfs

text_tokenizer<-function (x) {
    
    textdata<-tibble(text = x)%>%
    unnest_tokens(word,text)%>%
    count(word,sort = T)%>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
    
return(textdata)
}

#1.Using map from purrr package to loop the above function over all the pdfs

edited_text<-map(final_text,
                 text_tokenizer)

#OR

#2. Using lapply

edited_text2<-final_text2%>%
    lapply(.,text_tokenizer)


#View results

edited_text

editex_text2


#Stemming and Lemmatization of the words. Here the results from the first part 'word_data_pdfs' have been used

if(!require(SnowballC))install.packages('SnowballC') #stemming

if(!require(textstem))install.packages('textstem') #lemmatization

#A dictionary needed before actual lemmatization using the textstem and lexicon package

if(!require(lexicon))install.packages('lexicon') 

lemmatized_dictionary <- textstem::make_lemma_dictionary(word_data_pdfs$rowname, 
                                                         engine = 'lexicon')

#Obtaining the lemmatized and stemmed words

word_data_pdfs<-word_data_pdfs%>%
    mutate(stemwords=SnowballC::wordStem(rowname,
                                         language = 'english'),
           lemm_words=textstem::lemmatize_strings(rowname,
                                                  lemmatized_dictionary))

#Viewing the result

View(word_data_pdfs)
