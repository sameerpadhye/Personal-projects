#Generating a Corpus from a scanned pdf 

#For more information on what corpus is please visit https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/


#libraries used

library(tidyverse)


# Data file path (Data file is assumed to be in the working directory)

test_file_path<- paste0(getwd(),"/sample.pdf")


#The first step is to convert the pdf pages to images using the magick package

if(!require(magick))install.packages('magick')

#Converting the pdf pages to image

pdf_img <- image_read_pdf(text_path2, 
                          density = 72) #This value would change as per the quality of the scanned document


#Viewing the scanned document (it is visualized in the Viewer in RStudio)

pdf_img


#Modifying the image in order to extract maximum words and then converting the image to text using tessaract package. Here the second page is converted

text_data <- manual[2] %>%
    image_resize("2000x") %>%
    image_write(format = 'png', 
                density = '300x300')%>%
    tesseract::ocr() 


#Visualizing the text

text_data


#Converting the text into a 'corpus' using the package quanteda

if(!require(quanteda))install.packages('quanteda') 


text_corpus<-quanteda::corpus(text_data)


#Tokenizing the corpus using tokens function from quanteda

tokens_corpus <- tokens(text_corpus, remove_punct = TRUE, remove_numbers = TRUE)


#Viewing the tokens

tokens_corpus

#This can further be converted into a dataframe, tibble or a matrix for further text analysis
