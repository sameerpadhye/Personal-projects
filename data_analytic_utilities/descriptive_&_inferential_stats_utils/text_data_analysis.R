#Extracting and exploring text data 

# For more information please visit these webpages 1. https://github.com/juliasilge/tidytext and 2. https://uc-r.github.io/tidy_text


#1. Extracting and exploring text from pdf's 


# Libraries

library(tidyverse)


# Data file path (Data file is assumed to be in the working directory)

test_file_path<- paste0(getwd(),"/sample.pdf")


#Extracting text from pdf using 'pdftools'. Package tm is also used in text data manipulation (Here just used to remove numbers from the text)

if(!require(pdftools))install.packages('pdftools') 

if(!require(tm))install.packages('tm') 

data_text<-pdf_text(txt_file_path)%>% #converting the pdf file to text
    str_replace_all(",","")%>% #function from stringr used to replace commas
    str_squish()%>%  # function from stringr to remove whitespaces
    tm::removeNumbers(.) 


#Exploring the data

#1. Class of the object

paste0('class of the dataset: ',class(data_text))

#2. Number of pages in the pdf (Length of the object)

paste0('Number of pages in the pdf: ',length(data_text))

#3. Checking the word count in each page

str_count(data_text)


#Transforming and tokenizing the dataset using tidytext

if(!require(tidytext))install.packages('tidytext') 

# obtaining the data in a dataframe type of setting (as a tibble)

text_df<-tibble(text = data_text)%>%   # data
    tidytext::unnest_tokens(word,
                            text)%>% # function to tokenize the words
    dplyr::count(word,
                 sort = T)%>%  #counting the words
    dplyr::anti_join(stop_words) %>% 
    # used to remove some common words like 'the'
    dplyr::count(word, 
                 sort = TRUE)


#Since there are 12 pages of the pdf, we need to loop over all the pages. First,a function using the above code is obtained

#Function

function_word<-function(df){
    
    tibble(chapter = (1:length(df)),
           text = df)%>%
        unnest_tokens(word,
                      text)%>%
        count(word,sort = T)%>%
        anti_join(stop_words) %>%
        count(word, 
              sort = TRUE)
}

#Then the function is looped over the text data using map_df from purrr package to obtain the word counts in all the 12 pages

data_text_all<-data_text%>%
    map_df(.,function_word)%>%
    count(word, sort = TRUE)%>% 
    arrange(desc(n))


#Exploring the data

head(data_text_all,10)


# Basic visualizations of the data. Top 25 words in the data

data_text_all%>%
    top_n(25)%>%
    ggplot(aes(word, 
               n)) +
    geom_bar(stat = "identity")+
    theme_bw(base_size = 15)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1))




