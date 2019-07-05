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


# Visualization of the top 25 words in the data

data_text_all%>%
    top_n(25)%>%
    ggplot(aes(word, 
               n)) +
    geom_bar(stat = "identity")+
    theme_bw(base_size = 15)+
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1))+
    coord_flip() # for easier viewing


#Exploring the word count wherein the count is than the median count

data_text_all%>%
    filter(n>median(n))


#Creating custom stop words for removal after exploring the data (stop_words contains two columns namely, words and lexicon and hence the column names of the new data are kept same)

additional_stop_words <- tibble::tribble(
    ~word,  ~lexicon,
    # custom words are added here. These will change as per user requirement
    "mm", "CUSTOM",
    "figs",  "CUSTOM",
    "ing", "CUSTOM",
    "fig", "CUSTOM"
)


#Creating new stop_word_data by binding the above dataset with stopwords data

stop_words_add <- stop_words %>% 
    bind_rows(additional_stop_words)


#Creating a new dataset by using the new stop words. A new factor is created based on 1. the words and 2. their respective counts


data_text_all2<-data_text_all%>%
    anti_join(stop_words_add) %>%
    arrange(desc(n))%>%
    dplyr::mutate(word_fct=fct_reorder(word,n))


# Plotting the new dataset (top 25 words) with the new factor

data_text_all2%>%
    top_n(25)%>%
    ggplot(aes(x = word_fct, y = n)) +
    geom_bar(stat = "identity")+
    coord_flip()+
    ggtitle("Word count of the pdf")


# Wordcloud for the words

if(!require(wordcloud))install.packages('wordcloud')
if(!require(RColorBrewer))install.packages('RColorBrewer')


# Create a complaint word cloud of the top 50 terms, colored red

wordcloud(
    words = data_text_all2$word, #words
    freq = data_text_all2$n, # their counts
    max.words = 50,  # max words to be included in the figure
    rot.per=0.5,   #rotation of words
    min.freq=3,   # min frequency of words cutoff
    scale=c(1.5,.5),  #scale of the size of the words
    vfont=c("sans serif",
            "bold"), #Font
    colors=brewer.pal(8, 
                      "Dark2")) #colors (RcolorBrewer used here)


##Using tm package for text_mining

## More information can be found on https://towardsdatascience.com/understanding-and-writing-your-first-text-mining-script-with-r-c74a7efbe30f

if(!require(tm))install.packages('tm')


# Converting the pdf file to a corpus file

text_data<-pdf_text(txt_file_path)

corpus_text<-SimpleCorpus(VectorSource(text_data))%>% #creating the corpus
    tm_map(.,removePunctuation)%>% #remove punctuations
    tm_map(.,stripWhitespace)%>% # remove white spaces
    tm_map(.,removeWords,
           stopwords('english')) # remove common stop words and articles


# Converting the corpus file to a document term matrix followed by conversion into a matrix

txt_matrix<-DocumentTermMatrix(corpus_text)%>%
    as.matrix(.)%>%
    t(.)


# Converting the matrix into a tibble with word counts

txt_matrix_sum<-txt_matrix%>%
    data.frame(.)%>% #conversion into a dataframe
    rownames_to_column('rownames')%>% # obtaining the words 
    mutate(word_count=rowSums(.[,-1]))%>% #getting the counts
    dplyr::arrange(desc(word_count))%>% #arranging the words based on counts
    dplyr::select(rownames,word_count)%>% #selecting words and counts
    dplyr::rename(word=rownames)%>% #renaming the column (to word)
    anti_join(stop_words) # removing additional stop words (from quanteda)


#Viewing the data

View(txt_matrix_sum)

#Wordcloud using the above dataset

if(!require(wordcloud))install.packages('wordcloud')

wordcloud(
    words = txt_matrix_sum$word, #words
    freq = txt_matrix_sum$word_count, # their counts
    max.words = 50,  # max words to be included in the figure
    rot.per=0.5,   #rotation of words
    min.freq=3,   # min frequency of words cutoff
    scale=c(1.5,.5),  #scale of the size of the words
    vfont=c("sans serif","bold"), #Font
    colors=brewer.pal(8, "Dark2"))





