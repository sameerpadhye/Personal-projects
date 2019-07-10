#Basic Sentiment Analysis 

# For more information please visit these webpages 1. https://towardsdatascience.com/sentiment-analysis-concept-analysis-and-applications-6c94d6f58c17

# Data for Sentiment analysis used here is the same pdf used in other text analyses as well. Though, this is not the appropriate text to be used, the main purpose here is to give a code so that anyone can easily adopt it to their needs and requirements


# Libraries

library(tidyverse)


# Data file path (Data file is assumed to be in the working directory)

test_file_path<- paste0(getwd(),"/sample.pdf")


#The extraction part is same as for other text analyses using pdftools package

if(!require(pdftools))install.packages('pdftools') 

if(!require(tm))install.packages('tm') 

data_text<-pdf_text(txt_file_path)%>% #converting the pdf file to text
    str_replace_all(",","")%>% #function from stringr used to replace commas
    str_squish()%>%  # function from stringr to remove whitespaces
    tm::removeNumbers(.) 


#Transforming and tokenizing the dataset using tidytext

if(!require(tidytext))install.packages('tidytext') 

# obtaining the data in a dataframe type of setting (as a tibble) for all the pages of the pdf by using the following function

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
    map_df(.,function_word)


# The words from the pdf are joined with the sentiment dictionary/ies. Here the Loughran dictionary has been used from the tidytext package.Other dictionaries can also be used as per requirement

#Obtaining the data by joining followed by counting the words with respect to different sentiments

word_count_sentiment <- data_text_all %>% 
    #join with the sentiment dictionary Loughran. 
    inner_join(get_sentiments("loughran")) %>%
    #grouping by the sentiments from the Loughran dictionary
    group_by(sentiment)%>%
    count(sentiment)


#Exploring the data

View(word_count_sentiment)


#Plot to visualize number of words/sentiment

word_count_sentiment%>%
    ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE)+
    coord_flip() +
    labs(title = "Sentiment Word Counts",
         x = "Words")+
    theme_bw(base_size = 14)


#Obtaining corresponding sentiment of each selected word from the text

word_count_sentiment2 <- data_text_all %>% 
    inner_join(get_sentiments("loughran")) %>% 
    #Here filtering has been done for positive, negative and uncertainty sentiments but can be changed as per user requirements
    filter(sentiment %in% c("positive", "negative", "uncertainty"))%>%
    # Counts of words with respective sentiments
    count(word,
          sentiment)%>%
    group_by(sentiment) %>% 
    ungroup() %>% 
    # Create a factor called word2 that has each word ordered by the count
    mutate(word_ordered = fct_reorder(word, 
                                      n))

#Exploring the data

View(word_count_sentiment2)


# Plot to visualize the above dataset

word_count_sentiment2 %>%
    ggplot(aes(x = word_ordered, 
               y = n, 
               fill = sentiment)) +
    geom_col(show.legend = FALSE)+
    # # Create a separate facet for each sentiment with free axes
    facet_wrap(~ sentiment, 
               scales = "free") +
    coord_flip() +
    # Title the plot "Sentiment Word Counts" with "Words" for the x-axis
    labs(
        title = "Sentiment Word Counts",
        x = "Words"
    )+
    theme_bw(base_size = 14)


#Calculating an overall sentiment using the words from the text and Bing dictionary from tidytext package

word_count_sentiment3 <- data_text_all %>% 
    # Append the bing sentiment dictionary
    inner_join(get_sentiments('bing'))  %>%
    group_by(sentiment)%>%
    count()%>%
    # Spread the sentiment and count columns
    spread(sentiment,n) %>% 
    # Compute overall_sentiment = positive - negative
    mutate(overall_sentiment = positive - negative)


#Exploring the data

View(word_count_sentiment3)


#Visualization of above dataset

word_count_sentiment3%>%
    gather(sentiments,
           value,
           negative:overall_sentiment)%>% #using gather to gather data for easy plotting
    ggplot(aes(x = sentiments, 
               y = value)
    ) +
    geom_col(show.legend = FALSE) +
    coord_flip() + 
    # Title the plot "Overall Sentiment by Complaint Type," with an "Airline Twitter Data" subtitle
    labs(
        title = "Overall Sentiment"
    ) + 
    theme_bw(base_size = 14)
