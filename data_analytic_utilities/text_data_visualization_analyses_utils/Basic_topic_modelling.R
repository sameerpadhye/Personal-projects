#Basics of topic modelling in text analysis


#libraries used

library(tidyverse)


# Data file path (Data file is assumed to be in the working directory)

txt_file_path<- paste0(getwd(),"/sample.pdf")


#pdf to text for all the pages using pdftools

if(!require(pdftools))install.packages('pdftools')

text_data2<-pdf_text(txt_file_path)


#text to corpus using tm package

if(!require(tm))install.packages('tm') 

corpus_text<-SimpleCorpus(VectorSource(text_data2))%>%
    tm_map(.,removePunctuation)%>%
    tm_map(.,stripWhitespace)%>%
    tm_map(.,removeWords,stopwords('en'))


#corpus to dtm (Document term matrix) using tm package

tokens_data<-corpus_text%>%
    tm::DocumentTermMatrix(.)


#dtm used for topic modelling. Number of topics can be changed as per user requirement

if(!require(topicmodels))install.packages('topicmodels') 

lda_test <- LDA(tokens_data, 
                k = 4,  # number of topics
                method = "Gibbs", #method used for modelling
                control = list(seed = 999)) 


# View the results using tidytext package

if(!require(tidytext))install.packages('tidytext') 


#Exploring the probabilities of each word from the text

lda_test_result<-tidytext::tidy(lda_test,
                                matrix='beta')


#Exploring the top 20 words with respective probabilities

top_terms_topics <- lda_test_result %>%
    group_by(topic) %>%
    top_n(20, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

#Explore the result

View(top_terms_topics)

#plot for visualizing the above data

top_terms_topics %>%
    mutate(term = reorder(term, 
                          beta)) %>%
    ggplot(aes(term, 
               beta, 
               fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, 
               scales = "free") +
    coord_flip()


# Document - topic probabilities (each page of pdf corresponding to which topic and its respective probability)

doc_topic_data<-tidytext::tidy(lda_test,
                               matrix='gamma')

#Explore the result

View(doc_topic_data)

#Plotting the above data

doc_topic_data %>%
    ggplot(aes(factor(topic), 
               gamma)) +
    geom_boxplot() +
    facet_wrap(~ document)


