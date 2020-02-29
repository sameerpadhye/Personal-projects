#Correlation analysis

#libraries used
library(tidyverse)
library(reshape2)

#Here a sample dataset is used. The numerical data from any dataframe (correlations of which need to be determined) can be substituted as per requirement

#Data for analysis

correlation_data<-data.frame(
    var_1=c(10,12,17,29,35,NA,56,89,112,156),
    var_2=seq(160,1,length.out = 10),
    var_3=runif(10),
    var_4=sample(c(1:200),10,replace = T))


#Exploring the data

head(correlation_data,3)


## Correlation test for 2 variables 

# cor.test provides the t statistic and p values as well. 'cor' only gives the correlation coefficient. Hence, cor.test is used

# Pearson correlation coefficient (used when data are normal)

cor.test(correlation_data$var_1,
         correlation_data$var_2,
         method=c("pearson"))


# Spearman correlation coefficient (used when data are normal)

cor.test(correlation_data$var_1,
         correlation_data$var_2,
         method=c("spearman"))


#Scatterplot for visualization (var_1 and var_2 visualized here)

correlation_data%>%
    ggplot(aes(x=var_1,
               y=var_2))+
    geom_point(size=5,
               color='black',
               pch=21,
               fill='steelblue')+
    theme_bw(base_size = 18)


#Multiple correlation using all variables in the data

# Multiple correlation using pearson's correlation coefficient. Data needs to be a a matrix hence converted within the function itself. Here Pearson's correlation coefficient is used. Spearman can also be used as per requirement


mult_correlation<-Hmisc::rcorr(as.matrix(correlation_data),
                               type=c('pearson'))

#getting the correlation coefficients

mult_correlation$r

#getting the p values (significance) of the coefficients

mult_correlation$P


#Heatmap visualization of the correlations

heatmap(mult_correlation$r)


##Visualization using ggplot. Correlation coefficients are used 

#getting the long format of the data

data_for_viz<-reshape2::melt(mult_correlation$r)


#plot (Var1 and Var2 are just the column names that reshape assigns. These can be changed if required)

data_for_viz%>%
    ggplot(aes(x=Var1,
               y=Var2,
               fill=value))+
    geom_tile()+
    theme(axis.title.x=element_blank(),
          axis.title.y = element_blank())


#Correlation of a binary variables

#The continous data have been first converted to binary data using the correlationfunnel package

if(!require(correlationfunnel))install.packages('correlationfunnel')

#Obtaining the data (First six columns selected at the end)

data_analysis_binary <- correlation_data %>%
    dplyr::select(var_1:var_3)%>%
    na.omit(.)%>%
    binarize(n_bins = 4, 
             thresh_infreq = 0.01)%>%
    dplyr::select(1:5)

#View the result

View(data_analysis_binary)


#Correlation of binary variables. 

#Rename the new columns (for convenience)

colnames(data_analysis_binary)<-c("var_1_1","var_1_2","var_1_3","var_1_4","var_2")

#Correlation analysis Here the var_2 is selected as a response variable against which correlations of the other 4 variables will be displayed

data_analysis_binary%>%
    na.omit(.)%>%
    correlationfunnel::correlate(target = var_2)


#Visualizing the correlation analysis

data_analysis_binary%>%
    na.omit(.)%>%
    correlationfunnel::correlate(target = var_2)%>%
    plot_correlation_funnel(interactive = FALSE)


#Finding highly correlated variables using caret package

if(!require(caret))install.packages('caret')

#1. Getting the correlation of the dataset

correlations_of_data<-correlation_data%>%
    select_if(is.numeric)%>%
    as.matrix(.)%>%
    cor(.,method = "spearman")


#2. Index for selecting high correlated variables

index_selection<-findCorrelation(correlations_of_data,
                                 cutoff = 0.65)%>%
    sort(.)

#3. Finding the highly correlated variables (the columns displayed are the hgihly correlated variables based on the cutoff given above)

correlations_of_data[,c(index_selection)]
