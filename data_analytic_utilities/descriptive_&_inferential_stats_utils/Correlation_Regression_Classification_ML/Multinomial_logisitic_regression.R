# Multinomial logisitic regression 


#Libraries used


library(tidyverse)
library(readxl)


#data file path (Here environmental data has been used)


data_path<-paste0(getwd(),"/environmental_data.xlsx")


# Importing the dataset


mult_log_data<-readxl::read_excel(data_path,sheet=1)


# exploring the data


str(mult_log_data)


table(mult_log_data$habitat_type)


# Visualizing the data based on the three levels of the habitat_type


mult_log_data%>%
    gather(env_trait,values,Altitude:Salinity)%>%
    ggplot(aes(x=habitat_type,
               y=values))+
    geom_boxplot(fill='grey',
                 lwd=0.9)+
    theme_bw(base_size = 18)+
    facet_wrap(~env_trait,
               scales = 'free')


# Defining the formula for regression(Only a certain dependent variables have been used in the analysis.


reg.formula<-reformulate(names(mult_log_data)[c(3,5,6,9)], 
                         names(mult_log_data[1]))

reg.formula


# Performing multinomial regression using nnet package


if(!require(nnet))install.packages('nnet') 


# Fitting the regression model


mult_reg_model <- nnet::multinom(reg.formula, data = mult_log_data)


# Summarize the model


summary(mult_reg_model)


# Obtaining the coefficients from the model 


exp(coef(mult_reg_model))


#Obtaining the predicted probabilities


fitted(mult_reg_model)
