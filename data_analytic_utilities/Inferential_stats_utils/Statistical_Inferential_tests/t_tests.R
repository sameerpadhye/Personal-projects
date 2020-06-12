##Performing t.tests in R

#Libraries used

library(tidyverse)
library(magrittr)


#Data for analysis. Here the data can be assumed to be weights of 20 animals pre and post treatments respectively

pre_treatment_wt<-rnorm(n=20,
                        mean=10,
                        sd=0.7)

post_treatment_wt<-rnorm(n=20,
                         mean=5,
                         sd=0.4)

#Combining the data into a dataframe

wt_data<-cbind (pre_treatment_wt,post_treatment_wt)%>%
    data.frame(.)


#Viewing the data

head(wt_data,3)

#Summarizing the data

summary(wt_data)

#Data visualization

wt_data%>%
    gather(treatment,
           value,
           pre_treatment_wt:post_treatment_wt)%>%  ## data converted from wide to long data type
    ggplot(aes(x=treatment,
               y=value))+
    geom_boxplot(col='black',
                 fill="orange")+
    theme_bw(base_size = 18)+
    ylab("Trait_values")+
    xlab("Factor")+
    ggtitle("Boxplot of distribution of traits in two habitats")


#Checking the data for normality 

#Data are first attached for convenience

attach(wt_data)

shapiro.test(pre_treatment_wt)

shapiro.test(post_treatment_wt)


# Checking the variance homogeneity assumption

wt_data%>%
    gather(treatment,
           value,
           pre_treatment_wt:post_treatment_wt)%>%
    mutate_at('treatment',as.factor)%$%  # this is a operator used from magrittr package
    var.test(value~treatment,
             data = .)


#1. Paired sample t test

paired_t_test<-t.test(pre_treatment_wt,
       post_treatment_wt,
       paired = TRUE)

paired_t_test

#Results of the test

# t statistic

paired_t_test$statistic

# p value

paired_t_test$p.value


detach(wt_data) # detaching after analysis


#2. Independent sample t test 

 # The same data will be used here with different column names so that the results of normality and homogeneity of variance tests (and the summary with visualization) will be the same. 

 # Here the two columns will be weights of a populations of a single species measured at two different locations
 
site_1_wt<-rnorm(n=20,
                 mean=10,
                 sd=0.7)

site_2_wt<-rnorm(n=20,
                 mean=5,
                 sd=0.4)

#Combining the data into a dataframe and attaching it

site_wt_data<-cbind (site_1_wt,
                     site_2_wt)%>%
    data.frame(.) 

attach(site_wt_data)

# t test for independent samples

ind_t_test<-t.test(site_1_wt,
       site_2_wt,
       var.equal = TRUE) # Homogeneity of variance is assumed true for this test


ind_t_test
#Results of the test

# t statistic

ind_t_test$statistic

# p value

ind_t_test$p.value


detach(site_wt_data) # detaching the dataset


# In addition there is one more arugment called 'alternative' which checks whether the  t value is larger, smaller than the value given by the null hypothesis or if the value is simply not equal to the value given by the null hypothesis (www.stat.yale.edu)

