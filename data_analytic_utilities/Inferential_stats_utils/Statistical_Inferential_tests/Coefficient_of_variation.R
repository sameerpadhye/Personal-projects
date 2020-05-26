######## Coefficient of variation (CV) #######################################


#install.packages('MBESS')

library(tidyverse)
library(goeveg)
library(MBESS)

# Data for analysis

cv_df<-data.frame(col_a=sample(c(1:100),size = 50,replace = TRUE),
                  col_b=sample(c(1:500),size = 50,replace = TRUE),
                  col_c=sample(c(1:800),size = 50,replace = TRUE))

# View data

head(cv_df)


# Calculating the coefficient of variation manually. The formula for it is sd/mean

mean_val<-mean(cv_df$col_a,na.rm = T)

sd_val<-sd(cv_df$col_a,na.rm = T)

CV_val<-sd_val/mean_val

CV_val

# Coefficient of variation using the package goeveg

#install.packages('goeveg')

library(goeveg)

goeveg::cv(cv_df$col_a,na.rm = T)

goeveg::cv(cv_df$col_b,na.rm = T)

goeveg::cv(cv_df$col_c,na.rm = T)

