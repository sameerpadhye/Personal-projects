##Piping in R

# Data used for showing the use of pipes

pipes_df<-data.frame(x=sample(c(1:100),10,replace=T),
              y=sample(c(1:100),10,replace=T),
              z=sample(c(1:100),10,replace=T))


#1. Normal pipe operator %>% using tidyverse (dplyr) package

pipes_df%>%
    colSums(.) # This corresponds to use the df and take its column sums

pipes_df%>%
    log(.) # This corresponds to use the df and take its log value

pipes_df$x%>%
    mean(.,na.rm = T)  # This corresponds to use the df$x and take its mean


#2. Compound assignment operator %<>% using the package magrittr. 

if(!require(magrittr))install.packages('magrittr') 

# This type is used to update the LHS with a new value using a pipe

# The x vector

pipes_df$x

# Using the pipe

pipes_df$x%<>%mean(.) # This corresponds to use the df$x and update it with its mean

#Checking the changed x vector

pipes_df$x


#3. Exposition pipe operator %$% using the magrittr package 

# Exposition pipe helps in using the data on the LHS to RHS. 
# E.g.Calculating the correlation of x and y specifically

pipes_df%$%cor(y,z) # This corresponds to use the pipes_df and then calculate the correlation of x and y columns from the dataframe


#4. Tee operator %T>% using the magrittr package

#This operator helps in continuing the pipeline even after calling a function such as plot or print

pipes_df[,c(2,3)]%T>%  # This corresponds to use the first two columns of the df and continue using this data for plot and further function
    plot()%>%  
    rowSums(.)


#More information on piping can be found on https://www.datacamp.com/community/tutorials/pipe-r-tutorial
