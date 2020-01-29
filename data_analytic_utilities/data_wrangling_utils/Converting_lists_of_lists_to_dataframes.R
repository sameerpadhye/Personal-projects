## Converting a list of lists to a data.frame


# Common libraries used

require(tidyverse)

# A sample list of lists

list_all<-list(a=list(1,2,3),b=list(6,7,8),c=list(9,10,11))

names(list_all[[1]])

class(list_all)          


# Converting lists of lists to dataframes


#1. Using purrr package

require(purrr)

map_trans_list<-map_df(list_all,
       data.frame)
    
View(map_trans_list)

# Please note that the dataframe may contain NA's depending on the elements within the lists


#2. Using do.call() function

docall_trans_list<-do.call(rbind,
                    list_all)%>%
    data.frame(.)

View(docall_trans_list)
    
# Here the the elements of each list(here 3) are bound row-wise and since the number of elements in each list are same, there are no NA values in the df


#3. Using ldply function from plyr package

require(plyr)

plyr_trans_list<-plyr::ldply(list_all,
                         data.frame)


View(plyr_trans_list)

# This gives the same result as purrr package gives with NA values since the individual elements of the lists within the master list are not names


#4. Using purrr package and do.call

comno_trans_list<-map(list_all,
                      unlist)%>%
    do.call(rbind,.)

View(comno_trans_list)

