##Converting distance object to a dataframe

# Common libraries used

require(tidyverse)

# Sample data

data_for_analysis<-data.frame(a_col=c(1:10),
                              b_col=c(11:20),
                              c_col=c(21:30),
                              d_col=c(31:40))

# Converting the data into distance

dist_data<-dist(data_for_analysis,
                method = "euclidean")

dist_data

# Converting the distance data into a dataframe using reshape2 package

require(reshape2)

dist_data_df<-dist_data%>%
    as.matrix(.)%>%
    reshape2::melt(.,varnames=c("Col_from",
                                "Col_to"))

head(dist_data_df)

# The resulting dataframe gives three columns with first two being the name of the pair of which the distance is calculated while the third column gives the distance value

# Converting distance to dataframe using otuSummary package

require(otuSummary)

dist_data2<-dist(data_for_analysis,
                 method = "euclidean",
                 upper = FALSE)

dist_df<-otuSummary::matrixConvert(dist_data2, 
              colname = c("col1", 
                          "col2", 
                          "dist_val"))

dist_df
