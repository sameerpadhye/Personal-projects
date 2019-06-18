#Creating a database and a table using a data file on the local machine by RMySQL. 


#libraries

library(DBI)
library(RMySQL)
library(dplyr)
library(data.table)

#Establishing a connection

connect_data<-DBI::dbConnect(MySQL(), 
                               user='abcd', #provide the ID
                               password='', #provide the password 
                               host='localhost')  


#After opening the connection, a new database can be created

dbSendQuery(connect_data, 
             "CREATE DATABASE database_name;")

#selecting the newly created database

dbSendQuery(connect_data, 
            "USE database_name")


#Creating a table within the newly created database appending a downloaded csv file (here assumed to be downloaded in the working directory). In case the file has been downloaded somewhere else, appropriate path should be given

file_path<-paste0(getwd(),"/data_file.csv")

data_file<-read.csv(file_path)


#Alternatively, the file can also be downloaded and then used in the table. Here, the file is downloaded using the 'fread' function in data.table library

data_file <- fread('http://www.xyz.com/datasets/data_file.csv') #provide the path


#Writing the table to the database. Append argument is used here to add the csv data to the empty table.

dbWriteTable(connect_data,name = 'data_table_name',value=data_file,append=TRUE)


#Checking the fields (columns) of the table

dbListFields(connect_data,'demographics')


#Querying the table

query_1<-dbSendQuery(connect_data,'select * from demographics')

dbFetch(query_1,n=5)


#Disconnecting the connection

dbDisconnect(connect_data)


