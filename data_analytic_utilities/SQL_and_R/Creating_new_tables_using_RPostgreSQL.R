#Creating a database and a table using a data file on the local machine by RPostgreSQL. 

#The steps used are similar to that of RMySQL with the exception of the libraries used.

# For PostgreSQL, RPostgreSQL package is used

#libraries

library(DBI)
library(RPostgreSQL)

#Establishing a connection

connect_data<-DBI::dbConnect(PostgreSQL(), 
                             user='abcd', #provide the ID
                             password='', #provide the password 
                             host='localhost')  


#The rest of the steps are as per RMySQL (Still the code is provided below for convenience)

#Creating a new database

dbSendQuery(connect_data, 
            "CREATE DATABASE database_name;")


#selecting the newly created database

dbSendQuery(connect_data, 
            "USE database_name")


#Creating a table within the newly created database appending a downloaded csv file (here assumed to be downloaded in the working directory). In case the file has been downloaded somewhere else, appropriate path should be given

file_path<-paste0(getwd(),"/data_file.csv")

data_file<-read.csv(file_path)


#Alternatively, the file can also be downloaded and then used in the table. Here, the file is downloaded using the 'fread' function in data.table library

if(!require(data.table))install.packages('data.table') 

data_file <- fread('http://www.xyz.com/datasets/data_file.csv') #provide the path

#Here the demographics data is used which can be downloaded from kaggle

#Writing the table to the database. Append argument is used here to add the csv data to the empty table.

dbWriteTable(connect_data,name = 'demographics',value=data_file,append=TRUE)


#Checking the fields (columns) of the table

dbListFields(connect_data,'demographics')


#Querying the table

query_1<-dbSendQuery(data_connection,'select * from demographics limit 5')

dbFetch(query_1,n=5)


#Clearing the result (this should always be done after work)

dbClearResult(query_1)


#Disconnecting the connection (this should always be done after work)

dbDisconnect(connect_data)


