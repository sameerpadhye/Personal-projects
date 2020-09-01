# Load the readr package
require(readr)
require(readxl)
require(gdata)


# URL with csv file
csv_url <- "https://perso.telecom-paristech.fr/eagan/class/igr204/data/cereal.csv"

# Import the file 

data_imported1<-read.csv(csv_url,sep = ';')

head(data_imported1)

# Load the readr package
library(readr)

# Import the file using read_csv(): pools2
pools2<-read_csv(csv_url)

# Print the structure of pools1 and pools2
str(pools1)

str(pools2)



# Load the readxl and gdata package
library(readxl)
library(gdata)


# Specification of url: url_xls
url_xls <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/latitude.xls"

# Import the .xls file with gdata: excel_gdata
excel_gdata<-read.xls(url_xls)

# Download file behind URL, name it local_latitude.xls
download.file(url_xls,"local_latitude.xls")

# Import the local .xls file with readxl: excel_readxl
excel_readxl<-read_excel("local_latitude.xls")


# Load the httr package
library(httr)

# Get the url, save response to resp
url <- "http://www.example.com/"


# Print resp
resp<-GET(url)

# Get the raw content of resp: raw_content
raw_content<-content(resp,as="raw")

# Print the head of raw_content
head(raw_content)


# httr is already loaded

# Get the url
url <- "http://www.omdbapi.com/?apikey=72bc447a&t=Annie+Hall&y=&plot=short&r=json"

resp<-GET(url)
# Print resp
print(resp)

# Print content of resp as text
content(resp,as="text")

# Print content of resp
content(resp)


# jsonlite is preloaded

# Definition of quandl_url
quandl_url <- "https://www.quandl.com/api/v3/datasets/WIKI/FB/data.json?auth_token=i83asDsiWUUyfoypkgMz"

# Import Quandl data: quandl_data
quandl_data<-fromJSON(quandl_url)

# Print structure of quandl_data
str(quandl_data)
