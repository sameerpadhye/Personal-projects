###############################################################
## Basics of R
##
## 
## Sameer M. Padhye
##
## 
###############################################################



##========================================================
## Simple calculations in R without defining a variable/s
##========================================================



2+3

5/3



##========================================================
## Defining a variable 
##========================================================



x<-5  #x is the name of the variable which is assigned a value of 5 using the '<-' operator. 
#It is sometimes used interchangbly with '=' operator as well (Their use differ slightly in usage in some instances).


#Any type of data can be assigned to a variable. 



##========================================================
## Data types in R
##========================================================



#There are 5 basic data types in R


####1. Numeric


a<-5.6
a
class(x)


####2. Integer


b<-3L
b
class(b)


####3. Character


c<-"Zoology"
c
class(c)


####4. Logical


d<-logical(length = 3L)
d
class(d)


# There are two more types called raw and complex which are not commonly used (wont be covered here)



##========================================================
## Data structures in R
##========================================================



####1. Vector


#Defining a vector: Vectors are defined by using c() which means combine. Vectors can only have one data type.


vector_1<-c(1,2,3,4,5)

vector_2<-c("a","b","c")


# Obtaining the class of vector


class(vectors)

class(vector_2)


#Obtaining the type of vector


typeof(vectors)

typeof(vectors_2)


#Vectors can be used for performing basic mathematical operations


vector_1 + 6 # The value 6 will be added to each of the element of the vector


#indexing the vector (Finding the vector based on its position)


vector_1[2] # first position

#OR

vector_2[1:2] # first two positions


# Vectors are of two types based on the data they contain viz. atomic vectors (where the data is only of one type) or lists (where data can be of different types)



####2. Matrix


#Defining a Matrix


a_matrix<-matrix(data = c(1:9), # specify the data
                 ncol=3,   # specify the number of columns 
                 nrow = 3,  # specify the number of rows 
                 byrow = TRUE) # the data are filled by rows

a_matrix


# Obtaining the class of matrix


class(a_matrix)


#Obtaining the type of matrix


typeof(a_matrix)


#Number of elements


length(a_matrix)


# To check the dimensions of the matrix


dim(a_matrix)


#Accessing the elements of a matrix


a_matrix[2,3]  # The first value signifies the row and the second the column

a_matrix[2,]  # The first row of the matrix


#Matrices are always 2 dimensional with rows and columns and of only one data type.



####3. Array


#Arrays are also a type of data structure which are multidimensional matrices

a<-c(1:5)
b<-c(6:12)

a_array<-array(data = c(a,b),dim=c(2,3,2))

a_array


# To check the dimensions of the array


dim(a_array)



####4. Data frame


#Dataframes are similar to matrices where the data are stored in rows and columns but in dataframes different types of data can be stored.

#Defining a dataframe


a_dataframe<-data.frame(a=c(4,5,6,7,8),
                     b=c("d","e","f","g","h"),
                     d=c(TRUE,FALSE,FALSE,TRUE,TRUE))
a_dataframe


# Obtaining the class of dataframe


class(a_dataframe)


#Obtaining the type of dataframe


typeof(a_dataframe)


#Number of elements


length(a_dataframe)


#Here the elements of the dataframe are the columns unlike matrices wherein each value is a separate element


#Accessing the elements of a dataframe


#a. by position


a_dataframe[2,3]  # The first value signifies the row and the second the column

a_dataframe[2,]  # The first row of the dataframe


#b. by name


a_dataframe[c("a","b")]  # selecting  only first two columns (with all rows)


#c. accessing by using the $ sign


a_dataframe$b


#Selecting a specific element of a single column


a_dataframe$a[3]  # Third cell in the 'a' column

a_dataframe[2,"b"]  #  second cell in the 'b' column




####5. List


#Defining a list


a_list<-list(a=c(1:50),
                        b=c("d","e","f","g"),
                        d=c(TRUE,FALSE,TRUE))

a_list


# Obtaining the class of list


class(a_list)


#Obtaining the type of list


typeof(a_list)


#Number of elements


length(a_list)


#Accessing specific elements of a list


#a. by position


a_list[1] 


#b by name


a_list["a"] 


#c accessing by using the $ sign


a_list$a


# Accessing the specific cells of the elements of the list
 

a_list$a[[3]]  # accessing the thirdd element of the vector 'a' in the list

#OR

a_list[["a"]][3]


#for accessing the datatype of each element of the list '[[]]' are used. If not used properly, it can throw up errors during analysis
#E.g.


#a. Instance 1

a_list[1] * 5

class(a_list[1])


#b. Instance 2

a_list[[1]] * 5

class(a_list[[1]])


#Here, like the dataframe the elements of the dataframe are the unique datatypes stored in the list. Dataframes are a type of list. Data in lists are not organized into rows and columns and can be of different data types.
#Lists are the most felxible of the data structures



####5. Factors


#These are nominal variables (character data like names) with a assinged integer value. The value represents a weight to that character. 


#Good examples would the Grades obtained in an exam. E.g. 'O' grade signifies higher marks than 'A' grade


#Here the same data used in dataframes is used


data_for_factors<-data.frame(a=c(4,5,6,7,8),
           b=c("d","e","f","g","h"),
           d=c(TRUE,FALSE,FALSE,TRUE,TRUE))


# Checking the class and type of the specific 'b' column of dataframe


class(data_for_factors$b)


typeof(data_for_factors$b)


#Checking the levels of factors (these are the values assigned by R to the characters. In case of alphabets, R assigns values based on alphabetic sequence hence d will be 1 followed by e with 2 and so in)


levels(data_for_factors$b)


##========================================================
## Missing values, NANs, Inf
##========================================================


#Missing value: represented as NA in R


missing_val<-c(1,2,NA,5,6,NA)

missing_val


#NA values are present for each of the data type in R


#NANs: In R this means the value is Not A Number


0/0


#Infinity value


1/0


##========================================================
## Install packages in R (requires internet connection)
##========================================================


install.packages('package_name')


#calling the library 


library(package_name)


#Obtaining help for a specific function


help(mean)


# OR 


?mean


