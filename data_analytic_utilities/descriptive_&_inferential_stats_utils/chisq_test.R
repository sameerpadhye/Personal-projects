#Chisquare test of Independence.
#For information on the test, please refer to https://www.statisticssolutions.com/using-chi-square-statistic-in-research/

#data file path.

paste0(getwd(),'/data_analysis.csv')

#Importing the data. 

data_analysis<-read_excel(data_file_path,
                         sheet=1)%>%
    column_to_rownames(.,'Categories')

#Exploring the dataset

head(data_analysis,5)   

#Visualizing the strength of association of each category with each column
#Data is first converted into a table and then visualized by a balloonplot
#This part has been adpoted from http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r

table_data<-as.table(as.matrix(data_analysis))

library(gplots)

balloonplot(t(table_data), main ="Association strength", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

#Performing the Chisquare test. 

chisq_test<-chisq.test(data_analysis)

#P value of the test can also be calculated by using Monte Carlo Simulation by using the 'simulate.p.value' argument

#Chisquare statistic

chisq_test$p.value

#Chisquare statistic

chisq_test$statistic


#Testing the strength of association using Cramer's V and Phi Coefficient (High values suggests strong association)

if(!require(DescTools))install.packages('DescTools')

DescTools::Phi(table_data)

DescTools::CramerV(table_data)

