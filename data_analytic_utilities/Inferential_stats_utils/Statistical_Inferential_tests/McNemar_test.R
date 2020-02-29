##McNemar test in R

#Information on McNemar test can be obtained from the following online resources
#https://www.statisticshowto.datasciencecentral.com/mcnemar-test/#targetText=In%20order%20to%20run%20a,not%20(%E2%80%9Cno%E2%80%9D).&targetText=SPSS%3A%20The%20binomial%20distribution%20is%20used%20for%20the%20McNemar%20test.

#https://rcompanion.org/handbook/H_05.html


## Data

# dataset generated is a 2 X 2 matrix of occurrence (presence/absence) of a species_A in two localities. The values are of species present or absent
 

data_analysis<-matrix(c(56, 98, 162, 232),
                      nrow = 2,
                      dimnames = list("Locality_1" = c("Present", "Absent"),
                                      "Locality_2" = c("Present", "Absent")))

#View the data


data_analysis


#Test


# McNemar Test is performed using the function mcnemar.test from the base R package 'stats'


mcnemar.test(data_analysis)


# Results of the McNemar Test can be visualized using the function 'showMcNemarTest' from the package mcStats. The input is the dataset loaded


if(!require(mcStats))install.packages('mcStats') 


#plot


showMcNemarTest(data_analysis)
