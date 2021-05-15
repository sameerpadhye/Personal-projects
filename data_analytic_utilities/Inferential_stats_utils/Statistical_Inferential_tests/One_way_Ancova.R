##========================================================
## One-way ANCOVA
##========================================================

# Importing the data

one_way_ancova_data<-read_excel(file.choose(),
                                sheet=1)

# Structure of data

str(one_way_ancova_data)

# Visualizing the data

ggboxplot(one_way_ancova_data, 
          x = "Category_level1", 
          y = "weight", 
          color = "orangered")

#To carry out an one way ANCOVA, the formula is written as 

#dependent variable~independent variable(categorical)+indepedent covariate

#ANOVA model a name e.g. anovaD and use summary() to see the output.

one_way_ancova<-aov(weight~Category_level1+height,data=one_way_ancova_data)

# Observing the results using car package and type II or III sums of squares calculations

library(car)

Anova(one_way_ancova, type="III")
