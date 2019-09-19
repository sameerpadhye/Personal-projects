###############################################################
## Basics of Biological Data Handling using R
##
## Data Visualization with base R
##
## Sameer M. Padhye
##
## 
###############################################################




#### Histogram 


#Data


hist_data <- data.frame(
    habitat=factor(rep(c("A","B","C"), each=300)),
    sp_count=round(c(rnorm(300, mean=40, sd=4), 
                     rnorm(300, mean=45, sd=4.5),
                     rnorm(300, mean=52, sd=5.5)))
)


#View the data


head(hist_data)


# Plot


hist(hist_data$sp_count,
     breaks = 5,
main="Histogram",
xlab="Species count",
ylab = "Counts",
col='orange')




#### Barchart


# Data


barchart_data <- data.frame(population=c("pop_A", "pop_B", "pop_C"),
                            avg_length=c(15.5,17.5, 19.5))



#View the data


head(barchart_data)


#Plot


barplot(barchart_data$avg_length,
        ylab="Avgerage length",
        main="Barplot",
        col="grey",
        names.arg=c("Population A", 
                    "Population B", 
                    "Population C"))




#### Piechart


# Data used for barchart will be used here


# Plot


pie(barchart_data$avg_length,
    labels = barchart_data$population, 
    main= "Piechart",
    col = c("forestgreen","blue","grey"))




#### Scatterplot and Line chart


# Correlation and Regression data will be used here


correlation_data<-data.frame(
    var_1=c(10,12,17,29,35,34,56,89,112,156),
    var_2=seq(160,1,length.out = 10), 
    var_3=runif(10),  
    var_4=sample(c(1:200),10,replace = T)) 


#Scatterplot 


plot(correlation_data$var_1,
     correlation_data$var_2,
     xlab = "variable 1",
     ylab = "variable 2",
     main="Scatterplot",
     pch=19,
     col="black",
     cex=1.5)


#Adding a regression line


abline(lm(var_2 ~ var_1, data = correlation_data), col = "blue")


# Line chart


plot(correlation_data$var_1,
     correlation_data$var_2,
     xlab = "variable 1",
     ylab = "variable 2",
     main="Linechart",
     type = "b", 
     pch = 19, 
     col = "steelblue",
     cex=1.4)




#### Box plots


# Here, we will use the histogram dataset (hist_data)


#Plot


boxplot(sp_count ~ habitat, 
        data = hist_data, 
        frame = TRUE,
        main='Boxplot',
        names=c("Habitat A","Habitat B","Habitat C"),
        col=c("grey50","orange","steelblue"),
        horizontal = TRUE)





