# Kruskal Wallis test 


## data used for analysis


data_analysis<-data.frame(Habitats=rep(c("Habitat_1",
                                   "Habitat_2",
                                   "Habitat_3",
                                "Habitat_4"),
                                 each=20),
    species_data=sample(c(100:500),80,replace = TRUE))


# View the data


head(data_analysis,5)


#Visualizing the data


data_analysis%>%
    ggplot(aes(x=Habitats,
               y=species_data))+
    geom_boxplot(fill='steelblue')+
    theme_bw(base_size = 18)+
    ggtitle("Boxplot of species data")


# Checking the normality of the data (between the four habitats). For convenience, data is attached for the test and then detached


attach(data_analysis)


tapply(species_data,Habitats,shapiro.test)


detach(data_analysis)


# Kruskal Wallis test


kruskal.test(species_data ~ Habitats, 
             data = data_analysis)


# Post hoc tests using the package FSA


if(!require(FSA))install.packages('FSA') 


dunnTest(species_data ~ Habitats, 
         data = data_analysis,
         method="bh")
