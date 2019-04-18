##Performing PERMANOVA in R

# PERMANOVA is used to analyse significance between groups based on many dependent variables which might not be normally distributed

# generating a random dataset for analysis
data_for_analysis<-data.frame(
    Trait_1=runif(30),
    Trait_2=sample(1:50,30,replace=T),
    Trait_3=rnorm(30,mean=15,sd=4),
    factor_A=as.factor(rep(c("A","B","C"),each=10)))

# Exploring the structure 
str(data_for_analysis)


# Checking multivariate spread of data is necessary before conducting the analysis even though the data are not normal.
# For conducting that analysis, the dataframe first needs to converted into a distance object.
# Here, Gower distance index is used but users can select different indices based on their requirement.

data_beta<-vegdist(subset(data_for_analysis,select=-factor_A), 
                       method = "gower",
                       binary = T,
                       na.rm = T)

#Beta dispersion to check the multivariate spread (dispersion) of data using the groups within the data (here factor_A)

data_betadis<-betadisper(data_beta, 
                         data_for_analysis$factor_A)%>%
    anova(.)

#results of the beta dispersion
data_betadis$`Pr(>F)`

#If the p value is more than 0.05, null hypothesis cannot be rejected (which means data have a homogeneous multivariate spread (equivalent to homogeneity of variances))

#PERMANOVA (Distance index and permutation number can be changed as per requirement)

data_permanova<-adonis(subset(data_for_analysis,select=-factor_A) ~ factor_A,
                           data = data_for_analysis, 
                           permutations = 5000, 
                           method = "gower")

#PERMANOVA results
data_permanova$aov.tab

