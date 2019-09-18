###############################################################
## Basics of Biological Data Handling using R
## 
## Basic Data Analyses in R
##
## Sameer M. Padhye
##
## 
###############################################################

##=============================================
## Correlation and Regression
##=============================================



# We will use the following dataset for correlation and regression

correlation_data<-data.frame(
    var_1=c(10,12,17,29,35,34,56,89,112,156),
    var_2=seq(160,1,length.out = 10), # seq generates a sequence of numbers
    var_3=runif(10),  # runif generates random numbers
    var_4=sample(c(1:200),10,replace = T)) # sample generates random numbers

#Exploring the data

head(correlation_data,3)



#### Correlation

#Obtaining the correlation values

cor(correlation_data)

# 'cor' only gives the correlation coefficient. 

# 'cor.test' provides the t statistic and p values as well. 

# Pearson correlation coefficient (used when data are normal)

cor.test(correlation_data$var_1,
         correlation_data$var_2,
         method=c("pearson")) # this will change based on the type of data




#### Linear regression

# Defining the regression formula

formula_regression<-as.formula("var_1~var_2")

# Fitting the model. 

# The typical formula is y~x where y is the dependent variable and x is the independent variable

model_regression <-  lm(formula_regression, 
                      data = correlation_data)

# Summary of the model

summary_model<-summary(model_regression)

summary_model

# Obtaining the adjusted R squared value (strength of the regression model)

summary_model$adj.r.squared




##=============================================
## t test
##=============================================



#### Paired t test

# Data 

# The data here represents a pre and post treatment weight of an organism exposed to a certain chemical

paired_t_data<-data.frame(pre_treatment_wt=rnorm(n=20,
                                                  mean=10,
                                                  sd=0.7),
                          # rnrom is a function used to generate a numerical vector of a certain mean and standard deviation
                          post_treatment_wt=rnorm(n=20,
                                                   mean=5,
                                                   sd=0.4))
#Viewing the data

head(paired_t_data,3)

#Summarizing the data

summary(paired_t_data)

#Checking the data for normality 

attach(paired_t_data)

shapiro.test(pre_treatment_wt)

shapiro.test(post_treatment_wt)

# Performing the t test

paired_t_test<-t.test(pre_treatment_wt,
                      post_treatment_wt,
                      paired = T)

#Results of the test

# t statistic

paired_t_test$statistic

# p value

paired_t_test$p.value

detach(paired_t_data) # detaching the dataset




#### Independent sample t test

# Here the two columns will be weights of 2 populations of a single species measured at two different locations

independent_t_data<-data.frame(site_1_wt=rnorm(n=20,
                 mean=15,
                 sd=0.7),
                 site_2_wt=rnorm(n=20,
                 mean=9,
                 sd=0.4))

#Viewing the data

head(independent_t_data,3)

#Summarizing the data

summary(independent_t_data)

#Checking the data for normality 

attach(independent_t_data)

shapiro.test(site_1_wt)

shapiro.test(site_2_wt)

# Performing the t test for independent samples

ind_t_test<-t.test(site_1_wt,
                   site_2_wt,
                   var.equal = T) # Homogeneity of variance is assumed true for this test

#Results of the test

# t statistic

ind_t_test$statistic

# p value

ind_t_test$p.value

detach(independent_t_data) # detaching the dataset




##=============================================
## One way ANOVA
##=============================================



#Data for ANOVA

anova_data<-data.frame(
    habitat=rep(c("Habitat_1",
                  "Habitat_2",
                  "Habitat_3"),
                each=20), # rep is used repeat a sequence 
    body_wt=rnorm(60,mean=30,
                  sd=6))

#Converting the habitat into a factor

anova_data$habitat<-as.factor(anova_data$habitat)

#Viewing the data

head(anova_data,3)

# Viewing the levels of the Factor

levels(anova_data$habitat)

#Summarizing the data

summary(anova_data)

#Checking the data for normality 

shapiro.test(anova_data$body_wt)

#Performing ANOVA 

attach(anova_data)

anova_data<-aov(body_wt~habitat)

summary(anova_data)

#Tukey Posthoc test

TukeyHSD(anova_data)

detach(anova_data)




##=============================================
## Chi square test (test of independence)
##=============================================

chi_sq_data<-data.frame(Category=c("Family_1",
                                   "Family_2",
                                   "Family_3",
                                   "Family_4"),
                        site_1=sample(60:120,
                                      4,
                                      replace = TRUE),
                        site_2=sample(20:50,
                                      4,
                                      replace = TRUE),
                        site_3=sample(200:350,
                                      4,
                                      replace = TRUE),
                        site_4=sample(5:10,
                                      4,
                                      replace = TRUE))


#Exploring the dataset

head(chi_sq_data)   

#Performing the Chisquare test. 

chisq_test<-chisq.test(subset(chi_sq_data,
                              select=site_1:site_4),
                       y=chi_sq_data$Category)

#results

#Chisquare statistic

chisq_test$statistic

#Chisquare statistic

chisq_test$p.value





##=============================================
## Non Parametric tests
##=============================================



#### Mann Whitney U test

# Data

# the data here represents a hypothetical data of 10 (same) species abundances from two different sites

site_1<-data.frame(site=rep("site_1",10),
                   abundance=sample(25:85,10))

site_2<-data.frame(site=rep("site_2",10),
                   abundance=sample(90:200,10))

#Combining the data of both the sites to get the final dataset

man_whit_data<-rbind(site_1,
                     site_2)
    
# Viewing the data

head(man_whit_data,5)

#Checking the data for normality 

shapiro.test(site_1$abundance)

shapiro.test(site_2$abundance)

#Test

mann_whitney_test<-wilcox.test(abundance~site,
                               data = man_whit_data)

#Results

mann_whitney_test




#### Wilcoxon signed rank test

# Data

# the data here represents a hypothetical data of species abundances from a single site before and after a change in the pH value

before<-data.frame(period=rep("before",10),
                   abund_before=sample(120:250,10))

after<-data.frame(period=rep("after",10),
                   abund_after=sample(45:95,10))

#Combining the data of both the sites to get the final dataset

wilcox_data<-cbind(before,
                     after)

# Viewing the data

head(wilcox_data,5)

#Checking the data for normality 

shapiro.test(before$abund_before)

shapiro.test(after$abund_after)

#Test

wilcox_test<-wilcox.test(wilcox_data$abund_before,
                         wilcox_data$abund_after,
                         paired = TRUE)

#Results

wilcox_test




#### Kruskal Wallis test

# Here the data based on the data generated for ANOVA will be used for convenience

kruskal_data<-data.frame(
    habitat=rep(c("Habitat_1",
                  "Habitat_2",
                  "Habitat_3"),
                each=20), # rep is used repeat a sequence 
    body_wt=sample(50:500,60))

#View the data

head(kruskal_data,5)


#It is assumed that data are not normal before conducting the Kruskal Wallis test

#Kruskal test

kruskal.test(body_wt ~ habitat, 
             data = kruskal_data)


