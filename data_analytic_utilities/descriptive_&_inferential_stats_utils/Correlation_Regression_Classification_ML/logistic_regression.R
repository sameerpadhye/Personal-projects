##Using simple logistic regression 


# Libraries used

library(psych)
library(QuantPsyc)
library(tidyverse)
library(magrittr)
library(car)
library(ppcor)
library(caret)


# Data file path (Data file is assumed to be in the working directory)


data_file<- paste0(getwd(),"/log_regression_sample_data.csv")


# Importing the data

log_reg_data<-readr::read_csv(data_file)


# Exploring the data (structure)

str(log_reg_data)

head(log_reg_data,3)


# Re-formatting the data for analysis 

data_for_analysis<-log_reg_data%>%
  dplyr::mutate_at(vars(matches("predictor_1")),
                   as.factor)


# Exploring the modified data

head(data_for_analysis,3)


# Checking for multicollinearity in the environmental descriptors

collinearity_var<-data_for_analysis%>%
  dplyr::select(predictor_3,predictor_4:predictor_6)%>%
  pcor(.,method = "spearman")%>%
  .$estimate

collinearity_var


# Since none of the environmental variables are collinear, all of them are used in the regression analysis


# Plots to check the distribution of the predictors using the psych package

library(psych)

str(data_for_analysis)

# plot with raw values

psych::multi.hist(subset(data_for_analysis,
                         select = predictor_2:predictor_7))

# It can be observed that some of the predictors have a skewed distribution and hence need to be transformed (Here 4 and 7 used)

# Log (x+1) transformation

data_for_analysis_trans<-data_for_analysis%>%
  mutate_at(vars(4,7),
            log1p)

# Plotting the above data

psych::multi.hist(subset(data_for_analysis_trans,
                         select = predictor_2:predictor_7))


# Plots (boxplot) to check how environmental variables fare in terms of presenceabsence of the species (predictor_1 not used here)

data_for_analysis%>%
  dplyr::select(-predictor_1)%>%
  dplyr::mutate(sp_occ=ifelse(Species_A=='0',#Species name#
                              'absent',
                              'present'))%>%
  gather(env_var,
         value,
         predictor_3,
         predictor_4:predictor_6)%>%
  mutate_at(vars(matches('env_var')),
            as.factor)%>%
  ggplot(aes(x=sp_occ,
             y=value))+
  geom_boxplot(fill="grey")+
  facet_wrap(~env_var,scales = "free")+
  theme_bw(base_size = 16)+
  xlab('Species occurrence')+
  ylab('values')


# Defining the formula for the regression

reg.formula<-reformulate(termlabels = paste(names(data_for_analysis%>%
                                                    subset(.,select=-c(Species_A,predictor_1)))), response = 'Species_A')


# Logistic regression using all environmental descriptors

logistic_model<-glm(reg.formula,
                    data=data_for_analysis,
                    family=binomial)


# Summary of the model

summary(logistic_model)


# Obtaining the odds ratios of the descriptors

exp(coef(logistic_model))


# Classification of the cases (here species presence and absence) based on the model

QuantPsyc::ClassLog(logistic_model,data_for_analysis$Species_A)


# Confusion matrix for the model using regclass package


if(!require(regclass))install.packages('regclass')


confusion_matrix(logistic_model)


# Obtaining a list of most important descriptors (sequentially) using the 'varImp' function in 'caret'package

caret::varImp(logistic_model)


# Using 'glance' from broom package for obtaining the model summary


if(!require(broom))install.packages('broom')


model_summary<-broom::glance(logistic_model)


model_summary


# Obtaining the pseudo R2 of the model based on the summary values obtained above


pseudoR2_model <- 1 - model_summary$deviance/model_summary$null.deviance

pseudoR2_model


# Making predictions based on the model. (type = 'response' should be added to obtain predicted probabilities)


data_for_analysis$predicted_val <- predict(logistic_model,type='response')


# Plotting the gainplot to check the efficiency of the model


if(!require(WVPlots))install.packages('WVPlots')


GainCurvePlot(data_for_analysis,'predicted_val' ,'Species_A', "logistic_model")
