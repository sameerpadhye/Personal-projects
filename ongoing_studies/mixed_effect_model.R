#libraries used

library(lme4)
library(AICcmodavg)
library(readxl)
if(!require(expss))install.packages('expss') 

# Data file path (It is assumed that the sample data is saved in the working directory and saved as an excel file)

data_path<-"C:/Users/samee/Desktop/R data/sample_datasets/mixed_effect_model.xlsx"


#Importing data for analysis (Species abundance dataset used here). 

data_analysis<-read_excel(data_path,
                          sheet=2)
    # dplyr::mutate(water_level=rep(c("LWL","HWL"),each=28))

 #Modifying the dataset (Original data with separate columns for each water level treatment so data modified for analysis in steps)

#1. separating the dataset into water level treatment

 data_LWL<-data_analysis[,c(1,2)]

 data_HWL<-data_analysis[,c(3,4)]
 

#2. Combining the two by rows to get a long dataframe (This way wont generate any NA's)
 
library(data.table)

long_dataset<-rbindlist(list(data_LWL,data_HWL))%>%
    as.data.frame()


#3. Adding the two water level factors to the long data

long_dataset<-long_dataset%>%
    dplyr::mutate(water_level=rep(c("LWL","HWL"),
                                  each=30))%>%
    dplyr::mutate_if(is.character,
                     as.factor)%>%
    dplyr::rename(pred_id=Predator_id,
                  time_taken=Time_attack)

str(long_dataset)

#1. Experiment 1

# using mixed effect models for analysis

gamma_fit <- glmer(time_taken ~ water_level + (1 | pred_id), 
                   family = "Gamma",
                   data=long_dataset)
summary(gamma_fit)
norm_fit <- glmer(time_taken ~ water_level + (1 | pred_id), 
                  family = "gaussian",
                  data=long_dataset
                  )
summary(norm_fit)
#AICc is smaller, therefore this model provides a better fit

paste0('AICc gamma fit value:', AICc(gamma_fit))

paste0('AICc gamma fit value:', AICc(norm_fit))


#We can also inspect the residuals to see if we see any patterns. The ideal is to observe dots randomly distributed

plot(gamma_fit)

plot(norm_fit)


#Now lets actually test for the effect of water level in time to atack.

#We first fit a model with no effect of treatment

gamma_fit_noeffect <- glmer(time_taken ~ 1 + (1 | pred_id), 
                            family = "Gamma",
                            data=long_dataset) 


#We then fit another with the effect.

gamma_fit_all<- glmer(time_taken ~ water_level + (1 | pred_id), 
                      family = "Gamma",
                      data=long_dataset) 


#We test whether the model that includes the effect provides a significantly better fit to the data.

anova_Time <- anova(gamma_fit_noeffect,
                    gamma_fit_all, 
                    test = T)
anova_Time


#You can use summary to look at coeffitients to interpret if the effect is positive or negative.

summary(gamma_fit_all)

# https://stackoverflow.com/questions/26417005/odds-ratio-and-confidence-intervals-from-glmer-output for obtaining the confidence intervals for 'glmer'analysis (less lines of code)

if(!require(broom.mixed))install.packages('broom.mixed') 

broom.mixed::tidy(gamma_fit,conf.int=TRUE,exponentiate=TRUE,effects='fixed')



#Is there a difference in the proportion of atacks between species in different water levels?

data_analysis2<-read_excel(data_path,
                          sheet=2)

#1. separating the dataset into water level treatment

data_LWL2<-data_analysis2[,c(1:3)]

data_HWL2<-data_analysis2[,c(4:6)]


#2. Combining the two by rows to get a long dataframe (This way wont generate any NA's)

library(data.table)

long_dataset2<-rbindlist(list(data_LWL2,data_HWL2))%>%
    as.data.frame()

names(long_dataset2)
#3. Adding the two water level factors to the long data

long_dataset2<-long_dataset2%>%
    dplyr::mutate(water_level=rep(c("LWL","HWL"),
                                  each=28))%>%
    dplyr::mutate_if(is.character,
                     as.factor)%>%
    dplyr::rename(pred_id=Predator_id,
                  time_taken=time)

# I modified your dataset based on what the reviewer gave in the code , "#Here we only need the number of "successes" in catching one of the prey choices, since the other one would be 1 - proportion of catches of the first."
# So 1 was for C and hence 0 would be for M

long_dataset2<-long_dataset2%>%
    dplyr::mutate(sp_pref=ifelse(species=="C",1,0))

#Same approach as before

fit_binom <- glmer(sp_pref ~ water_level + (1 | pred_id), 
                   family = "binomial",
                   data=long_dataset2)
summary(fit_binom)
fit_norm_prob <- lmer(sp_pref ~ water_level + (1 | pred_id),
                      data=long_dataset2)
summary(fit_norm_prob)

AICc(fit_norm_prob)

AICc(fit_binom)#Better


no_eff_binom <- glmer(sp_pref ~ 1 + (1 | pred_id), 
                      family = "binomial",
                      data=long_dataset2)

fit_binom <- glmer(sp_pref ~ water_level + (1 | pred_id), 
                   family = "binomial",
                   data=long_dataset2)

anova(no_eff_binom, fit_binom)#Effect of treatments!


summary(fit_binom)#looking at the estimate we see that the low water level treatment has a smaler estimate (smaller time).

library(car)

car::Anova(fit_binom)

#Code for the function logit2prob
source("https://sebastiansauer.github.io/Rcode/logit2prob.R")

#To interpret coeffitients in the scale of your response variable
logit2prob(summary(fit_binom)$coef[1,1])#High Water Level estimate
logit2prob(summary(fit_binom)$coef[1,1]  + (1.96*summary(fit_binom)$coef[1,2]))#High Water Level upper confidence interval at 95%
logit2prob(summary(fit_binom)$coef[1,1]  - (1.96*summary(fit_binom)$coef[1,2]))#High Water Level lower confidence interval at 95%

logit2prob(summary(fit_binom)$coef[1,1]  + summary(fit_binom)$coef[2,1])#Low Water Level estimate
logit2prob(summary(fit_binom)$coef[1,1]  + summary(fit_binom)$coef[2,1] + (1.96*summary(fit_binom)$coef[2,2]))#Low Water Level upper confidence interval at 95%
logit2prob(summary(fit_binom)$coef[1,1]  + summary(fit_binom)$coef[2,1] - (1.96*summary(fit_binom)$coef[2,2]))#Low Water Level lower confidence interval at 95%
