# Power analysis to determine sample sizes for some inferential tests


#libraries used


library(tidyverse)


# Power analysis is performed using functions from the 'pwr' package


if(!require(pwr))install.packages('pwr') 


# Data points for correlation (and simple linear regression) analysis


cor_power<-pwr.r.test(n = NULL, # Since number of samples are to be estimated, its kept null
           r =0.8, # the correlation coefficient (approx. value)
           sig.level =0.01, # statistical significance level
           power = 0.9, # Power of the test
           alternative = "two.sided") # type of alternative hypothesis


# Data points for t tests


pwr.t.test(n = NULL, # number of samples
            d = 0.5, # effect size
            sig.level = 0.01, # statistical significance level
            power = 0.9, # power of the test
           type= "paired") #type of t test


# Data points for ANOVA


pwr.anova.test(k = 4, # number of groups (levels)
               n = NULL, # number of samples
               f = 0.4, # effect size
               sig.level = 0.01, # statistical significance level
               power = 0.9) # power of the test


# Data points for ChiSquare tests


pwr.chisq.test(w = 0.4, # effect size
               N = NULL, 
               df = 4, # degrees of freedom
               sig.level = 0.01, # statistical significance level
               power = 0.9) # power of the test
