#  Comparing proportions using a Z test

#### One sample Proportion Z test

counts_before<-120

counts_after<- 55


# One sample proportion test

one_sample_prop_test<-prop.test(counts_after, # success
          counts_before, # total counts
          alternative = 'two.sided') # Alternative hypothesis type

# Overall results

one_sample_prop_test

# Specific results of the test

#1. Chisq statistic

one_sample_prop_test$statistic

#2. p value

one_sample_prop_test$p.value

#3. estimates (esitmated probability values of successes) 

one_sample_prop_test$estimate



#### Multiple samples proportions Z test

# Data for the test

sp_counts_before<-c(85,55,50,65)

sp_counts_treatment<-c(40,24,18,35)

# Multiple samples Proportion test

multi_samples_prop_test<-prop.test(sp_counts_treatment, # vector of successes
          sp_counts_before,  # total counts
          alternative = "two.sided", # type of alternative hypothesis
          conf.level = 0.95) # confidence level of the confidence interval

# Overall result

multi_samples_prop_test

# Specific results

#1. Chisq statistic

multi_samples_prop_test$statistic

#2. p value

multi_samples_prop_test$p.value

#3. estimates (esitmated probability values of successes) 

multi_samples_prop_test$estimate


