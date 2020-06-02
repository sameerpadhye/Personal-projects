# Single sample Z test


install.packages('BSDA')
library(BSDA)

body_weight<-c(56,76.5,98.2,88,94,68,66,85)

plot(body_weight,cex=.9)

mean_1<-mean(body_weight)

z.test(body_weight,
       sigma.x = 75,
       alternative = "two.sided")

# Two sample Z test

body_weight2<-c(60,66.5,78.2,58,84,56.3,63,75)

mean_2<-mean(body_weight2)

diff_mean<-mean_1-mean_2

z.test(body_weight,
       body_weight2,
       sigma.x = 70,
       sigma.y=60,
       mu=10,
       alternative = "two.sided")
