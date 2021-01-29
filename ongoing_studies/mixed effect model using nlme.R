library(nlme)
data(Oats)
str(Oats)
plots(Oats)

#linear model
model1=lm(yield~Variety*nitro,data=Oats)
summary(model1)
coef(model1)

#mixed effect
model2=lme(yield~Variety*nitro,data=Oats,random=~1|Block/Variety)
summary(model2)
coef(model2)

# to check for residuals if there is pattern of dispersion
plot(model2)
# to check if the random effects are distributed randomly
plot(ranef(model2))
