## Beeswarm plot to visualize the distribution of data where all the sample points are plotted 

#package for plotting
library(beeswarm)

#sample dataset
data(mtcars)

#number of categories for plotting (colors)
number_of_categories<-length(unique(mtcars$cyl))

#plot (Here only few arguments have been used). For information on additional arguments, ?beeswarm can be run 

beeswarm(wt~cyl,data = mtcars,
         col=palette(rainbow(number_of_categories)), #colors used. Depends on number of categories
         pch=19, #shape of points
         method="square", # how points should be arranged
         cex=1) # size of the points



