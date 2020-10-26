test<-read.csv(file.choose(),header=T) 
edit(test)
attach(test)
names(test)
kruskal.test(Latitude ~ Names, data = test)

data(airquality)
edit(airquality)