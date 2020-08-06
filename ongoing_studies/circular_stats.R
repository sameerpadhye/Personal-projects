#install.packages('circular')
library(readxl)
library(circular)
library(tidyverse)

data(fisherB18)
data(wind)
View(wind)
View(fisherB10c)
View(fisherB11c)
plot(fisherB1, pch=16,
     col="blue", stack=T, shrink=1.2, bins=720, ticks=T)

windc <- circular(fisherB1, type="angles", units="radians", template='geographics')

windc

par(mai=c(0.85, 0.85, 0.05, 0.05), cex.axis=1.1, cex.lab=1.3)

plot(wind, pch=16, xlab="Observation number", ylab="Wind direction (in radians)")


par(mai=c(0, 0, 0, 0))
View(windc)
plot(windc, cex=1.5, bin=720, stack=TRUE, sep=0.035, shrink=1.3)

axis.circular(at=circular(seq(0, 7*pi/4,pi/4)), labels=c("N","NE","E","SE","S","SW","W","NW"), zero=pi/2, rotation='clock', cex=1.5)

ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation='clock', tcl=0.075)


plot(windc, cex=1.1, bin=720, stack=TRUE, sep=0.035, shrink=1.8)

axis.circular(at=circular(seq(0, 7*pi/4,pi/4)), labels=c("N","NE","E","SE","S","SW","W","NW"), zero=pi/2, rotation='clock', cex=1.1)

ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation='clock', tcl=0.075)

rose.diag(windc, bins = 16, col="darkgrey", cex=1.1, prop=1.3, add=TRUE)

lines(density.circular(windc, bw=75), lwd=2, lty=2)

lines(density.circular(windc, bw=40), lwd=2, lty=1)

lines(density.circular(windc, bw=10), lwd=2, lty=3)

par(mai=c(0.85, 0.85, 0.05, 0.05), cex.axis=1.1, cex.lab=1.3)

hist(wind, main="", xlab="Wind direction (radians)", ylab="Frequency", breaks=seq(from=0,to=2*pi,by=pi/8), col="grey", xlim=c(0,2*pi))



View(fisherB11)
fB11 <- c(fisherB11,8)

cfB11 <- circular(fB11, units="degrees", zero=circular(pi/2), rotation="clock")

fB11c <- circular(fB11,units="degrees", zero=circular(0), rotation="counter")



plot(cfB11, stack=TRUE, bins=720, cex=1.5)

arrows.circular(cfB11)

arrows.circular(mean(cfB11), y=rho.circular(cfB11), lwd=3)



cf2 <- 2*cfB11

plot(cf2, stack=TRUE, bins=720, cex=1.5)

arrows.circular(cf2)

arrows.circular(mean(cf2), y=rho.circular(cf2), lwd=3)

data_path<-"C:/Research_data/Research data/Other groups/seed_data/Data for circular statistics_July2020.xls"

data<-read_excel(data_path,sheet=1)

circ_obj<-circular(data$No._of_flowering_species,units="degrees", zero=circular(0), rotation="clock")
plot(circ_obj, stack=TRUE, bins=120, cex=1.5)

