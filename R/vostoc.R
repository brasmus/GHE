# R.E. Benestad, Oslo 04.02.2005

stand <- function(x,ii=NULL) {
 if (is.null(ii)) ii <- is.finite(x)
 x <- (x - mean(x[ii],na.rm=TRUE))/sd(x[ii],na.rm=TRUE)
}
# Read the data from URL: 

vostoc <- function() {
  #require(Rwave)
  
url1 <- "ftp://cdiac.ornl.gov/pub/trends/co2/vostok.icecore.co2"
url2 <- "http://cdiac.esd.ornl.gov/ftp/trends/temp/vostok/vostok.1999.temp.dat"
a <- readLines(url1)

# Save the data in a local file: discard the header..
a <- a[22:length(a)]
writeLines(a,"vostoc_co2.dat")

#                Mean
#       Age of   age of    CO2
#Depth  the ice  the air concentration
# (m)   (yr BP)  (yr BP)  (ppmv)

co2 <- read.table("vostoc_co2.dat",header=T,
                  col.names=c("Depth","age.of.ice","age.of.air","co2.concentration"))
attr(co2$Depth,"unit") <- "m"   
attr(co2$age.of.ice,"unit") <- "yr BP"  
attr(co2$age.of.air,"unit") <- "yr BP"  
attr(co2$co2.concentration,"unit") <- "ppmv"


# Save the data in a local file: discard the header..
a <- readLines(url2)
a <- a[59:length(a)]
writeLines(a,"vostoc_tas.dat")

#                  Deuterium               
#         Age of     content   Temperature
# Depth   the ice   of the ice  Variation
#  (m)    (yr BP)   (delta D)    (deg C)

tas <- read.table("vostoc_tas.dat",header=T,
                  col.names=c("Depth","age.of.ice","deuterium","temperature"))
attr(tas$Depth,"unit") <- "m"   
attr(tas$age.of.ice,"unit") <- "yr BP"  
attr(tas$deuterium,"unit") <- "delta D"  
attr(tas$temperature,"unit") <- "deg C"


dev.new()
plot(co2$age.of.ice, co2$age.of.air)

dev.new()
plot(-co2$age.of.ice, stand(co2$co2.concentration), type="l",lwd=3,col="grey",
     xlab=attr(co2$age.of.ice,"unit"),ylab="Standardised",
     main="Historical CO2 Record from the Vostok Ice Core",sub=url1)
lines(rep(-74000,2),c(-5,5),lty=3,col="red"); text(-60000,-1.8,"supervolcano",col="red",cex=0.7)
grid()
lines(-tas$age.of.ice, stand(tas$temperature),lty=2)
legend(-4e5,-1.5,c("CO2","TAS"),col=c("grey","black"),lwd=c(3,2),lty=c(1,2),cex=0.8)
dev2bitmap("vostoc.png",res=150)

i1 <- is.element(co2$age.of.ice,tas$age.of.ice)
i2 <- is.element(tas$age.of.ice,co2$age.of.ice)
CO2 <- co2$co2.concentration[i1]
T <- tas$temperature[i2]
plot(1,1,type="n")

dev.new()
acf(cbind(CO2,T))
dev2bitmap("vostoc-acf.png",res=150)

spectrum(co2$co2.concentration)
spectrum(tas$temperature)
dev2bitmap("vostoc-spectrum.png",res=150)

par(mfrow=c(2,1))
wl <- cwt(co2$co2.concentration,8,plot=FALSE)
image(co2$age.of.ice,1:8,log(abs(wl)),main="CO2")
contour(co2$age.of.ice,1:8,log(abs(wl)),add=TRUE)

wl <- cwt(tas$temperature,8,plot=FALSE)
image(-tas$age.of.ice,1:8,log(abs(wl)),main="temperature")
contour(-tas$age.of.ice,1:8,log(abs(wl)),add=TRUE)
}

