#library(met.no.REB)

co2.temp <- function() {
  
#co2.test <- readLines("http://cdiac.ornl.gov/ftp/trends/co2/maunaloa.co2")
#nrows.co2 <- length(co2.test) - 20
#Mauna.Loa <- read.table("http://cdiac.ornl.gov/ftp/trends/co2/maunaloa.co2",
#                        skip=16,nrows=nrows.co2)
#Mauna.Loa[Mauna.Loa<0] <- NA
#co2 <- c(t(as.matrix(Mauna.Loa[,2:13])))
#yy <- as.matrix(Mauna.Loa[,1])
#yymm <- sort(rep(yy,12) + (rep(1:12,length(yy)) - 0.5)/12)
#
#use <- yymm > 1900
#yymm <- yymm[use]
#co2 <- co2[use]
Mauna.Loa <- read.table("ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_annmean_mlo.txt",col.names=c("year","co2","error"))
yy <- Mauna.Loa$year

# Vostoc ice core:

#url1 <- "ftp://cdiac.ornl.gov/pub/trends/co2/vostok.icecore.co2"
#url2 <- "http://cdiac.esd.ornl.gov/ftp/trends/temp/vostok/vostok.1999.temp.dat"
#a <- readLines(url1)

# Save the data in a local file: discard the header..
#a <- a[22:length(a)]
#writeLines(a,"vostoc_co2.dat")

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
#a <- readLines(url2)
#a <- a[60:length(a)]
#writeLines(a,"vostoc_tas.dat")

#                  Deuterium               
#         Age of     content   Temperature
# Depth   the ice   of the ice  Variation
#  (m)    (yr BP)   (delta D)    (deg C)

tas <- read.table("vostoc_tas.dat",header=FALSE,
                 col.names=c("Depth","age.of.ice","deuterium","temperature"))
attr(tas$Depth,"unit") <- "m"   
attr(tas$age.of.ice,"unit") <- "yr BP"  
attr(tas$deuterium,"unit") <- "delta D"  
attr(tas$temperature,"unit") <- "deg C"

CRU <- HadCRUT3v.GL(internet=FALSE)
GISS <- GISS.GL()

data(T2m.ncep,envir=environment())
i1 <- is.element(yy,year.ncep)
i2 <- is.element(year.ncep,yy)
plot(stand(log(Mauna.Loa$co2[i1])),stand(T2m.ncep[i2]),pch=19,
     xlab="log(CO2-conc.)",ylab="Global mean T(2m) estimate",
     sub="Standardised series",xlim=c(-2,2),ylim=c(-2,3),
     main="log(CO2) - versus temperature")
grid()
abline(lm(stand(T2m.ncep[i2]) ~ stand(log(Mauna.Loa$co2[i1]))),lty=2)
print(cor.test(log(Mauna.Loa$co2[i1]),T2m.ncep[i2]))
print(summary(lm(stand(T2m.ncep[i2]) ~ stand(log(Mauna.Loa$co2[i1])))))
lnCO2 <- c(stand(log(Mauna.Loa$co2[i1])))
temp <- c(stand(T2m.ncep[i2]))

i1 <- is.element(round(co2$age.of.ice/10),round(tas$age.of.ice/10))
i2 <- is.element(round(tas$age.of.ice/10),round(co2$age.of.ice/10))
points(stand(log(co2$co2.concentration[i1])),stand(tas$temperature[i2]),
       col="grey")
abline(lm(stand(tas$temperature[i2]) ~ stand(co2$co2.concentration[i1])),
       lty=2,col="grey")
print(cor.test(log(co2$co2.concentration[i1]),tas$temperature[i2]))
print(summary(lm(stand(tas$temperature[i2])~stand(co2$co2.concentration[i1]))))
lnCO2 <- c(lnCO2,stand(log(co2$co2.concentration[i1])))
temp <- c(temp,stand(tas$temperature[i2]))


i1 <- is.element(yy,CRU$Year)
i2 <- is.element(CRU$Year,yy)
points(stand(log(Mauna.Loa$co2[i1])),stand(CRU$T2m[i2]),col="blue",pch=19)
abline(lm(stand(log(Mauna.Loa$co2[i1])) ~ stand(CRU$T2m[i2])),lty=2,col="blue")
print(cor.test(log(Mauna.Loa$co2[i1]),CRU$T2m[i2]))
print(summary(lm(stand(log(Mauna.Loa$co2[i1])) ~ stand(CRU$T2m[i2]))))
lnCO2 <- c(lnCO2,stand(log(Mauna.Loa$co2[i1])))
temp <- c(temp,stand(CRU$T2m[i2]))


i1 <- is.element(yy,GISS$Year)
i2 <- is.element(GISS$Year,yy)
points(stand(log(Mauna.Loa$co2[i1])),stand(GISS$T2m[i2]),col="darkgreen",pch=19)
abline(lm(stand(log(Mauna.Loa$co2[i1])) ~ stand(GISS$T2m[i2])),
       lty=2,col="darkgreen")
print(cor.test(log(Mauna.Loa$co2[i1]),GISS$T2m[i2]))
print(summary(lm(stand(log(Mauna.Loa$co2[i1])) ~ stand(GISS$T2m[i2]))))
lnCO2 <- c(lnCO2,stand(log(Mauna.Loa$co2[i1])))
temp <- c(temp,stand(GISS$T2m[i2]))

#data(T2m.era40)
#i1 <- is.element(yy,year.era40)
#i2 <- is.element(year.era40,yy)
#points(stand(log(Mauna.Loa$co2[i1])),stand(T2m.era40[i2]),pch=19,col="red")
#abline(lm(stand(log(Mauna.Loa$co2[i1]))~stand(T2m.era40[i2])),lty=2,col="red")
#print(cor.test(log(Mauna.Loa$co2[i1]),T2m.era40[i2]))
#print(summary(lm(stand(log(Mauna.Loa$co2[i1]))~stand(T2m.era40[i2]))))
#lnCO2 <- c(lnCO2,stand(log(Mauna.Loa$co2[i1])))
#temp <- c(temp,stand(T2m.era40[i2]))


#data(T2m.ncdc)
#i1 <- is.element(yy,year.ncdc)
#i2 <- is.element(year.ncdc,yy)
#points(stand(log(Mauna.Loa$co2[i1])),stand(T2m.ncdc[i2]),pch=19,col="cyan")
#abline(lm(stand(T2m.ncdc[i2]) ~ stand(log(Mauna.Loa$co2[i1]))),lty=2,col="cyan")
#print(cor.test(log(Mauna.Loa$co2[i1]),T2m.ncdc[i2]))
#print(summary(lm(stand(T2m.ncdc[i2]) ~ stand(log(Mauna.Loa$co2[i1])))))
#lnCO2 <- c(lnCO2,stand(log(Mauna.Loa$co2[i1])))
#temp <- c(temp,stand(T2m.ncdc[i2]))

#legend(-2,3,c("GISTEMP","HadCRUT3v","NCDC","ERA40","NCEP/NCAR","Vostok"),
legend(-2,3,c("GISTEMP","HadCRUT3v","NCEP/NCAR","Vostok"),
       pch=c(rep(19,5),21),cex=0.75,bg="grey95",
       col=c("darkgreen","blue","black","grey"))

#dev2bitmap("co2_x_t2m.png",res=150)

print(cor.test(lnCO2,temp))

dev.new()
par(ps=12,bg="black",col.axis="white",col.main="white",col.lab="white",
    cex.lab=1.4)
plot(lnCO2,temp,pch=19,
     xlab="log(CO2-conc.)",ylab="Global mean T(2m) estimate",
     sub="Standardised series",xlim=c(-2,2),ylim=c(-2,3),
     main="log(CO2) - versus temperature")
lines(c(-5,5),c(-5,5),lty=2,lwd=2,col="red")
largesymbols(lnCO2,temp,col="blue",cex=3)
dev2bitmap("co2_x_t2m-nice.png",res=150)
}
