#rm(list=ls())
#library(chron)

gcr <- function() {
  
url1 <- "ftp://ulysses.sr.unh.edu/NeutronMonitor/DailyAverages.1951-.txt"
url2 <- "ftp://ftp.ngdc.noaa.gov/STP/GEOMAGNETIC_DATA/AASTAR/aaindex"

print(paste("Reading GCR data from",url1,"..."))
a <- readLines(url1)
print("Saving GCR data in local file 'neutron2.dat'; skipping the header:")
#print(a[1:43])
a <- a[44:length(a)]
writeLines(a,"neutron2.dat")


gcr <- read.table("neutron2.dat",header=T, as.is=T,
                  col.names=c("Date","SecsOf1904","Climax","Huancayo.NoGC",
                             "Huancayo.GC","Haleakala.IGY","Haleakala.S/M"))
climax <- filter(as.numeric(gcr$Climax),rep(1,30)/30)
nt.gcr <- length(climax)
climax <- climax[seq(15,nt.gcr,by=30)]
gcr.yy <- as.numeric(substr(as.character(gcr$Date),7,10))
gcr.mm <- as.numeric(substr(as.character(gcr$Date),1,2))
gcr.dd <- as.numeric(substr(as.character(gcr$Date),4,5))
jday.gcr  <- gcr.yy + (gcr.mm-1)/12 + (gcr.dd-0.5)/365.25
jday.gcr <- jday.gcr[seq(15,nt.gcr,by=30)]

Rs <- sunspots()

dev.new()
par(cex.sub=0.8)
plot(range(gcr.yy,na.rm=TRUE),range(climax,na.rm=TRUE),type="n",
     main=paste("Climax Galactic Cosmic Ray (GCR) counts",min(gcr.yy),"-",max(gcr.yy)),lwd=4,
     sub="URL http://ulysses.uchicago.edu/NeutronMonitor/neutron_mon.html (gcr.R)",
     ylab="30-day mean Counts",xlab="Time")
grid()


R.stand <- 0.75*sd(climax,na.rm=TRUE)*
      (Rs$sunspotnumber - mean(Rs$sunspotnumber,na.rm=TRUE))/sd(Rs$sunspotnumber,na.rm=TRUE) + quantile(climax,0.01,na.rm=TRUE)
axis(side=4,at=seq(min(R.stand,na.rm=TRUE),max(R.stand,na.rm=TRUE),length=5),
     labels=FALSE,col="pink",cex=0.5)
mtext("Sunspot number",4,col="pink")
lines(Rs$year,R.stand,
      lwd=5,col="pink")
points(jday.gcr,climax,pch=19,cex=0.7,col="grey50")
lines(jday.gcr,climax,lty=3)
grid()

GCR <- list(gcr.yy=gcr.yy,climax=climax,Rs=Rs,jday.gcr=jday.gcr)
invisible(GCR)
}

#dev2bitmap(file="gcr.png")
