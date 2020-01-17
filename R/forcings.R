
#source("met.no/R/CRUTEM3v.GL.R")
#giss <- GISS.GL()
forcings <- function() {
#  require(cyclones)
#if (!file.exists("GISS-T2m.mon.txt")) {
  print("Get GISS from URL")
  test.read <- readLines("http://data.giss.nasa.gov/gistemp/graphs/Fig.A2.txt")
  if (now("c(day,mon,year)")[2]==1) iglines <- 5 else
                                    iglines <- 6
  nrows <- length(test.read) - iglines

  writeLines(test.read, "GISS-T2m.mon.txt")
#}

t2m <- read.table("GISS-T2m.mon.txt", skip = 4, col.names = c("Year",
        "Annual.Mean", "X5.year.mean"), nrows = nrows, na.strings="*")
t2m$X5.year.mean <- as.character(t2m$X5.year.mean)
t2m$X5.year.mean[is.element(t2m$X5.year.mean, "*")] <- NA
t2m$X5.year.mean <- as.numeric(t2m$X5.year.mean)
t2m$Annual.Mean <- t2m$Annual.Mean -
  mean(t2m$Annual.Mean[is.element(t2m$Year,1961:1990)])

#R <- read.table("~/data/indices/sunspot_num.dat",
#                col.names=c("year","month","number"))
#R$number[R$number<0] <- NA
#yymm <- R$year+(R$month-0.5)/12
R <- sunspots(plot=FALSE)
R$number <- R$sunspotnumber
R$number[R$number<0] <- NA
yymm <- trunc(R$yyyymm/100) + (R$yyyymm - 100*trunc(R$yyyymm/100) - 0.5)/12

#source("gcr.R")
GCR <- gcr()
attach(GCR)
  
url <- 'http://gcmd.nasa.gov/records/GCMD_NOAA_NCDC_PALEO_2004-035.html'
ftp <- 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/climate_forcing/solar_variability/lean2000_irradiance.txt'


if (!file.exists("Lean2004.txt")) {
  print("Get Lean (2004) from URL")
  Lean2004 <- readLines(ftp)
  first <- grep("YEAR",Lean2004)
  a <- Lean2004[first:length(Lean2004)]
  meta.data <- Lean2004[1:(first-1)]              
  writeLines(a,"Lean2004.txt")
}
Lean2004 <- read.table("Lean2004.txt",header=TRUE)

# Re


par(col.axis="white",xaxt="n",yaxt="n",bty="n",mar=rep(0,4))
load("GHE/data/sky.rda")  
plot(c(1950,2012),c(-2,16),type="n",xlab="",ylab="")
par(col.axis="black")
rasterImage(sky,1940,-4,2015,16)  

ii <- (yymm >= 1950)
R.number <- R$number[ii]; yymm <- yymm[ii]
polygon(c(yymm,yymm[length(yymm)],yymm[1]),
        c(7*R.number/max(R.number,na.rm=TRUE),0,0)-2,
        col="pink",border="red",density=40)

t2m$Annual.Mean[t2m$Year < 1950] <- NA
lines(t2m$Year,2*stand(t2m$Annual.Mean)+12,lwd=7,col="darkred")

#co2.test <- readLines("http://cdiac.ornl.gov/ftp/trends/co2/maunaloa.co2")
#nrows.co2 <- length(co2.test) - 20
#Mauna.Loa <- read.table("http://cdiac.ornl.gov/ftp/trends/co2/maunaloa.co2",
#                        skip=16,nrows=nrows.co2)
#Mauna.Loa[Mauna.Loa<0] <- NA

CO2 <- co2(plot=FALSE)
CO2.s <- stand( log(CO2$CO2.am) )
CO2.trend <- data.frame(y=log(CO2$CO2.am),t=CO2$year - mean(CO2$year))

lines(CO2$year,2*CO2.s+12,type="l",lwd=7,col="darkgreen")
print(summary(lm(y ~ t, data=CO2.trend)))

i1 <- (Lean2004$YEAR > 1950)
lines(Lean2004$YEAR[i1],2*stand(Lean2004$"X11yrCYCLE.BKGRND"[i1])+2,
      lwd=5,col="cyan")

points(jday.gcr,2*stand(-climax)+2,pch=19,col="grey40")
lines(jday.gcr,2*stand(-climax)+2,lty=3)

legend(1950,16,c("GISTEMP","ln(CO2) [Mauna Loa]","-CLIMAX GCR",
                 "Sunspots [R. Obs. Belgium]", "TSI [Lean, 2004]"),
       pch=c(26,26,19,26,26),lwd=c(5,5,1,1,5),
       col=c("darkred","darkgreen","grey40","red","cyan"),
       lty=c(1,1,3,1,1),cex=0.9,bty="n")

text(1950,-2.2,"1950",col="yellow",font=2)  
text(2012,-2.2,"2012",col="yellow",font=2)

text(2013,3,"Solar",srt=90,col="yellow",font=2,cex=1.5)
text(2013,12,"Terrestrial",srt=90,col="green",font=2,cex=1.5) 
text(2000,-2.5,"R.E. Benestad, 2012",col="grey",cex=0.7)
  
#dev2bitmap(file="forcings.png",res=150)
#dev2bitmap(file="forcings.pdf",type="pdfwrite")
}
