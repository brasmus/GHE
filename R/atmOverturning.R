# R.E. Benestad, 12.01.2011.

#library(clim.pact)  # Available from http://cran.r-project.org
#library(met.no.REB)

elninoyrs <- function(ylim=c(-10,10),yrs=NULL,col="grey90",lwd=7) {
#http://apollo.lsc.vsc.edu/classes/met130/notes/chapter10/elnino.html
#http://en.wikipedia.org/wiki/El_Ni%C3%B1o%E2%80%93Southern_Oscillation
# 1790–93, 1828, 1876–78, 1891, 1925–26, 1972–73, 1982–83, 1997–98 and
# 2009–2010
  if (is.null(yrs)) yrs <- c(1903,1906,1912,1915,
                             1919,1924,1926,1931,
                             1933,1940,1942,1952,
                             1954,1958,1966,1970,
                             1973,1977,1983,1987,
                             1992,1995,1998,2003,
                             2007,2010)
  for (i in 1:length(yrs)) {
    lines(rep(as.Date(paste(yrs[i],'06-01',sep='-')),2),ylim,lwd=lwd,col=col)
  }
}

prepareData <- function(tempfil= "~/ferret_scripts/ncep-ta.nc") {
  
#R <- sunspots()
  #require(ncdf)
  
  print("NCEP")
 
  print("T-t-prof:")
  ncid2 <- open.ncdf(tempfil)
  ta2 <- get.var.ncdf(ncid2,"TEMP") + 273.15
  tim2 <-  get.var.ncdf(ncid2,"TIME")
  lev2 <-  get.var.ncdf(ncid2,"LEVEL")
  close.ncdf(ncid2)

  print("Process with Ferret: v6.2")
  print("use ERAINT_TA_1979-2012.nc; let temp= ta[x=@ave,y=@ave]")
  print("save/file=ncep-ta.nc temp")
#  tempfil <- "~/ferret_scripts/eraint-ta.nc"

  date2 <- caldat( tim2/24 + julday(1,1,1900) )
  yymm2 <- date2$year + (date2$month - 0.5)/12
  attr(ta2,'dates') <- date2
  attr(ta2,'yymm') <- yymm2
  attr(ta2,'lev') <- lev2
  attr(ta2,'tim') <- tim2
  attr(ta2,'interval') <- "Jan 1948 - Sep 2012"
  attr(ta2,'history') <- "prepareData (R-package 'GHE')"
  attr(ta2,'method') <- paste("FERRET (v6.2): use air.mon.mean.nc; ",
                           "let temp= air[x=@ave,y=@ave]",
                           "save/file=ncep-ta.nc temp")
  attr(ta2,'source') <- paste("NCEP/NCAR reanalysis: monthly synoptic at",
          "pressure levels and 00:00, 06:00, 12:00, 18:00 -",
  "http://data-portal.ecmwf.int/data/d/interim_full_moda/levtype=pl/")
  attr(ta2,'created') <- now()
  attr(ta2,'signature') <- "Rasmus E. Benestad"
  save(file="GHE/data/ta2.rda",ta2)
  
  print("Use netCDF files downloaded from ECMWF.int:")
  ncid <- open.ncdf("~/data/ERAINT/ERAINT_TA_1979-2012.nc")
  lon = get.var.ncdf(ncid,"longitude")
  lat=get.var.ncdf(ncid,"latitude")
  lev=get.var.ncdf(ncid,"levelist")
  tim=get.var.ncdf(ncid,"time")
  nz <- length(lev); nt <- length(tim); nx <- length(lon); ny <- length(lat)
  ta <- matrix(rep(NA,nz*nt),nz,nt)

  # Area of the grid boxes for weighting
  box.area <- rep(cos(pi*lat/180)* pi*min(diff(lon))/180*
                  abs(pi*min(diff(lat)))/180*(6.378e03)^2,nx)
  dim(box.area) <- c(ny,nx); box.area <- t(box.area)

  for (iz in 1:nz){
    t = get.var.ncdf(ncid,"ta",start=c(1,1,iz,1),count=c(nx,ny,1,nt))

    ysrt <- order(lat)
    image(lon,lat[ysrt],box.area[,ysrt],
          main=paste("Check: level=",lev[iz],"hPa"))
    addland()
    contour(lon,lat[ysrt],t[,ysrt,1],add=TRUE)
  
    for (it in  1:nt) {
      X <- c(t[,,it]*box.area)/sum(c(box.area))
      ta[iz,it] <- sum(X)
    }
    #print(summary(c(t))); print(summary(ta[,it]))
  }
  close.ncdf(ncid)
  date <- caldat( tim/24 + julday(1,1,1900) )
  yymm <- date$year + (date$month - 0.5)/12
  attr(ta,'tim') <- tim
  attr(ta,'lon') <- lon
  attr(ta,'lat') <- lat
  attr(ta,'lev') <- lev
  attr(ta,'dates') <- date
  attr(ta,'yymm') <- yymm
  attr(ta,'box.area.weight') <- box.area
  attr(ta,'interval') <- "Jan 1979 - Sep 2012"
  attr(ta,'method') <- "X <- c(w[,,it]*box.area); wa[iz,it] <- var(X)"
  attr(ta,'source') <- paste("ERAINT full res: monthly synoptic at",
          "pressure levels and monthly means of daily means -",
  "http://data-portal.ecmwf.int/data/d/interim_full_moda/levtype=pl/")
  attr(ta,'history') <- "prepareData (R-package 'GHE')"
  attr(ta,'created') <- now()
  attr(ta,'signature') <- "Rasmus E. Benestad"
  save(file="GHE/data/ta.rda",ta)
  
  print("Use netCDF files downloaded from ECMWF.int:")
  ncid <- open.ncdf("~/data/ERAINT/ERAINT_w_1979-2012.nc")
  lon = get.var.ncdf(ncid,"longitude")
  lat=get.var.ncdf(ncid,"latitude")
  lev=get.var.ncdf(ncid,"levelist")
  tim=get.var.ncdf(ncid,"time")
  nz <- length(lev); nt <- length(tim); nx <- length(lon); ny <- length(lat)
  wa <- matrix(rep(NA,nz*nt),nz,nt)

  # Area of the grid boxes for weighting
  box.area <- rep(cos(pi*lat/180)* pi*min(diff(lon))/180*
                  abs(pi*min(diff(lat)))/180*(6.378e03)^2,nx)
  dim(box.area) <- c(ny,nx); box.area <- t(box.area)

  for (iz in 1:nz){
    w = get.var.ncdf(ncid,"w",start=c(1,1,iz,1),count=c(nx,ny,1,nt))

    ysrt <- order(lat)
    image(lon,lat[ysrt],box.area[,ysrt],
          main=paste("Check: level=",lev[iz],"hPa"))
    addland()
    contour(lon,lat[ysrt],w[,ysrt,1],add=TRUE)
  
    for (it in  1:nt) {
      X <- c(w[,,it]*box.area)
      wa[iz,it] <- var(X)
    }
  }
  close.ncdf(ncid)
  date <- caldat( tim/24 + julday(1,1,1900) )
  yymm <- date$year + (date$month - 0.5)/12
  attr(wa,'tim') <- tim
  attr(wa,'lon') <- lon
  attr(wa,'lat') <- lat
  attr(wa,'lev') <- lev
  attr(wa,'dates') <- date
  attr(wa,'yymm') <- yymm
  attr(wa,'box.area.weight') <- box.area
  attr(wa,'interval') <- "Jan 1979 - Sep 2012"
  attr(wa,'method') <- "X <- c(w[,,it]*box.area); wa[iz,it] <- var(X)"
  attr(wa,'source') <- paste("ERAINT full res: monthly synoptic at",
          "pressure levels and monthly means of daily means -",
  "http://data-portal.ecmwf.int/data/d/interim_full_moda/levtype=pl/")
  attr(wa,'history') <- "prepareData (R-package 'GHE')"
  attr(wa,'created') <- now()
  attr(wa,'signature') <- "Rasmus E. Benestad"
  save(file="GHE/data/wa.rda",wa)
 

#data(atmOverturning); attach(atmOverturning)

  print("For temperatures - need to change the variable name:")
  print("ncrename -v t,ta ERAINT_T_1979-2012.nc ERAINT_TA_1979-2012.nc")  

  if (FALSE) {
  print("Process with Ferret: v6.2")
  print("use ERAINT_TA_1979-2012.nc; let temp= ta[x=@ave,y=@ave]")
  print("save/file=eraint-ta.nc temp")
  tempfil <- "~/ferret_scripts/eraint-ta.nc"
  print("T-t-prof:")
  ncid2 <- open.ncdf(tempfil)
  ta <- get.var.ncdf(ncid2,"TEMP")
  tim2 <-  get.var.ncdf(ncid2,"TIME")
  lev <-  get.var.ncdf(ncid2,"LEVELIST")
  close.ncdf(ncid2)

  date2 <- caldat( tim2/24 + julday(1,1,1900) )
  yymm2 <- date2$year + (date2$month - 0.5)/12
  attr(ta,'dates') <- date2
  attr(ta,'yymm') <- yymm2
  attr(ta,'lev') <- lev
  attr(ta,'tim') <- tim2
  attr(ta,'interval') <- "Jan 1979 - Sep 2012"
  attr(ta,'history') <- "prepareData (R-package 'GHE')"
  attr(ta,'method') <- paste("FERRET (v6.2): use ERAINT_TA_1979-2012.nc; ",
                           "let temp= ta[x=@ave,y=@ave]",
                           "save/file=eraint-ta.nc temp")
  attr(ta,'source') <- paste("ERAINT full res: monthly synoptic at",
          "pressure levels and 00:00, 06:00, 12:00, 18:00 -",
  "http://data-portal.ecmwf.int/data/d/interim_full_moda/levtype=pl/")
  attr(ta,'created') <- now()
  attr(ta,'signature') <- "Rasmus E. Benestad"
  save(file="GHE/data/ta.rda",ta)
 }

    #Ferret: v6.2
  #use ERAINT_spec-humid_1979-2012.nc
  #let sh=Q[x=@ave,y=@ave]
  #save/file=eraint-sephum.nc sh
  qfil <- "~/ferret_scripts/eraint-sephum.nc"
  print("Q:")
  ncid4 <- open.ncdf(qfil)
  Q <- get.var.ncdf(ncid4,"SH")
  tim2 <-  get.var.ncdf(ncid4,"TIME")
  lev <-  get.var.ncdf(ncid4,"LEVELIST")
  close.ncdf(ncid4)

  date2 <- caldat( tim2/24 + julday(1,1,1900) )
  yymm2 <- date2$year + (date2$month - 0.5)/12
  attr(Q,'dates') <- date2
  attr(Q,'yymm') <- yymm2
  attr(Q,'lev') <- lev
  attr(Q,'tim') <- tim2
  attr(Q,'interval') <- "Jan 1979 - Sep 2012"
  attr(Q,'history') <- "prepareData (R-package 'GHE')"
  attr(Q,'method') <- paste("FERRET (v6.2): use ERAINT_TA_1979-2012.nc; ",
                           "let SH= Q[x=@ave,y=@ave]",
                           "save/file=eraint-sephum.nc SH")
  attr(Q,'source') <- paste("ERAINT full res: monthly synoptic at",
          "pressure levels and 00:00, 06:00, 12:00, 18:00 -",
  "http://data-portal.ecmwf.int/data/d/interim_full_moda/levtype=pl/")  
  attr(Q,'created') <- now()
  attr(Q,'signature') <- "Rasmus E. Benestad"

  save(file="GHE/data/Q.rda",Q)

}

AtmOverturning <- function() {

#  load("GHE/data/wa.rda")
#  load("GHE/data/ta.rda")
    data("wa",envir=environment())
    data("ta",envir=environment())
  lev <- attr(wa,'lev'); tim <- attr(wa,'tim')
  nz <- length(lev); nt <- length(tim)

  Z <- seq(0,100000,by=100)
  heights <- approx(p.hydrostatic(Z),Z,lev)$y
  dZ <- c(0,abs(diff(heights)))
  upper <- heights >= 6500
  middle <- (heights < 6500) & heights >= 1000
  lower <- heights < 1000
  date <- caldat( tim/24 + julday(1,1,1900) )
  yymm <- date$year + (date$month - 0.5)/12
  # http://en.wikipedia.org/wiki/Specific_gas_constant#Specific_gas_constant
  R.sp <- 287.04
  air.mass <- 100*attr(wa,'lev')/(R.sp*rowMeans(ta)) * dZ
  wa <- diag(air.mass)%*%wa

  print("Define atmospheric levels:")
  troposphere <- heights < 12000
  alt <- heights[troposphere]
  y <- wa[troposphere,]

  overturning <- ma.filt(colSums(wa[middle,]) - mean(wa[middle,]),12)
  overturning.cal <- data.frame(y=ma.filt(colSums(wa[middle,]),12),
                                x=yymm - mean(yymm))
  print(summary(lm(y ~ x,data=overturning.cal)))

  par(bty="n",yaxt="n")
  y <- stand(overturning) + 3; 
  plot(yymm, y,type="l",lwd=4,
     main="Atmospheric overturning anomaly",
     ylab="",xlab="Time",ylim=c(-1.5,8),
     sub=expression(eta(z,t)==var[i](a[i]*w[i](z,t))*rho(z)))
  par(col.axis="black")
  axis(1)
  elninoyrs(ylim=c(-1.5,8))
  lines(yymm, y,lwd=4)
  grid()
  abline(lm(y ~ yymm),lty=2)
  Avz.trend <- summary(lm(y ~ x,data=overturning.cal))$coefficients
  c <- round(Avz.trend)

  lines(yymm,0.5*stand(ma.filt(colSums(wa[upper,])-mean(wa[upper,]),12))+6,
      lwd=4,col="steelblue")
  lines(yymm,0.5*stand(ma.filt(colSums(wa[lower,])-mean(wa[lower,]),12)),
      lwd=4,col="darkblue")

#  dev2bitmap("atmOverturning.png",res=150)
#  dev.copy2eps(file="atmOverturning.eps")
}

emissionHeigth <- function(ncep=TRUE) {
  
# Emission level height:

  #load("GHE/data/ta.rda")
  data("ta",envir=environment())
  lev <- attr(ta,'lev'); tim <- attr(ta,'tim')
  time <- as.Date(tim/24, origin=as.Date('1900-01-01'))
  nz <- length(lev); nt <- length(tim)
  #load("GHE/data/Q.rda")
  data(Q,envir=environment())
  Z <- seq(0,100000,by=100)
  alt <- approx(p.hydrostatic(Z),Z,lev)$y
  yymm <- attr(ta,'yymm')
  yymm3 <- attr(Q,'yymm')
  # p = rho*R.sp*T -> rho = p/(R.sp * T)
  # http://en.wikipedia.org/wiki/Specific_gas_constant#Specific_gas_constant
  # http://www.engineeringtoolbox.com/molecular-mass-air-d_679.html
  R <- 286.9 # J /(kg K)
  dZ <- c(0,abs(diff(alt)))
  air.mass <- 100*attr(Q,'lev')/(R*rowMeans(ta)) * dZ
  print(paste("Mass of a column air=",round(sum(air.mass)),"=",
              round(100000/9.81),"kg"))
  Q <- diag(air.mass)%*%Q
  #p= M*g/A
  
  #load("GHE/data/levpreseraint.rda")
  data(levpreseraint,envir=environment())
  troposphere <- alt < 12000
  alt <- alt[troposphere]
  ta <- ta[troposphere,]

  t254k <- rep(NA,nt)
  for (i in 1:nt) t254k[i] <- approx(x=ta[,i],y=alt,xout=254)$y
  i1 <- is.element(as.numeric(format(time,'%Y')),1980:2011)
  y <- 0.5*stand(ma.filt(t254k,12),ii=i1)+3.25
  z254k.cal <- data.frame(y=ma.filt(t254k,12),
                          x=yymm - mean(yymm))
  q.cal <- data.frame(y=ma.filt(colSums(Q),12),
                      x=yymm - mean(yymm))
  print("Z254K:")
  print(summary(lm(y ~ x,data=z254k.cal)))
  print("Spec.Hum:")
  print(summary(lm(y ~ x,data=q.cal)))

  if (ncep) {
    #load("GHE/data/ta2.rda")
    data(ta2,envir=environment())
    lev2 <- attr(ta2,'lev'); tim2 <- attr(ta2,'tim')
    nz2 <- length(lev2); nt2 <- length(tim2)
    alt2 <- approx(p.hydrostatic(Z),Z,lev2)$y
    date2 <- caldat( tim2/24 + julday(1,1,1900) )
    time2 <- as.Date(tim2/24, origin=as.Date('0001-01-01'))
    yymm2 <- date2$year + (date2$month - 0.5)/12
    t254k2 <- rep(NA,nt2)
    for (i in 1:nt2) t254k2[i] <- approx(x=ta2[,i],y=alt2,xout=254)$y
    i2 <- is.element(as.numeric(format(time2,'%Y')),1980:2011)
    y2 <- 0.5*stand(ma.filt(t254k2,12),ii=i2)+3.25
    print(summary(c(ta2))); print(lev2); print(alt2)
    print(summary(y2)); print(summary(yymm2))
  }
  
  par(bty="n",yaxt="n")

  plot(time, y,type="l",lwd=4,
     main=expression(paste("Atmospheric emission level",T[254*K],
         " and relative humidity ",q[tot])),
     ylab="",xlab="Time",ylim=c(-1.5,6.5),
       xlim=as.Date(c("1948-01-01","2013-12-13")),
     sub="ERAINT")
  elninoyrs(ylim=c(-1.5,6.5))
  lines(time, y,lwd=4)
  abline(lm(y ~ time),lty=2,col="darkgreen")
  y <- ma.filt(t254k,12); t <- yymm - mean(yymm)
  t254k.trend <- summary(lm(y ~ t))
  c <- round(t254k.trend$coefficients)
  
  if (ncep) lines(time2,y2,col="grey",lwd=3)

# Total water vapour content:
  y3 <- 0.5*stand(ma.filt(colSums(Q),12))
  lines(time,y3,col="steelblue",lwd=3)

  abline(lm(y3 ~ time),lty=2,col="steelblue")

  legend(as.Date("1948-06-01"),6.25,
       c(expression(paste("ERAINT:",Z[254*K])),
         expression(paste("NCEP/NCAR:",Z[254*K])),
         expression(q[tot])),
       col=c("black","grey","steelblue"),
       lty=c(1,1,1),lwd=c(4,2,2),bty="n",cex=0.75,
         bg="white")

#  dev2bitmap("emissionHeigth.png",res=150)
#  dev.copy2eps(file="emissionHeigth.eps")
}


Hulbert1931Tz <- function(x,xb=3300,S=1.35e6,rho=5.67e-8,alpha=0.10) {
  #Table 1, right column from Hulbert (1931)
  alphaxb.x <- c(145,60,5.2,3.8,2.3,1.67,1.00,0.44,0)
  f <-0.55
  phi <- 1 - f
  t4 <- S*A/(8*rho*f)*(1-phi)/(1+0.5*phi*alpha*xb)*(1+alphaxb.x)
}
   
supportDiag <- function() {

  data(ta,envir=environment())
  data(levpres,envir=environment())
  lev <- attr(ta,'lev'); tim <- attr(ta,'tim')
  time <- as.Date(tim/24, origin=as.Date('1900-01-01'))
  nz <- length(lev); nt <- length(tim)
  Z <- seq(-10,100000,by=10)
  alt <- approx(p.hydrostatic(Z),Z,lev)$y

# Supporting diagnostics
  

  trop <- (alt/1000) < 12
  t254k <- rep(NA,nt)
  par(bty="n",yaxt="n")
  for (i in 1:nt) t254k[i] <- approx(x=ta[trop,i],y=alt[trop],xout=254)$y

  dev.new()
  plot(rowMeans(ta),alt/1000,type="l",lwd=3,
     main="Vertical T-profile from ERAINT",
     sub="Global mean averaged over 1979 - 2012",
     xlab="Temperature (K)",ylab="Altitude (km)")
grid()
lines(range(ta),rep(mean(t254k)/1000,2),lty=2)
text(215,5,"mean emission level altitude",cex=0.75)

t <- ta[trop,]; z <- alt[trop]/1000; srtz <- order(z)

calibr <- data.frame(t=rowMeans(t[srtz,]),z=z[srtz])
lapserate <- lm(t ~ z,data=calibr)
lines(predict(lapserate,newdata=calibr),z[srtz],lty=2,col="red")
text(260,10,paste("Mean lapse rate=",
                  round(summary(lapserate)$coefficients[2],2),"K/km"),col="red")

cols <- rgb(sin(pi*seq(0,1,length=nt)),seq(1,0,length=nt),seq(0,1,length=nt))
  
LR <- rep(NA,nt)
temp4 <- ta[trop,]; d <- dim(temp4)
z <- alt[trop]/1000; srtz <- order(z)
for (j in 1:d[1]) temp4[j,] <- ma.filt(temp4[j,],12)
for (i in 1:nt) {
  t <- temp4[,i]
  if (sum(is.finite(t))==d[1]) {
    calibr <- data.frame(t=t[srtz],z=z[srtz])
    lapserate <- lm(t ~ z,data=calibr)
    lines(predict(lapserate,newdata=calibr),z[srtz],lty=2,col=cols[i])
    LR[i] <- summary(lapserate)$coefficients[2]
  }
}

text(265,50,"Lapse rate")

colorbar(yymm=tim,fig=c(0.2,0.4,0.70,0.80),breaks=0:nt,col=cols)

fig.old <- c(0,1,0,1)
par(fig=c(0.53,0.93,0.45,0.70),new=TRUE,cex.axis=0.75,xaxt="s",yaxt="s",
    cex.main=0.75)
plot(time,LR,type="l",xlab="",ylab="",col="blue")
grid()
par(fig=fig.old,new=TRUE)

#dev2bitmap("atmOverturning-Tprofile.png",res=150)
#dev2bitmap("atmOverturning-Tprofile.jpg",type="jpeg",res=200)
#dev.copy2eps(file="atmOverturning-Tprofile.eps")
#dev2bitmap("atmOverturning-Tprofile.pdf",type="pdfwrite")


}

olr.corr <- function() {

  col=c("black","red","blue","green")
  data(olr.t2m.corr,envir=environment())
  #load(file="GHE/data/olr-t2m-corr.rda")
  olr.meta <- attr(olr.t2m.corr,'olr.meta')
  olr.meta$Date2[length(olr.meta$Date2)] <- '2005/07/31'
  date1 <- as.numeric(substr(olr.meta$Date1,1,4)) +
           (as.numeric(substr(olr.meta$Date1,6,7))-1)/12
  date2 <- as.numeric(substr(olr.meta$Date2,1,4)) +
           (as.numeric(substr(olr.meta$Date2,6,7))-1)/12
  date2[length(date2)] <- attr(olr.t2m.corr,'yymm')[length(attr(olr.t2m.corr,'yymm'))]
                      
  par(xaxt="s",yaxt="s",bty="n")
  plot(attr(olr.t2m.corr,'yymm'),olr.t2m.corr,type="l",lwd=1,col="grey",
     ylab="correlation",xlab="time",
     sub="ERA40 T(2m) - Liebmann & Smith (2006) OLR")
  # March 12, 1999
  lb <- rgb(1,0.95,0.95)
  rect(1999+1/6+12/365.25,0.886,2015,1,col=lb,border=lb)
  elninoyrs(ylim=c(-1,1))
  y <- filter(olr.t2m.corr,rep(1,12)/12)
  lines(attr(olr.t2m.corr,'yymm'), olr.t2m.corr,col="grey")
  lines(attr(olr.t2m.corr,'yymm'),y,lwd=5)

  #print(date1); print(date2); print(attr(r,'yymm'))  
  for (i in 1:length(date1)) {
    yy <- y
    ii <- (attr(olr.t2m.corr,'yymm') < date1[i]) |
          (attr(olr.t2m.corr,'yymm') > date2[i])
    yy[ii] <- NA
    lines(attr(olr.t2m.corr,'yymm'),yy,lwd=3,col=col[i%%4+1])
    text(date1[i],0.89,olr.meta$Satellite[i],pos=4,cex=0.75,col="pink",
         srt=90)
  }
  
  #dev.copy2eps(file="olr-t2m-corr.eps")
}





