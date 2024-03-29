# R.E. Benestad, 28.12.2011.

#library(clim.pact)  # Available from http://cran.r-project.org
#library(cyclones)

gauss.filt <- function (x, n) {
    i <- seq(0, qnorm(0.975), length = n/2)
    win <- dnorm(c(sort(-i), i))
    win <- win/sum(win)
    y <- filter(x, win)
    y
}


# Simple formula for estimating the pressure at a given height 
p.hydrostatic <- function (z,p0 = 1013.250,Temp = 288,g = 9.81,
                           k = 1.38e-23,M = 0.027/6.022e+23) {
    p <- p0 * exp(-(M * g * z)/(k * Temp))
    p
}


stand <- function (x, m = NULL, s = NULL,ii=NULL,verbose=FALSE) {
    if (is.null(ii))
      ii <- is.finite(x)
    if (is.null(m)) 
        m <- mean(x[ii])
    if (is.null(s)) 
        s <- sd(x[ii])
    labels <- seq(min(10 * round(floor(x)/10, 1), na.rm = TRUE), 
        max(10 * round(ceiling(x)/10, 1), na.rm = TRUE), length = 20)
    if (verbose) print(paste("stand: m=",m,"s=",s))
    x <- (x - m)/s
    at <- (round(labels, 2) - m)/s
    attr(x, "m") <- m
    attr(x, "s") <- s
    attr(x, "base.line") <- ii
    attr(x, "axis.at") <- at
    attr(x, "axis.labels") <- round(labels, 2)
    x
}

ma.filt <- function (x, n) {
    y <- filter(x, rep(1, n)/n)
    y
}

sunspots <- function(url="http://sidc.oma.be/DATA/monthssn.dat") {
  asciidata <- readLines(url)
  for (i in 1:6) asciidata[i] <- paste(asciidata[i],"NA")
  for (i in (length(asciidata)-20):length(asciidata)) {
    asterisk <- instring("*",asciidata[i])
    #print(paste("i=",i,"astersk=",asterisk,asciidata[i]))
    if (length(asterisk)>0) {
      if (asterisk[1]>0) {
        if (length(asterisk)==1) asciidata[i] <-
          substr(asciidata[i],1,asterisk[1]-1) else
        if (length(asterisk)==2) asciidata[i] <-
          paste(substr(asciidata[i],1,asterisk[1]-1),
                substr(asciidata[i],asterisk[1]+1,asterisk[2]-1))
      }
    } 
    if (i >= length(asciidata)-5) asciidata[i] <- paste(asciidata[i],"NA")
  }
  writeLines(asciidata,"SIDC-sunspotnumber.txt")
  Rs <- read.table("SIDC-sunspotnumber.txt",header=FALSE,
                   col.names=c("yyyymm","year","sunspotnumber","smoothed"))
  attr(Rs,"description") <-
    list(src="monthly sunspot number from ROYAL OBSERVATORY OF BELGIUM",url=url)
  Rs
}

colorbar <-  function(yymm,fig=c(0.33,0.66,0.30,0.34),breaks,col) {
  print("colorbar")
  fig.old <- c(0,1,0,1)
  par(fig=fig,new=TRUE,mar=rep(0,4),xaxt="n",yaxt="n",cex.axis=0.5)
  colbar <- cbind(c(breaks[-1]),c(breaks[-1])); d <- dim(colbar)
  #print(d); print(length(yymm))
  image(yymm,c(0,1),colbar,breaks=breaks,col=col)
  par(fig=fig.old,new=TRUE)
}


prettygraph <- function(t,y,col="black",tshadeoffset=0.1,yshadeoffset=0.1) {
  greys <- rgb( (seq(1,0.06,length=100)^0.1),
                (seq(1,0.06,length=100)^0.1),
                (seq(1,0.06,length=100)^0.1) )
  toffs <- tshadeoffset*(max(t,na.rm=TRUE)- min(t,na.rm=TRUE))*0.25
  yoffs <- yshadeoffset*(max(y,na.rm=TRUE)- min(y,na.rm=TRUE))
  for (i in 1:100) {
    ii <- ( (i - 100)/100 )
    lines(t-toffs+ii*toffs,gauss.filt(y-yoffs+ii*yoffs,5),lwd=2,col=greys[i])
    lines(t-toffs-ii*toffs,gauss.filt(y-yoffs-ii*yoffs,5),lwd=2,col=greys[i])
  }
  lines(t,y,lwd=5,col=col)
}

    caldat <- function (julian) {
    igreg = 2299161
    julian <- trunc(julian)
    jalpha <- julian * 0
    ja <- julian * 0
    im <- (julian >= igreg)
    if (sum(im) > 0) {
        jalpha[im] <- trunc(((julian - 1867216) - 0.25)/36524.25)
        ja[im] <- julian + 1 + jalpha - trunc(0.25 * jalpha)
    }
    im <- (julian < igreg)
    if (sum(im) > 0) 
        ja[im] <- julian[im]
    jb <- ja + 1524
    jc <- trunc(6680 + ((jb - 2439870) - 122.1)/365.25)
    jd <- 365 * jc + trunc(0.25 * jc)
    je <- trunc((jb - jd)/30.6001)
    id <- jb - jd - trunc(30.6001 * je)
    mm <- je - 1
    im <- (mm > 12)
    if (sum(im) > 0) 
        mm[im] <- mm[im] - 12
    iyyy <- jc - 4715
    im <- (mm > 2)
    if (sum(im) > 0) 
        iyyy[im] <- iyyy[im] - 1
    im <- (iyyy <= 0)
    if (sum(im) > 0) 
        iyyy <- iyyy - 1
    caldat <- list(month = mm, day = id, year = iyyy)
    invisible(caldat)
  }


  julday <- function (mm, id, iyyy) {
    igreg <- 588829
    mm <- trunc(mm)
    id <- trunc(id)
    iyyy <- trunc(iyyy)
    im <- (iyyy == 0)
    if (sum(im, na.rm = TRUE) > 0) 
        return("There is no year zero!")
    if ((length(mm) != length(id)) | (length(mm) != length(iyyy)) | 
        (length(iyyy) != length(id))) 
        return("The vectors must have same length!")
    im <- (iyyy < 0)
    if (sum(im) > 0) 
        iyyy[im] <- iyyy[im] + 1
    jy <- mm * 0
    jm <- mm * 0
    ja <- mm * 0
    im <- (mm > 2)
    if (sum(im) > 0) {
        jy[im] <- iyyy[im]
        jm[im] <- mm[im] + 1
    }
    im <- (mm <= 2)
    if (sum(im) > 0) {
        jy[im] <- iyyy[im] - 1
        jm[im] <- mm[im] + 13
    }
    jul <- trunc(365.25 * jy) + trunc(30.6001 * jm) + id + 1720995
    im <- (id + 31 * (mm + 12 * iyyy) >= igreg)
    if (sum(im) > 0) {
        ja[im] <- trunc(0.01 * jy)
        jul[im] <- jul + 2 - ja[im] + trunc(0.25 * ja[im])
    }
    julday <- jul
    invisible(julday)
  }



TAAC2016 <- function() {



  #if (!file.exists("GHE/R/atmOverturning.rda")) {
#  ncid <- open.ncdf("~/data/ERAINT/eraint_w.nc")
#  lon = get.var.ncdf(ncid,"longitude")
#  lat=get.var.ncdf(ncid,"latitude")
#  lev=get.var.ncdf(ncid,"levelist")
#  tim=get.var.ncdf(ncid,"time")
#  nz <- length(lev); nt <- length(tim); nx <- length(lon); ny <- length(lat)
#  wa <- matrix(rep(NA,nz*nt),nz,nt)

  # Area of the grid boxes
#  box.area <- rep(cos(pi*lat/180)* pi*min(diff(lon))/180*
#                  abs(pi*min(diff(lat)))/180*(6.378e03)^2,nx)
#  dim(box.area) <- c(ny,nx); box.area <- t(box.area)
#  
#  for (iz in 1:nz){
#    w = get.var.ncdf(ncid,"w",start=c(1,1,iz,1),count=c(nx,ny,1,nt))
#
#    ysrt <- order(lat)
#    image(lon,lat[ysrt],box.area[,ysrt],
#          main=paste("Check: level=",lev[iz],"hPa"))
#    addland()
#    contour(lon,lat[ysrt],w[,ysrt,1],add=TRUE)
#  
#    for (it in  1:nt) {
#      X <- c(w[,,it]*box.area)
#      wa[iz,it] <- var(X)
#    }
#  }
#  close.ncdf(ncid)
#  atmOverturning <- list(wa=wa,tim=tim,lon=lon,lat=lat,lev=lev)
#  save(file="GHE/data/atmOverturning.rda",atmOverturning)
#} else {
#  load ("GHE/data/atmOverturning.rda")
#  nz <- length(lev); nt <- length(tim); nx <- length(lon); ny <- length(lat)
#}

data(atmOverturning,envir=environment());
#load("GHE/data/atmOverturning.rda");
#attach(atmOverturning)
wa <- atmOverturning$wa; tim <- atmOverturning$tim
lon <- atmOverturning$lon; lat <- atmOverturning$lat
lev <- atmOverturning$lev
nz <- length(lev); nt <- length(tim); nx <- length(lon); ny <- length(lat)

Z <- seq(-10,100000,by=10)
heights <- round(0.1*approx(x=p.hydrostatic(Z),y=Z,xout=lev)$y)*10

upper <- heights >= 6500
middle <- (heights < 6500) & heights >= 1000
lower <- heights < 1000
date <- caldat( atmOverturning$tim/24 + julday(1,1,1900) )
yymm <- date$year + (date$month - 0.5)/12

#print("T-t-prof:")
#ncid2 <- open.ncdf(tempfil)
#temp2 <- get.var.ncdf(ncid2,"TEMP")
#tim2 <-  get.var.ncdf(ncid2,"TIME")
#lev <-  get.var.ncdf(ncid2,"LEVELIST")
#close.ncdf(ncid2)
#
#date2 <- caldat( tim2/24 + julday(1,1,1900) )
#yymm2 <- date2$year + (date2$month - 0.5)/12
#Tt.prof <- list(temp=temp2,tim=tim2,lev=lev,date=date2,yymm=yymm2)
#save(file="GHE/data/Tt.prof.rda",Tt.prof)

data(Tt.prof,envir=environment())
#load("GHE/data/Tt.prof.rda")
tim2 <- Tt.prof$tim; temp2 <- Tt.prof$temp
date2 <- caldat( tim2/24 + julday(1,1,1900) )
yymm2 <- date2$year + (date2$month - 0.5)/12

#print("T-z-prof:")
#ncid3 <- open.ncdf(proffil)
#temp3 <- get.var.ncdf(ncid3,"PROF")
#close.ncdf(ncid3)
#nt2 <- length(tim2)
#Tz.prof <- temp3
#save(file="GHE/data/Tz.prof.rda",Tz.prof)


nt2 <- length(tim2)

#print("Q:")
#ncid4 <- open.ncdf(qfil)
#Q <- get.var.ncdf(ncid4,"Q")
#close.ncdf(ncid4)
#save(file="GHE/R/Q.rda",Q)

data(Q,envir=environment())
#load("GHE/data/Q.rda")
#attr(Q,'yymm') <- yymm2
#save(file="GHE/R/Q.rda",Q)

#print("ERAINT model levels:")
# http://www.ecmwf.int/products/data/technical/model_levels/model_def_60.html
#levpres <- read.table("eraint-model.levels.txt",header=TRUE)
#save(file="GHE/R/levpres.rda",levpres)
data(levpreseraint,envir=environment())
#load("GHE/data/levpreseraint.rda")

alt3 <- round(0.1*approx(x=p.hydrostatic(Z),y=Z,xout=levpreseraint)$y)*10
t254k <- rep(NA,nt2)

print("Define atmospheric levels:")
troposphere <- alt3 < 12000
alt <- alt3[troposphere]
temp2 <- temp2[troposphere,]

overturning <- ma.filt(colMeans(wa[middle,]) - mean(wa[middle,]),12)
overturning.cal <- data.frame(y=ma.filt(colMeans(wa[middle,]),12),
                              x=yymm - mean(yymm))


# Graphics
# PRL Figure 1:

dev.new()
par(yaxt="n",xaxt="s",bty="n")
y <- stand(overturning) + 1.5; 
plot(yymm, y,type="n",
     main="Atmospheric 'overturning' anomaly",
     ylab="",xlab="Time",xlim=c(1989,2010),ylim=c(-3.5,6),
     sub="data source: ERAINT")
par(col.axis="black")
axis(1)
grid()

prettygraph(yymm, y)
prettygraph(yymm, 0.5*stand(ma.filt(colMeans(wa[upper,]),12))+4,col="steelblue")
prettygraph(yymm, 0.5*stand(ma.filt(colMeans(wa[lower,]),12))-1.80,col="darkgreen")

abline(lm(y ~yymm),lty=2)
Avz.trend <- summary(lm(y ~ x,data=overturning.cal))$coefficients
c <- round(Avz.trend)
text(2000,5.25,expression(frac(sum( (a %*%v[z] - bar(a %*%v[z]) )^2,i=1,n),n)),pos=4)
text(2005,0.0,paste("trend:",round(c[1]/100)," [",
                      round(c[3]/100),"]  +",
                      round(c[2]/10)," [",round(c[4]/10),
                      "] (hPa/s km^2)^2/decade"),
     cex=0.6,col="grey30")
print("'Overturning metric':")
print(summary(lm(y ~ x,data=overturning.cal)))

lines(rep(1994.5,2),c(-10000,15000),lwd=1,lty=3,col="darkblue")
lines(rep(1999,2),c(-10000,15000),lwd=1,lty=3,col="darkblue")
lines(rep(2007.35,2),c(-10000,15000),lwd=1,lty=3,col="darkblue")
text(1994.5,6,"1994",cex=0.75,col="darkblue")
text(1999.5,6,"1999",cex=0.75,col="darkblue")
text(2007.35,6,"2007",cex=0.75,col="darkblue")

legend(1988.5,6.25,
       c("var(a v_z) (above 6.5km)","var(a v_z) (1km - 6.5km)",
         "var(a v_z) (below 1km)"),
       col=c("steelblue","black","darkgreen"),
       lwd=5,bg="grey95",cex=0.75)

#dev2bitmap("prl2011-fig1.png",res=150)
#dev2bitmap("prl2011-fig1.jpg",type="jpeg",res=200)
#dev.copy2eps(file="prl2011-fig1.eps")
#dev2bitmap("prl2011-fig1.pdf",type="pdfwrite")

dev.new()
# PRL Figure 2:

# Emission level height:
print(length(t254k)); print(dim(temp2)); print(length(alt))
print(range(alt))
print(range(c(temp2)))
for (i in 1:nt) t254k[i] <- approx(x=temp2[,i],y=alt,xout=254)$y
Z <- stand(ma.filt(t254k,12))+3;


# Total water vapour content:
yymm <- attr(Q,'yymm') 
hum.cal <- data.frame(y=ma.filt(colSums(Q),12),
                      x=attr(Q,'yymm')  - mean(attr(Q,'yymm')))
Q <- 0.5*stand(ma.filt(colSums(Q),12)) - 1

par(yaxt="n",xaxt="s",bty="n")
plot(yymm, y,type="n",lwd=4,
     main="Atmospheric bulk emission level and moisture",
     ylab="",xlab="Time",xlim <- c(1989,2010),ylim=c(-3,6),
     sub="ERAINT/Sunspots")
par(col.axis="black")
axis(1)
grid()

prettygraph(yymm, Z)
prettygraph(yymm, Q,col="steelblue")
emissionlev.cal <- data.frame(y=ma.filt(t254k,12),x=yymm - mean(yymm))

# Emission level height trend:
abline(lm(Z ~ yymm),lty=2,col="grey30")
t254k.trend <- summary(lm(y ~ x,data=emissionlev.cal))
c <- round(t254k.trend$coefficients,2)
text(2004.5,4.90,paste("Z_T254K:",c[1]," [",c[3],"] +",
                     10*c[2],"[",10*c[4],"] m/decade"),
     cex=0.75,col="grey")
print("'Z_T254K':")
print(summary(lm(y ~ x,data=emissionlev.cal)))

# Total water vapour content trend:
abline(lm(Q ~ yymm,),lty=2,col="blue")
t254k.trend <- summary(lm(y ~ x,data=hum.cal))
c <- round(t254k.trend$coefficients,2)
text(1999,-2.7,paste("q_tot:",c[1]," [",c[3],"] +",
                  10*c[2]," [",10*c[4],"] kg m**-2/decade"),
     cex=0.75,col="darkblue")
print("q_tot:")
print(summary(lm(y ~ x,data=hum.cal)))

lines(rep(1994.5,2),c(-10000,15000),lwd=1,lty=3,col="darkblue")
lines(rep(1999,2),c(-10000,15000),lwd=1,lty=3,col="darkblue")
lines(rep(2007.35,2),c(-10000,15000),lwd=1,lty=3,col="darkblue")
text(1994.5,6,"1994",cex=0.75,col="darkblue")
text(1999.5,6,"1999",cex=0.75,col="darkblue")
text(2007.35,6,"2007",cex=0.75,col="darkblue")

legend(1988.5,6.25,
       c("Z(T=254k)","Q tot."),
       col=c("black","steelblue"),
       lty=c(1,1),lwd=5,bg="grey95",cex=0.75)

#dev2bitmap("prl2011-fig2.png",res=150)
#dev2bitmap("prl2011-fig2.jpg",type="jpeg",res=200)
#dev.copy2eps(file="prl2011-fig2.eps")
#dev2bitmap("prl2011-fig2.pdf",type="pdfwrite")


# Supporting diagnostics

#load("GHE/data/Tz.prof.rda")
data(Tz.prof,envir=environment());
temp3 <- Tz.prof
#load("GHE/data/levpres.rda")
data(levpres,envir=environment())

Z <- seq(-10,100000,by=10)
alt3 <- round(0.1*approx(x=p.hydrostatic(Z),y=Z,xout=levpres$ph)$y)*10
print("Supporting diagnostics")
cols <- rgb(sin(pi*seq(0,1,length=nt2)),
            seq(1,0,length=nt2),
            seq(0,1,length=nt2))
print(Z)
print(levpres$ph)
print(temp3)
print(alt3)

dev.new()
plot(temp3,alt3/1000,type="l",lwd=3,
     main="Vertical T-profile from ERAINT",
     sub="Global mean averaged over 1989-2010",
     xlab="Temperature (K)",ylab="Altitude (km)")
grid()
lines(range(temp3),rep(mean(t254k)/1000,2),lty=2)
text(215,5,"mean emission level altitude",cex=0.75)

trop <- (alt3/1000) < 12
t <- temp3[trop]; z <- alt3[trop]/1000; srtz <- order(z)
calibr <- data.frame(t=t[srtz],z=z[srtz])
lapserate <- lm(t ~ z,data=calibr)
lines(predict(lapserate,newdata=calibr),z[srtz],lty=2,col="red")
text(220,20,paste("Lapse rate=",
                  round(summary(lapserate)$coefficients[2],2),"K/km"),col="red")

LR <- rep(NA,nt2)
temp4 <- temp2; d <- dim(temp4)
z <- alt/1000; srtz <- order(z)
for (j in 1:d[1]) temp4[j,] <- ma.filt(temp4[j,],12)
for (i in 1:nt2) {
  t <- temp4[,i]
  if (sum(is.finite(t))==d[1]) {
    calibr <- data.frame(t=t[srtz],z=z[srtz])
    lapserate <- lm(t ~ z,data=calibr)
    lines(predict(lapserate,newdata=calibr),z[srtz],lty=2,col=cols[i])
    LR[i] <- summary(lapserate)$coefficients[2]
  }
}

text(265,45,"Lapse rate")

colorbar(yymm=yymm2,fig=c(0.2,0.4,0.70,0.80),breaks=0:nt2,col=cols)

fig.old <- c(0,1,0,1)
par(fig=c(0.53,0.93,0.32,0.55),new=TRUE,cex.axis=0.75,xaxt="s",yaxt="s",
    cex.main=0.75)
plot(yymm2,LR,type="l",xlab="",ylab="",col="blue")
grid()
par(fig=fig.old,new=TRUE)

#dev2bitmap("prl2011-figS2.png",res=150)
#dev2bitmap("prl2011-figS2.jpg",type="jpeg",res=200)
#dev.copy2eps(file="prl2011-figS2.eps")
#dev2bitmap("prl2011-figS2.pdf",type="pdfwrite")


dev.new()
plot(temp3,alt3/1000,type="l",lwd=3,ylim=c(0,12),
     main="Vertical Temperature Profile",
     sub="Global mean averaged over 1989-2010 (ERAINT)",
     xlab="Temperature (K)",ylab="Altitude (km)")
grid()
lines(range(temp3),rep(mean(t254k)/1000,2),lty=2)
text(215,7,"mean emission level altitude",cex=0.75)

trop <- (alt3/1000) < 12
t <- temp3[trop]; z <- alt3[trop]/1000; srtz <- order(z)
calibr <- data.frame(t=t[srtz],z=z[srtz])
lapserate <- lm(t ~ z,data=calibr)
lines(predict(lapserate,newdata=calibr),z[srtz],lty=2,col="red")
text(220,20,paste("Lapse rate=",
                  round(summary(lapserate)$coefficients[2],2),"K/km"),col="red")

LR <- rep(NA,nt2)
temp4 <- temp2; d <- dim(temp4)
z <- alt/1000; srtz <- order(z)
for (j in 1:d[1]) temp4[j,] <- ma.filt(temp4[j,],12)
for (i in 1:nt2) {
  t <- temp4[,i]
  if (sum(is.finite(t))==d[1]) {
    calibr <- data.frame(t=t[srtz],z=z[srtz])
    lapserate <- lm(t ~ z,data=calibr)
    lines(predict(lapserate,newdata=calibr),z[srtz],lty=2,col=cols[i])
    LR[i] <- summary(lapserate)$coefficients[2]
  }
}

text(265,45,"Lapse rate")

dev.new()
srtz <- order(alt)
image(yymm2,alt[srtz]/1000,t(temp2[srtz,]),
      main="Global mean vertical T-profile",
      ylab="Altitude (km)",xlab="Time")
contour(yymm2,alt[srtz]/1000,t(temp2[srtz,]),add=TRUE)
grid()

#dev2bitmap("prl2011-figS3.png",res=150)
#dev2bitmap("prl2011-figS3.jpg",type="jpeg",res=200)
#dev.copy2eps(file="prl2011-figS3.eps")
#dev2bitmap("prl2011-figS3.pdf",type="pdfwrite")
}
