co2 <- function(url="ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_mm_mlo.txt",
                plot=TRUE,Temp=TRUE) {

  # http://cdiac.ornl.gov/ftp/trends/co2/maunaloa.co2

  
  #Mauna.Loa <- read.table(url)
  #nrows.co2 <- length(co2.test) - 20
  # 1958   3    1958.208      315.71      315.71      314.61     -1

  Mauna.Loa <- read.table(url,
                          col.names=c("year","month","yymm","ave","interp",
                                "trend","no.days"))
  #Mauna.Loa[Mauna.Loa<0] <- NA
  co2 <- Mauna.Loa$interp
  yy <- Mauna.Loa$year
#  yy <- as.matrix(Mauna.Loa[,1])
#  yymm <- sort(rep(yy,12) + (rep(1:12,length(yy)) - 0.5)/12)
  yymm <- Mauna.Loa$yymm
  years <- as.numeric(rownames(table(yy)))
  ny <- length(years)
  CO2 <- rep(NA,ny)
  for (i in 1:ny) {
    ii <- is.element(yy,years[i])
    if (sum(ii)==12) CO2[i] <- mean(co2[ii],na.rm=TRUE)
  }
  
  use <- yymm > 1900
  yymm <- yymm[use]
  co2 <- co2[use]
  yy <- yy[use]

  if (plot) {
    plot(yymm,co2,type="l",lwd=2,col="green",
         main="Mauna Loa CO2",xlab="Time",ylab="concentration (ppmv)")
    grid()
    points(yymm,co2,pch=19,cex=0.5)

    if (Temp) {
      data(T2m.ncep)
      i1 <- is.element(years,year.ncep)
      i2 <- is.element(year.ncep,yy)
      c <- cor.test(stand(log(CO2[i1])),T2m.ncep[i2])
      print(c)
    }

    dev.new()
    plot(yymm,stand(Mauna.Loa$trend),type="l",lwd=3,col="green",
         main="Mauna Loa CO2 & NCEP T(2m)",xlab="Time",ylab="standardised",
         sub=paste("Cor=",round(c$estimate,3),"p-value=",round(c$p.value,3)))
    grid()
    points(yymm,co2,pch=19,cex=0.35,col="grey")

    lines(year.ncep,stand(T2m.ncep),lwd=2)
    dev2bitmap("co2_t2m.png",res=150)
  }
  results <- list(year=years,CO2.am=CO2,yymm=yymm,co2=co2)
  invisible(results)
}
  
Keelercurve <- function(
          url="ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_mm_mlo.txt",
          urlbg="http://www.realclimate.org/images/raffinery.jpg") {
  
  #require(jpeg)
  if (!file.exists("raffinery.jpg"))
     download.file(urlbg,"raffinery.jpg")
  img <- readJPEG("raffinery.jpg")

  Mauna.Loa <- read.table(url,
                          col.names=c("year","month","yymm","ave","interp",
                                "trend","no.days"))
  #Mauna.Loa[Mauna.Loa<0] <- NA
  co2 <- Mauna.Loa$interp
  yy <- Mauna.Loa$year
#  yy <- as.matrix(Mauna.Loa[,1])
#  yymm <- sort(rep(yy,12) + (rep(1:12,length(yy)) - 0.5)/12)
  yymm <- Mauna.Loa$yymm
  

  dev.new(width=10,height=8)
  par(bty="n",xaxt="n",yaxt="n",mar=rep(0,4),fig=c(0,1,0,1))
  plot(yymm,co2, type='n')
  rasterImage(img, 0.999*min(yymm), 0.99*min(co2),
              1.001*max(yymm), 1.01*max(co2))
  lines(yymm,co2,lwd=11,col="white")
  lines(yymm,co2,lwd=9,col="grey30")
  text(mean(yymm),max(co2),"CO2 concentrations (Mauna Loa)",font=2,cex=1.75)
  text(min(yymm),min(co2),"1958",col="white",pos=4)
  text(max(yymm),min(co2),"1958",col="white",pos=2)
  dev2bitmap("CO2.png",res=150)
}
