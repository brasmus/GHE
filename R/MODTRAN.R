## Emission from earth: RADIANCE(WATTS/CM2-STER-XXX)

MODTRAN <- function(h=6.64e-34,k=1.38e-23,c=3.00e8) { 
  
 # i<-seq(log(2),log(200000),by=0.01)
#  l<-exp(i) * 1.00e-9
  print('MODTRAN')
  #read.table('MODTRAN.dat',header=TRUE) -> MODTRAN
  data("MODTRAN")
  par(bty='n')
  plot(MODTRAN$FREQ,1.0e7*MODTRAN$TOTAL,type='l',
       main='MODTRAN spectral lines',sub='http://climatemodels.uchicago.edu/modtran/',
       xlab=expression(1/cm),ylab=expression(10^7 * W/m^2 * cm^{-1}))
  polygon(c(MODTRAN$FREQ,0),c(1.0e7*MODTRAN$TOTAL,0),col="grey")

  ## MODIS: w/m^2 / (1.0-2 m)
  print('HERE')
  ## Placks law units: W/m^2 / 1.0e-6m
  # Unit: W /m^2 (1.0-6m)^1 sr^1
  scal <- 1.0e-7*1.0e3
  l <- MODTRAN$WAVLEN*1.0e-6
  T1 <- Ll(250,l,alpha) * scal
  T2 <- Ll(200,l,alpha) * scal
  T3 <- Ll(300,l,alpha) * scal
  T4 <- Ll(350,l,alpha) * scal
  T5 <- Ll(150,l,alpha) * scal
  T6 <- Ll(500,l,alpha) * scal
  Te <- Ll(254,l,alpha) * scal
  Ts <- Ll(288,l,alpha) * scal
  k <- MODTRAN$FREQ
  # print(Te)
  # lines(k,T1,lwd=1,col="grey",lty=2)
  # lines(k,T2,lwd=1,col="grey",lty=2)
  # lines(k,T3,lwd=1,col="grey",lty=2)
  # lines(k,T4,lwd=1,col="grey",lty=2)
  # lines(k,T5,lwd=1,col="grey",lty=2)
  # lines(k,T6,lwd=1,col="grey",lty=2)
  # lines(k,Te,lwd=1,col="blue",lty=1)
  # lines(k,Ts,lwd=1,col="green",lty=1)
  # invisible(MODTRAN)
}