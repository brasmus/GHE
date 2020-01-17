# This is an -script
# by R.E. Benestad, 14.06.2010
# R can be downloaded from http://cran.r-project.org 
# Fleagle & Businger (1980) 'An introduction to Atmospheric Physics'  eq. 5.18, p. 215
# to execute, write 'source("blackbody.R")' at the R-prompt.

blackbody <- function(h=6.64e-34,k=1.38e-23,c=3.00e8,HITRAN=TRUE) {
  alpha<-2*h*c^2

  Ll <- function(Temp,l,alpha) alpha/( l^5 * (exp(h*c/(k*l*Temp))-1) )


  i<-seq(log(100),log(20000),by=0.01)
  l<-exp(i) * 1.00e-9

  sun.bb<-Ll(6000,l,alpha)
  earth.bb<-Ll(288,l,alpha)

  T1 <- Ll(250,l,alpha)
  T2 <- Ll(200,l,alpha)
  T3 <- Ll(300,l,alpha)
  T4 <- Ll(350,l,alpha)
  T5 <- Ll(150,l,alpha)
  T6 <- Ll(500,l,alpha)

  map <- matrix(rep(NA,length(i)*10),length(i),10)
  for (ii in 1:length(i)) map[ii,] <- ii

  x <- l*1.00e9; y <- seq(1,10,length=10)
  map[(x < 380) | (x > 750),] <- NA

  spectrum <- rgb(seq(0,1,length=100),
                sin(pi*seq(0,1,length=100)),
                seq(1,0,length=100))

  par(ps=12,bg="black",col.axis="white",col.main="white",col.lab="white")
  plot(l*1.00e9,sun.bb,type="l",log="xy",lwd=3,
     main="Solar & Terestrial heatflux",
     xlab="wave length [nm]",col="yellow",
     ylab="Flux density",ylim=c(1,1e15))

  co2bands <- c(4.25,14)*1000
  for (i in 1:length(co2bands)) {
    lines(rep(co2bands[i],2),c(1,1e8),lwd=3,col="grey10")
  }
  
  image(x,y,map,col=spectrum,add=TRUE)
  text(15,1.0e12,"Planck's law",cex=2,col="white")

  lines(l*1.00e9,T1,lwd=1,col="grey")
  lines(l*1.00e9,T2,lwd=1,col="grey")
  lines(l*1.00e9,T3,lwd=1,col="grey")
  lines(l*1.00e9,T4,lwd=1,col="grey")
  lines(l*1.00e9,T5,lwd=1,col="grey")
  lines(l*1.00e9,T6,lwd=1,col="grey")

  lines(l*1.00e9,earth.bb,lwd=3,col="green")

  grid()
  text(500,8.0e11,"6000K",col="yellow")
  text(1e4,5.0e7,"288K",col="green")
  
  if (HITRAN) {
    abp <- HITRAN(plot=FALSE)
    lambda <- 10000000/abp$waveNum
    S <- filter(abp$S,rep(1,500)/500)
    lines(lambda,S,lwd=5,col="white")
    lines(lambda,S,col="red",lwd=3)
  }
  #dev2bitmap(file="blackbody.png",res=150)
  #dev2bitmap("blackbody.pdf",type="pdfwrite")
}
