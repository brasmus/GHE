# This is an -script
# by R.E. Benestad, 14.06.2010
# R can be downloaded from http://cran.r-project.org 
# equations based on Hartmann (1994)
# to execute, write 'source("planetsradiativebalance.R")' at the R-prompt.


largesymbols <- function(x,y,col="blue",cex=3) {
  N <- 100
  others <- seq(0.3,0.9,length=N)^2 
  if (col=="blue") cols <- rgb(others,others,sqrt(seq(0.1,1,by=N))) else
  if (col=="red") cols <- rgb(sqrt(seq(0.1,1,by=N)),others,others) else
  if (col=="green") cols <- rgb(others,sqrt(seq(0.1,1,by=N)),others) else
  if (col=="yellow") cols <- rgb(sqrt(seq(0.1,1,by=N)),sqrt(seq(0.1,1,by=N)),others)
  for (i in 1:N) {
    xofs <- 0.01*i/N*max(x,na.rm=TRUE); yofs <- 0.01*i/N*max(y,na.rm=TRUE)
    sizefact <- 1 - (i-1)/N*0.9
    points(x+xofs,y+yofs,pch=19,cex=cex*sizefact,col=cols[i])
  }
}

T.e <- function(R,A,S0=1367,rho=5.67e-8) {
  S <- S0/(R^2)
  T.e <- (S*(1-A)/4/rho)^0.25
  T.e
}

solarsystem <- function() {

np <- 13
pch <- rep(19,np)
col <- rep("grey20",np)
col[3] <-"blue"
pch[4] <- 21
pch[11:13] <- 4 


X <- matrix(rep(NA,np*3),np,3)
rownames(X)  <- c("Mercury","Venus","Earth","Moon","Mars","Jupiter",
                  "Saturn","Uranus","Neptun","Pluto",
                  "Titan","Europa","Triton")
colnames(X) <- c("R","Albedo","Mean.T")

# From:
#Planetary database
#Source for planetary data, and some of the data on
#the moons, is http://nssdc.gsfc.nasa.gov/planetary/factsheet/
# ~/data/planets.txt
X[1,] <- c(0.387, 0.119, 440.0)
X[2,] <- c(0.723, 0.750, 737.0)
X[3,] <- c(1.000, 0.306, 288.0)
X[4,] <- c(1.000, 0.110, 0.5*(100+400))
X[5,] <- c(1.524, 0.250, 210.0)
X[6,] <- c(5.203, 0.343, 165.0)
X[7,] <- c(9.539, 0.342, 134.0)
X[8,] <- c(19.181,0.300,  76.0)
X[9,] <- c(30.058,0.290,  72.0)
X[10,] <- c(39.5, 0.500,  50.0)
X[11,] <- c(9.539,0.21,   95.0)
X[12,] <- c(5.203,0.67,  103.0)
X[13,] <- c(30.058,0.76,  34.5)

# From http://www.astro-tom.com/getting_started/planet_classification.htm
#      http://www.solarviews.com/eng/moon.htm
# A from Houghton (1986) The Physics of Atmospheres
# http://www.astronomytoday.com/astronomy/mercury.html
# http://www.universetoday.com/guide-to-space/the-moon/moon-albedo/
# 
#R <-   c(0.387,0.723, 1,   1,    1.524, 5.203, 9.539, 19.181, 30.058, 39.5)
#A <-   c(0.06, 0.77,  0.30,0.12,0.15, 0.58, )
#Tsd <- c(350,  480,   15, 107, -23,   -150,  -180,  -214,   -220,   -230)
#Tsn <- c(-170, 480,   15, -153,  -23,   -150,  -180,  -214,   -220,   -230)
#Ts <-  0.5*(Tsd + Tsn) + 273.15

srt <- order(X[,1])
R <- X[srt,1]; A <- X[srt,2]; Ts <- X[srt,3];
col <- col[srt]; pch <- pch[srt]

plot(R,Ts,main="Energy balance",xlab="distance from sun (Au)",
     ylab="Surface temperature (K)",type="n",,cex=1.5)
points(R,T.e(R,A),col="red",pch=19,type="b",lty=2)
grid()
dev2bitmap("plantesinradiativebalance0.jpg",type="jpeg")

plot(R,Ts,main="Energy balance",xlab="distance from sun (Au)",
     ylab="Surface temperature (K)",pch=pch,col=col,cex=1.5)
points(R,T.e(R,A),col="red",pch=19,type="b",lty=2)
grid()
legend(4,700,c("measured","predicted"),pch=19,lty=c(0,2),col=c("grey","red"))

polygon(c(20,40,40,20,20),c(400,400,750,750,400),border="white",col="white")
fig=c(0.45,0.9,0.45,0.85)
fig.old <- c(0,1,0,1)
par(fig=fig,new=TRUE,mar=c(4,4,0,0),cex.axis=0.7)
plot(Ts,T.e(R,A),pch=pch,col=col,cex=1.5,
     xlab="measured",ylab="predicted",xlim=c(0,800),ylim=c(0,800))
lines(c(0,500),c(0,500),col="darkred",lty=2)
grid()

#dev2bitmap("plantesinradiativebalance0.png",res=200)
#dev2bitmap("plantesinradiativebalance0.pdf",type="pdfwrite")

dev.new()
par(ps=12,bg="black",col.axis="white",col.main="white",col.lab="white",
    cex.lab=1.5)
plot(Ts,T.e(R,A),pch=pch,col=col,cex=1.5,
     main="Our Solar System: Surface & Emission Temperature",
     sub="GHE affects observed temperature, but is not accounted for in predictions",
     xlab="measured",ylab="predicted",xlim=c(0,800),ylim=c(0,800))
lines(c(0,800),c(0,800),col="red",lty=2,lwd=2)
largesymbols(Ts,T.e(R,A),col="blue",cex=3)
grid()
text(725,180,"Venus",cex=1.5,col="white")
#dev2bitmap("planetsinradiativebalance.png",res=200)
#dev2bitmap("plantesinradiativebalance.pdf",type="pdfwrite")

attr(Ts,'T.e') <- T.e(R,A)
attr(Ts,'object') <-  rownames(X)
attr(Ts,'X') <- X
invisible(Ts)
}
