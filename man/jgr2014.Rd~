\name{GHE}
\alias{GHE}
\alias{GHEschematic}
\alias{GeoChaos}
\alias{LorenzSystem}
\alias{greenhouseeffect}
\alias{p.hydrostatic}
\alias{stand}
\alias{ma.filt}
\alias{sunspots}
\alias{forcings}
\alias{gcr}
\alias{blackbody}
\alias{co2}
\alias{co2.temp}
\alias{vostoc}
\alias{colorbar}
\alias{prettygraph}
\alias{largesymbols}
\alias{jgr2014}
\alias{IR}
\alias{rot}
\alias{sol}
\alias{piler}
\alias{testrot}
\alias{HITRAN}
\alias{T.e}
\alias{solarsystem}
\alias{Keelercurve}
\alias{gauss.filt}
\alias{elninoyrs}
\alias{prepareData}
\alias{AtmOverturning}
\alias{emissionHeigth}
\alias{Hulbert1931Tz}
\alias{supportDiag}
\alias{olr.corr}
\alias{caldat}
\alias{julday}
\title{Greenhouse effect and climate.}
\description{
Functions for plotting scematics and carrying out some simple climate
analysis plus a number of other functions.

\code{p.hydrostatic} the pressure at a given height under the assumption
of hydrostatic balance.
\code{forcings} Time series plot of solar forcing. \code{gcr} retrieves
galactic cosmic ray data and \code{sunspots} the sunspot data.
\code{jgr2014} R-code for reproducing figures in the publication JGR2014.
\code{HITRAN} function plotting IR spectral lines.
\code{solarsystem} Emission temperature of the objectys in the solar system.
\code{Keelercurve} time series of CO2.
\code{blackbody} code to plot the Planck function - blackbody radiation
\code{co2}
\code{co2.temp} plots scatteer plots between ln(CO2) and temperature.
}
\usage{
p.hydrostatic(z,p0 = 1013.250,Temp = 288,g = 9.81,
              k = 1.38e-23,M = 0.027/6.022e+23)
sunspots(url="http://sidc.oma.be/DATA/monthssn.dat")
forcings()
gcr()
jgr2014()
HITRAN(param=2, plot = TRUE)
solarsystem()
Keelercurve(url="ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_mm_mlo.txt",
            urlbg="http://www.realclimate.org/images/raffinery.jpg")
blackbody(h=6.64e-34,k=1.38e-23,c=3.00e8,HITRAN = TRUE)
co2(url="ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_mm_mlo.txt",
    plot=TRUE,Temp=TRUE)
co2.temp()
vostoc()
T.e(R,A,S0=1367,rho=5.67e-8)
stand(x,ii=NULL)
ma.filt(x, n)
colorbar(yymm,fig=c(0.33,0.66,0.30,0.34),breaks,col)
prettygraph(t,y,col="black",tshadeoffset=0.1,yshadeoffset=0.1)
largesymbols(x,y,col="blue",cex=3)
IR(x,levels)
rot(x,angle=80)
sol(x,y,r=10)
piler(lw,x,y)
testrot()
gauss.filt(x, n)
elninoyrs(ylim=c(-10,10),yrs=NULL,col="grey90",lwd=7) 
prepareData(tempfil= "~/ferret_scripts/ncep-ta.nc") 
AtmOverturning() 
emissionHeigth(ncep=TRUE) 
Hulbert1931Tz(x,xb=3300,S=1.35e6,rho=5.67e-8,alpha=0.10) 
supportDiag() 
olr.corr()
GHE()
GHEschematic()
GeoChaos(X.0=c(-0.1,0.1,0),N=10000,R=0.1,nu=1,sigma=5)
LorenzSystem(X.0=c(-0.1,0.1,0),N=100000,sigma=10,beta=8/3, rho=28)
greenhouseeffect()
caldat(julian)
julday(mm, id, iyyy)
}
\arguments{
    \item{z}{height in m.}
    \item{p0}{mean pressure at mean sea-level}
    \item{Temp}{surface temperature}
    \item{g}{acceleration due to gravity in m/s^2}
    \item{k}{Boltzmann constant}.
    \item{M}{mass ratio for air}
    \item{url}{URL of data source.}
    \item{urlbg}{URL of background graphics.}
    \item{param}{parameter}
    \item{h}{Planck constant in Js}
    \item{c}{speed of light in m/s}
    \item{R}{distance from the sun}
    \item{A}{planetary albedo}
    \item{S0}{'solar constant'}
    \item{x}{time series/horizontal coordinate or parameter for equation in Hulbert (1931)}.
    \item{y}{time series/vertical coordinate}.
    \item{angle}{angle in degrees}.
    \item{n}{filter window width}.
    \item{levels}{Vertical levels}.
    \item{t}{time axis}.
    \item{lw}{long-wave}.
    \item{HITRAN}{TRUE: use the prvided HITRAN data}
    \item{plot}{TRUE: plot the results}
    \item{fig}{see \code{\link{par}}}
    \item{yymm}{year and month}
    \item{breaks}{see \code{\link{hist}}}
    \item{tshadeoffset}{graphics adjustmant}
    \item{yshadeoffset}{graphics adjustmant}
    \item{cex}{see \code{\link{par}}}
    \item{r}{radius}
    \item{tempfil}{File name for temperature data after processing with Ferret}
    \item{ylim}{see \code{\link{plot}}}
    \item{lwd}{see \code{\link{plot}}}
    \item{yrs}{years}
    \item{col}{see \code{\link{plot}}}
    \item{ncep}{FLAG - TRUE plot NCEP}
    \item{xb}{parameter for equation in Hulbert (1931)}
    \item{S}{The 'solar constant'}
    \item{rho}{Stefan-Boltzman's constant or parameters for the for Lorenz/GeoChaos system.}
    \item{alpha}{The albedo}
    \item{X.0}{initial conditions for Lorenz/GeoChaos system.}
    \item{N}{length of integration}
    \item{sigma}{parameters for the for Lorenz/GeoChaos system.}
    \item{beta}{parameters for the for Lorenz/GeoChaos system.}
    \item{nu}{parameters for the for Lorenz/GeoChaos system.}
    \item{julian}{julian day}
    \item{mm}{month}
    \item{id}{day}
    \item{iyyy}{year}
}
\author{R.E. Benestad}
\examples{

print("emissionHeigth()")
emissionHeigth()

print("AtmOverturning()")
AtmOverturning()

print("olr.corr()")
olr.corr()

# other plots...

print("jgr2014()")
jgr2014()

\dontrun{
print("co2()")
co2()
}
print("LorenzSystem()")
LorenzSystem()

print("GeoChaos()")
GeoChaos()

print("GHE()")
GHE()

print("greenhouseeffect()")
greenhouseeffect()

print("blackbody()")
blackbody()

print("solarsystem()")
solarsystem()

print("supportDiag()")
supportDiag() 
}
\keyword{manip}

