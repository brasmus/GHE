# Based on information in
#"http://geosci.uchicago.edu/~rtp1/PrinciplesPlanetaryClimate/Courseware/
#     PlanetaryClimateCourseware/ChapterScripts/Chapter4Scripts/PyTran.py"

HITRAN <- function(param=2,plot=TRUE) {

elem <- switch(as.character(param),
  "1"="H2O", "21"="HOCl",
  "2"="CO2", "22"="N2",
  "3"="O3",  "23"="HCN",
  "4"="N2O", "24"="CH3Cl",
  "5"="CO",  "25"="H2O2",
  "6"="CH4", "26"="C2H2",
  "7"="O2",  "27"="C2H6",
  "8"="NO",  "28"="PH3",
  "9"="SO2", "29"="COF2",
  "10"="NO2","30"="SF6",
  "11"="NH3","31"="H2S",
  "12"="HNO3","32"="HCOOH",
  "13"="OH", "33"="HO2",
  "14"="HF", "34"="O",
  "15"="HCl","35"="ClONO2",
  "16"="HBr", "36"="NO+",
  "17"="HI", "37"="HOBr",
  "18"="ClO","38"="C2H4",
  "19"="OCS","39"="CH3OH",
  "20"="H2CO")

mol.weight <- switch(as.character(param),
   "1"=18,"2"=44,"3"=48,"4"=40,"6"=16,"9"=64,"14"=20,"15"=36)                  

N.avogadro <- 6.0221415e23

widths <- c(2,1,12,10,10,5,5,10,4,8,15,15,15,15,6,12,1,7,7)
col.names <- c("iso","unknown1","waveNum","lineStrength","unknown2",
               "airWidth","selfWidth","Elow","TExp","unknown3",
               "unknown4","unknown5","unknown6","unknown7","unknown8",
               "unknown9","unknown10","unknown11","unknown12")

if (param < 10) cparam <- paste("0",param,sep="") else
                cparam <- as.character(param)
URL <- paste("http://geosci.uchicago.edu/~rtp1/PrinciplesPlanetaryClimate/",
             "Data/WorkbookDatasets/Chapter4Data/hitran/ByMolecule/",
             cparam,"_hit04.par",sep="")

abp <- read.fwf(URL,widths=widths,col.names=col.names)
dWave = .01*(abp$p/1.e5)
abp$S = 0.1*(N.avogadro/mol.weight)*abp$lineStrength

if (plot) {
  par(cex.sub=0.65)
  plot(10000/abp$waveNum,gauss.filt(abp$S,1000),type="l",log="xy",
     main=paste(elem,"Line absorption (HITRAN)"),sub=URL,
     xlab="wave length [micrometers]",ylab="Line strength [(m^2/kg)(cm^-1)]",
     xlim=c(1,20),ylim=c(0.1,100),lwd=3)
  polygon(c(10000/abp$waveNum,max(10000/abp$waveNum,na.rm=TRUE),
          min(10000/abp$waveNum,na.rm=TRUE)),
        c(gauss.filt(abp$S,1000),rep(min(abp$S,na.rm=TRUE),2)),
        col="steelblue",lwd=3,border="black")
  grid()
}
attr(abp,"element") <- elem
invisible(abp)
}
