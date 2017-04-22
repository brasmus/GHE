# Demo: showing the absorption band of some greenhouse gases and the
# windows which are probed by the AVHRR onboard satellites:
h2o <- HITRAN(param=1,plot=FALSE)
ch4 <- HITRAN(param=6,plot=FALSE)
o3 <- HITRAN(param=3 ,plot=FALSE)
co2 <- HITRAN(xlim = c(0.5, 20), ylim = c(0.001, 100))
lines(10000/h2o$waveNum, gauss.filt(h2o$S, 1000),lwd=2,col="blue")
lines(10000/ch4$waveNum, gauss.filt(ch4$S, 1000),lwd=2,col="darkgreen")
lines(10000/o3$waveNum, gauss.filt(o3$S, 1000),lwd=2,col="pink")
rect(0.58,0.0001,0.68,1000,col=rgb(0.5,0.5,0.5,0.2),border='grey')
rect(0.725,0.0001,1,1000,col=rgb(0.5,0.5,0.5,0.2),border='grey')
rect(1.58,0.0001,1.64,1000,col=rgb(0.5,0.5,0.5,0.2),border='grey')
rect(3.55,0.0001,3.93,1000,col=rgb(0.5,0.5,0.5,0.2),border='grey')
rect(10.3,0.0001,11.3,1000,col=rgb(0.5,0.5,0.5,0.2),border='grey')
rect(11.5,0.0001,12.5,1000,col=rgb(0.5,0.5,0.5,0.2),border='grey')
legend(0.5,100,c(expression(CO[2]),expression(CH[4]),
                 expression(H[2]*O),expression(O[3])),
       lty=1,lwd=2,col=c('black','darkgreen','blue','pink'),bty='n')
