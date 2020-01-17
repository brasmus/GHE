


GHE <- function() {

par(col.axis="white")
plot(c(0,100),c(-10,100),type="n",main="The Greenhouse effect",xlab="Temperature",ylab="Height")
x <- seq(0,8,length=1000)

for (ii in 0:20) {
  col <- paste("grey",100-ii,sep="")
  polygon(c(0,50,50,0,0),c(0,0,70-ii,70-ii,0),col=col,border=col)
}
for (ii in 0:30) {
  col <- paste("grey",100-ii,sep="")
  polygon(c(50,100,100,50,50),c(0,0,90-ii,90-ii,0),col=col,border=col)
}

lines(rep(50,2),c(0,100),lwd=3)
lines(c(0,50),rep(60,2),lty=2)
lines(c(50,100),rep(75,2),lty=2)

LW <- list(x=1.5*c(x,8,9,7.5,9,7.5),y=0.25*c(cos(0.56*pi*x),0,0,2,0,-1))
lw.up <- rot(LW,angle=-85)
piler(lw.up,x=30,y=60)
piler(lw.up,x=70,y=75)

x <- seq(0,8,length=1000)
SW <- list(x=c(10*x,80,90,75,90,75),y=c(cos(10.05*pi*x),0,0,2,0,-2))
sw <- rot(SW,angle=80)
lines(sw$x,sw$y+80,lwd=5,col="yellow")
lines(sw$x,sw$y+80)

arrows(95,0,95,95,lwd=3)
text(93,50,"HEIGHT",srt=90,font=2)

polygon(c(0,100,100,0,0),c(0,0,-10,-10,0),col="darkgreen",density=15,lwd=2)
dev2bitmap(file="GHE0.png",res=200)
dev2bitmap(file="GHE0.pdf",type="pdfwrite")

test <- readLines("http://cdiac.ornl.gov/ftp/trends/co2/maunaloa.co2")
Mauna.Loa <- read.table("http://cdiac.ornl.gov/ftp/trends/co2/maunaloa.co2",skip=14,
                        header=TRUE,nrows=length(test) - (15 + 4),sep="\t")
Mauna.Loa[Mauna.Loa<0] <- NA

x <- seq(30,70,length=length(Mauna.Loa$Average))
polygon(c(28,72,72,28,28),c(10,10,43,43,10),col="white")
lines(x,(Mauna.Loa$Average - 300)*0.3 + 10)
text(50,40,"CO2 Mauna.Loa")

arrows(30,12,70,12,length = 0.1)
text(50,15,"time")
arrows(30,12,30,40,length = 0.1)
text(32,23,"conc.",srt=90)

arrows(40, 20, 20, 50,col="steelblue",lwd=3)
arrows(65, 33, 80, 60,col="steelblue",lwd=3)

dev2bitmap(file="GHE.png",res=200)
dev2bitmap(file="GHE.pdf",type="pdfwrite")

}
