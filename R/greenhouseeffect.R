

rot <- function(x,angle=80) {
  angle <- pi*angle/180
#  R <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)),2,2)
  R <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)),2,2)
#  print(R)
  
  y <- x
  for (i in 1:length(x$x)) {
    y$x[i] <-  cos(angle)*x$x[i] + sin(angle)*x$y[i]
    y$y[i] <- -sin(angle)*x$x[i] + cos(angle)*x$y[i]
  }
#  y <- R %*% t(x)
  y
}

sol <- function(x,y,r=10) {
  yellows=c("yellow3","yellow2","yellow1","yellow")
  ii <- 1
  for (R in seq(r,r*0.3,length=length(yellows))) {
    X <- R*cos(seq(0,2*pi,length=369)) + x
    Y <- R*sin(seq(0,2*pi,length=369)) + y
    polygon(X,Y,col=yellows[ii],border=yellows[ii])
    ii <- ii + 1
  }

  for (i in seq(0,2*pi,by=2*pi/36)) {
    lines(c(r+5,r+10)*cos(i) + x,
          c(r+5,r+10)*sin(i) + y,lty=2,lwd=2,col="yellow")
  }
}

piler <- function(lw,x,y) {
  lw$x <- lw$x - lw$x[1] + x
  lw$y <- lw$y - lw$y[1] + y
  lines(lw$x,lw$y,lwd=5,col="pink")
  lines(lw$x,lw$y)  
}

testrot <- function() {
  y <- list(x=c(0,0,1),y=c(1,0,0))
  plot(y,type="b",xlim=c(-1,1),ylim=c(-1,1),main="Test rotation")
  for (i in 1:360) points(cos(pi*i/180),sin(pi*i/180),pch=".")
  grid()
  lines(rot(y),col="red",type="b")     
  points(rot(y),col="red",pch=19)
}


IR <- function(x,levels) {
  xx <- seq(0,8,length=1000)
  LW <- list(x=.75*c(xx,8,9,7.5,9,7.5),y=0.25*c(cos(0.56*pi*xx),0,0,2,0,-1))
  lw.up1 <- rot(LW,angle=-80);  lw.dn1 <-  rot(LW,angle=70)
  lw.up2 <- rot(LW,angle=-110); lw.dn2 <-  rot(LW,angle=110)
  piler(lw.up1,x,0)

  LW <- list(x=2.0*c(x,8,9,7.5,9,7.5),y=0.5*c(cos(0.56*pi*x),0,0,2,0,-1))
  lw.up <- rot(LW,angle=-80); lw.dn <-  rot(LW,angle=70)

  for (i in 1:length(levels)) {
    if (i%%2==1) {X <- x; lw.up <- lw.up1; lw.dn <- lw.dn1} else
                     {X <- x; lw.up <- lw.up2; lw.dn <- lw.dn2}
    piler(lw.up,X,y=levels[i]+11)
    piler(lw.dn,X,levels[i]+9)
  }
}


greenhouseeffect <- function() {
system("rm *.gif")

for (ii in seq(0,20,by=5)) {
plot(c(0,100),c(-10,100),type="n",main="The Greenhouse effect",xlab="Temperature",ylab="Height")

levels <- seq(0,50+ii,by=20)

x <- seq(0,8,length=1000)
SW <- list(x=c(10*x,80,90,75,90,75),y=c(cos(10.05*pi*x),0,0,2,0,-2))
sw <- rot(SW,angle=80)

col <- paste("grey",100-ii,sep="")
polygon(c(0,100,100,0,0),c(0,0,200,200,0),col=col,border="grey70")
polygon(c(0,100,100,0,0),c(0,0,-10,-10,0),col="darkgreen",density=15,lwd=2)
 
iv <- 1; 
for (i in levels) {
   text(90,i+10,iv,cex=2,type=2,col="grey40"); iv <- iv+1
}

sol(-3,90)

for (i in 1:length(levels)) lines(c(0,100),rep(levels[i],2),col="grey60",lty=3)

lines(sw$x,sw$y+80,lwd=5,col="yellow")
lines(sw$x,sw$y+80)


IR(x=30,levels=levels)
x <- seq(0,8,length=1000)
LW <- list(x=1.5*c(x,8,9,7.5,9,7.5),y=0.25*c(cos(0.56*pi*x),0,0,2,0,-1))
lw.up <- rot(LW,angle=-85)
piler(lw.up,x=30,y=50+ii+10)


lines(seq(60,30,length=100),seq(0,60,length=100),lwd=5,col="lightblue")
lines(seq(60+ii*2/3,30,length=100),seq(0,60+ii,length=100),lwd=5,col="blue")


lines(c(30,60+ii*2/3),rep(0,2),lwd=5)
for (i in seq(30,60+ii,by=5)) points(i,0,pch=19,cex=0.5,col="grey70")
      
#text(50,101,paste("F(GHG concentrations)= X +",ii,"dX"),font=2,cex=1.3,col="grey")

lines(rep(30,2),c(-20,80),lty=2)

text(40,60+ii,"T",cex=2); text(42,58+ii,"e")

if (ii < 10) cii <- paste("0",ii,sep="") else
             cii <- as.character(ii)
png.file <- paste("drivhuseffekten",cii,".png",sep="")
gif.file <- paste("drivhuseffekten",cii,".gif",sep="")

dev2bitmap(file=png.file,res=200)
system(paste("convert ",png.file,gif.file))
system(paste("rm ",png.file))
}

system("gifmerge -l0 -15 drivhuseffekten*.gif > GHE-anim.gif")
}


GHEschematic <- function() {

  #require(jpeg)

  dev.new(width=10,height=10)
  par(bty="n",xaxt="n",yaxt="n",pty="m",mar = c(0, 0, 0, 0), xpd=NA)
  image.name <- "~/skyocean.jpg"
  img <- readJPEG(image.name)

  plot(c(0,100),c(-10,100),type="n",
     main="",xlab="",ylab="")
  rasterImage(img,-20,130, 130,300, angle=-90)
  for (ii in c(0,20)) {
    levels <- seq(0,50+ii,by=20)

    x <- seq(0,8,length=1000)
    SW <- list(x=c(10*x,80,90,75,90,75),y=c(cos(10.05*pi*x),0,0,2,0,-2))
    sw <- rot(SW,angle=80)

    col <- paste("grey",100-ii,sep="")
 
    iv <- 1; 
    for (i in levels) {
       text(90,i+10,iv,cex=2,type=2,col="grey40"); iv <- iv+1
    }

    sol(-3,90)

    for (i in 1:length(levels))
      lines(c(0,100),rep(levels[i],2),col="grey60",lty=3)

    lines(sw$x,sw$y+80,lwd=5,col="yellow")
    lines(sw$x,sw$y+80)


    IR(x=30,levels=levels)
    x <- seq(0,8,length=1000)
    LW <- list(x=1.5*c(x,8,9,7.5,9,7.5),y=0.25*c(cos(0.56*pi*x),0,0,2,0,-1))
    lw.up <- rot(LW,angle=-85)
    piler(lw.up,x=30,y=50+ii+10)

    lines(seq(60,30,length=100),seq(0,60,length=100),lwd=9)
    lines(seq(60+ii*2/3,30,length=100),seq(0,60+ii,length=100),lwd=9)
    lines(seq(60,30,length=100),seq(0,60,length=100),lwd=7,col="lightblue")
    lines(seq(60+ii*2/3,30,length=100),seq(0,60+ii,length=100),lwd=7,col="blue")

    lines(c(30,60+ii*2/3),rep(0,2),lwd=5)
    for (i in seq(30,60+ii,by=5)) points(i,0,pch=19,cex=0.5,col="grey70")
      
    lines(rep(30,2),c(-20,80),lty=2)
    text(40,60+ii,"T",cex=2); text(42,58+ii,"e")

    dev2bitmap(file="GHEschematic.png",res=150)
  }

}
