gheonearth <- function(url.earth="http://eoimages.gsfc.nasa.gov/images/imagerecords/36000/36019/AS8-16-2593_lrg.jpg",url.sun="http://planetfacts.org/wp-content/uploads/2011/03/Sun.png") {
  
  image.sun <- 'sun.jpg'
  image.earth <- 'earth.jpg'
  
  #library(jpeg)
  #library(animation)
  library(plotrix)
  N <- 30
  
  mklayer <- function(path="ghe") {
    # Saves the graphics in numbered JPEG files
    if (!file.exists(path)) dir.create(path)
    list <- list.files(path=path)
    num <- substr(as.character(1000 + length(list)+1),2,4)
    #dev2bitmap(type="pngalpha",file=paste(path,"/layer-",num,".png",sep=""),res=150)
  }
  
  # Draw the background: Earth & Sun
  newfig <- function(img.earth,img.sun) {
    #dev.new(width=10,height=10)
    par(xaxt="n",yaxt="n",bty="n",mar=rep(0,4),bg="black",col.main="white")
    plot(c(0,1),c(0,1),type="n",xlab="",ylab="")
    rasterImage(img.earth,0.7,0.5,1.5,1.3,angle=180)
    rasterImage(img.sun,0,0.7,0.3,1)
    
    # axis:
    arrows(0.0,0.45,0.0,0.75,lty=2,col="white")
    arrows(0.0,0.45,0.75,0.45,lty=2,col="white")
    text(0.7,0.4,'Temperature',col="white")
    text(0,0.7,'height',srt=90,col="white",pos=2)
    arrows(0.15,0.85,0.25,0.45,col="yellow",length=0.1)
    arrows(0.14,0.85,0.24,0.45,col="yellow",length=0.1)
    arrows(0.16,0.85,0.26,0.45,col="yellow",length=0.1)
    
    lines(rep(0.3,2),c(0.45,0.70),lty=3,col="grey")
    text(0.3,0.435,expression(T[E]))
    
    text(0.8,0.95,"Energy balance:",col="grey30")
    text(0.8,0.9,expression(frac((1-A),4)*S==sigma*T[E]^4),col="grey30")
    text(0.5,0.97,"The Greenhouse effect",col="white",cex=1.25,font=2)
    
    r <- 0.5 
    x <- r*cos(pi*seq(0,360,by=1)/180) + 0.3
    y <- r*sin(pi*seq(0,360,by=1)/180) + 0.1
    lines(x,y,col="grey",lty=3)
    h <- r + 0.1; dTdz=6*0.01
    lines(c(0.3,h-0.15),c(h,0.45),lty=3,col="steelblue")
  }
  
  IR <- function(h) {
    n <- 100
    x <- 0.01*sin(4*pi*seq(0,1,length=n)) + 0.3
    y <- seq(h,h+0.1,length=n)
    lines(x,y,lty=2,col="pink")
    arrows(0.3,h+0.1,0.31,h+0.11,length=0.05,lty=2,col="pink")
  }
  
  # Download a photographic image of the Earth:
  if (!file.exists(image.earth)) {
    download.file(url.earth,image.earth)
  }
  img.earth <- readJPEG(image.earth)
  
  if (!file.exists(image.sun)) {
    download.file(url.sun,'sun.png')
    system("convert sun.png sun.jpg")
  }
  img.sun <- readJPEG(image.sun)
  
  # Begin animation loop
  # Note the brackets within the parentheses
  
  #for (i in 1:N) {
  frame <- function(i) { 
    newfig(img.earth,img.sun)
    r <- 0.5 + 0.1*(i-1)/N
    x <- r*cos(pi*seq(0,360,by=1)/180) + 0.3
    y <- r*sin(pi*seq(0,360,by=1)/180) + 0.1
    if (i>1) draw.circle(0.3,0.1,r,col=rgb(1,1,1,(i-1)/(2*N)),border=NULL)
    lines(x,y,lwd=2,col="grey")
    #arrows(0.67,0.1,r+0.3,0.1,lwd=5,col="grey20",length=0.1)
    h <- r + 0.1
    lines(c(0.3,h-0.15),c(h,0.45),lwd=3,col="steelblue")
    IR(h)
    #mklayer()
    #dev.off()
  }
  
  
  #ani.options(interval=.05)
  #im.convert("ghe/*.png", output = "ghe.gif")
  
  saveGIF({for (i in 1:N) frame(i)},
                 movie.name="GHEonEarth.gif")
}
