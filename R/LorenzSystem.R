#require(deSolve)

LorenzSystem <- function(X.0=c(-0.1,0.1,0),N=100000,sigma=10,beta=8/3, rho=28) {
  # Lorenz force: F = q ( E + v x B)
  # Magnetic field flux: M*i_L
  # F_Lorentz = M I_L I_B
  # F_friction = F omega

  dX.dt <- function(t,y,parms) {
    with(as.list(parms), {
      X <- y[1]; Y <- y[2]; Z <-  y[3]
      dx.dt <- sigma*(Y - X) 
      dy.dt <- X*(rho - Z) - Y
      dz.dt <- X*Y - beta*Z
      yy <- c(dx.dt,dy.dt,dz.dt)
      list(yy)
    })
  }

  y <- X.0
  attr(y,'names') <- c("x","y","z")
  parms <- c(sigma=sigma, beta=beta, rho=rho)
  time <- seq(0,0.001*N,length=N)  
  out <- as.data.frame(rk4(y,time,dX.dt,parms))
  
  main <- "The Lorenz system"
  par(xaxt="n",yaxt="n",bty="n", xpd=NA)
  m <- 200
  col <- rgb(seq(1,0,length=m)^2,
             sin(pi*(1:m)/m)^2,
             seq(0,1,length=m)^2)
  colind <- round((out$z - min(out$z))/(max(out$z)-min(out$z))*m)
  colind[colind<1] <- 1; colind[colind>m] <- m
  colours <- col[colind]
  
  plot(out$x,out$y,pch=19,cex=0.3,main=main,
       xlab=expression(x),ylab=expression(y),col=colours)

  dx <- ( max(out$x) - min(out$x) )/10
  dy <- ( max(out$y) - min(out$y) )/10

  legend(max(out$x)-3*dx,min(out$y)+dy,
         c(expression(sigma==phantom(0)),expression(beta==phantom(0)),
           expression(rho==phantom(0)),sigma,beta,rho),bty="n",ncol=2)

  text(min(out$x),max(out$y),
       expression(frac(d*x,dt)==sigma*(y-x)),pos=4)
  text(min(out$x),max(out$y)-dy,
       expression(frac(d*y,dt)==x*(rho -z) -y),pos=4)
  text(min(out$x),max(out$y)-2*dy,
       expression(frac(d*z,dt)==x*y - beta*z),pos=4)
  invisible(out)
}

LorenzSystem() -> strangeattractor
