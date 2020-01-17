#require(deSolve)

GeoChaos <- function(X.0=c(-0.1,0.1,0),N=10000,R=0.1,nu=1,sigma=5) {
  # Lorenz force: F = q ( E + v x B)
  # Magnetic field flux: M*i_L
  # F_Lorentz = M I_L I_B
  # F_friction = F omega

  dX.dt <- function(t,y,parms) {
    # R:
    # v:
    # sigma:
    omega <- y[1]; i.B <- y[2]; i.L <- y[3]
    with(as.list(parms), {
      domega.dt <-  R - i.L*i.B - nu * omega
      di.B.dt <- omega*i.L - i.B
      di.L.dt <- sigma*(i.B - i.L)
      Y <- c(domega.dt,di.B.dt,di.L.dt)
      list(Y)     
    })
  }

  y <- X.0
  attr(y,'names') <- c("omega","i.B","i.L")
  parms <- c(R = R, nu = nu, sigma=sigma)
  time <- seq(0,0.1*N,length=N)  
  out <- as.data.frame(rk4(y,time,dX.dt,parms))
  
  main <- "Geodynamo chaos"
  par(xaxt="n",yaxt="n",bty="n", xpd=NA)
  plot(out$omega,out$i.B,type="b",pch=19,lty=1,cex=0.5,main=main,
       xlab=expression(omega),ylab=expression(i[B]),col="grey")

  dx <- ( max(out$omega) - min(out$omega) )/15
  legend(max(out$omega)-3*dx,max(out$i.B),
         c(expression(nu==phantom(0)),expression(sigma==phantom(0)),
           expression(R==phantom(0)),nu,sigma,R),bty="n",ncol=2)

  dy <- ( max(out$i.B) - min(out$i.B) )/15
  text(min(out$omega),min(out$i.B),
       expression(frac(d*omega,dt)==R - i[L]*i[B] - nu*omega),pos=4)
  text(min(out$omega),min(out$i.B)+dy,
       expression(frac(d*i[B],dt)==omega* i[L] - i[B]),pos=4)
  text(min(out$omega),min(out$i.B)+2*dy,
       expression(frac(d*i[L],dt)==sigma*(i[B]-i[B])),pos=4)
  invisible(out)
}

GeoChaos() -> geochaos
