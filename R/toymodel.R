## R.E. Benestad
## \frac{z_e}{dt} \approx \frac{-1}{\gamma c_s} \left[ \frac{S_0}{4} (1-A)  -  \sigma (T_E - \gamma z_E)^4 + \int_0^H e^{-\tau z} \rho \sigma (T_E - \gamma (z_E - z))^4 dz - \frac{\rho 10^{(11.40-2353/[T_E - \gamma z_E])}}{2} \eta_z(t) \right]

## eta = overturning index
## gamma = lapse rate
## S0 = solar constant
## cs = specific heat of the ground
## tau = the optical depth
## rho = density of air

toymodel <- function(x0=0.1,t0=1,t1=10,N=1000,
                     S0=1361,A=0.3,gamma=-5,
                     eta=0.01*rnorm(1000),
                     sigma=5.67*10^-8,cs=1000,tau=seq(7,8,length=1000),
                     rho=0.1,plot=TRUE) {
    require(deSolve)

    ## Generate functions Eta
    if (length(eta)==1) eta <- rep(eta,N)
    lhs <- eta[1]
    for (i in 2:length(eta)) lhs <- paste(lhs,eta[i],sep=",")
    funccode <- paste("Eta = function(t) { y=c(",lhs, 
                      "); x=seq(",t0,",",t1,",length=",length(eta),
                      "); Y=approx(x,y,t,n=",N,",rule=2)$y; Y}")
    eval(parse(text=funccode))

    ## Generate functions Tau
    if (length(tau)==1) tau <- rep(tau,N)
    lhs <- tau[1]
    for (i in 2:length(tau)) lhs <- paste(lhs,tau[i],sep=",")
    funccode <- paste("Tau = function(t) { y=c(",lhs, 
                      "); x=seq(",t0,",",t1,",length=",length(tau),
                      "); Y=approx(x,y,t,n=",N,",rule=2)$y; Y}")
    eval(parse(text=funccode))
    
    ## Diff eq. to integrate using 4th order Runge-Kutta:
    dXdt <- function(t,X,parms) {
        z <- X[1]
        ## http://en.wikipedia.org/wiki/Gas_constant#Specific_gas_constant
        R.s <- 8314.32/18  # Nm/kmol K
        rho.z <- rho*exp(seq(0,-3,length=100))
        Z <- seq(0,z+1,length=100)
        dz <- (z+1)/100
        tau.t <- Tau(t); eta.t=Eta(t)
        L <- 2260000  # Latent heat of vaporisation: J/kg
        with(as.list(parms), {
            ## Estimate the emission temperature based on the
            ## solar constant and albedo
            Te <- exp(log(S0*(1-A)/(4*sigma))/4)
            ## Multiply latent heat w gas const & temp and div by 2 
            L <- L/(2 * R.s * (Te - gamma*z))
            ## The Downward IR flux from atmosphere:
            F.dlw <- sum(exp(-tau.t*Z)*rho.z*sigma*(Te - gamma*z + Z)^4)*dz
            print(c(X,F.dlw,Te,Te - gamma*z,max(Z)))
            ## The diff. equ:
            dzdt <- S0*(1-A)/4 - sigma*(Te - gamma*z)^4 + F.dlw -
                    L * 10^(11.40-2353/(Te - gamma*z))*eta.t
            dzdt <- -dzdt/(gamma * cs)
            list(c(z=dzdt,eta=eta.t,tau=tau.t))
        } )
    }
    
    
    parms <- c(S0=S0,A=A,gamma=gamma,sigma=sigma,
               cs=cs,rho=rho)
    Te <- exp(log(S0*(1-A)/(4*sigma))/4)
    X <- c(x0,Eta(t0),Tau(t0))
    time <- seq(t0,t1,length=N)
    h <- diff(time)[1]
    attr(X,'names') <- c('z','eta','tau')
    print('4th-order Runge-Kutta:')
    out <- as.data.frame(rk4(X,time,dXdt,parms))
    if (plot) {
        par(bty="n",xaxt="n",pty="m",las=1,xpd=NA)
        plot(time,out$z,type="l",lwd=4,main='Toy greenhouse model',
             col="grey",sub=paste("Emission temperature=",round(Te),'K'),
             xlab="t",ylab=expression(z[e]))
        grid()
    }
    invisible(out)
}
