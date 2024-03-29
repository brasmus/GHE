\name{toymodel}
\alias{toymodel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Toy model
%%  ~~function to do ... ~~
}
\description{
  The function solves the toy model equation for the emission heigth
  making use of the energy balance equation.
  
  \deqn{\frac{z_e}{dt} \approx \frac{-1}{\gamma c_s} \left[
    \frac{S_0}{4} (1-A)  -  \sigma (T_E - \gamma z_E)^4 + \int_0^H
    e^{-\tau z} \rho \sigma (T_E - \gamma (z_E - z))^4 dz - \frac{\rho
      10^{(11.40-2353/[T_E - \gamma z_E])}}{2} \eta_z(t)
    \right]}{dz/dt=-[S/4*(1-A) - s(T-gz)**4 + F(z) - Lr(z)/2 e]/(gc)}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
toymodel(t0 = 1, t1 = 10, N = 1000, S0 = 1361, A = 0.3, gamma = -5, eta = 1+0.5*cos(seq(0,30,length=1000)), sigma = 5.67 * 10^-8, cs = 1000, tau = 10, rho = 0.1, plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t0}{start time
%%     ~~Describe \code{t0} here~~
}
  \item{t1}{end time
%%     ~~Describe \code{t1} here~~
}
  \item{N}{number of steps
%%     ~~Describe \code{N} here~~
}
  \item{S0}{The 'solar constant' (W/m**2)
%%     ~~Describe \code{S0} here~~
}
  \item{A}{The albedo
%%     ~~Describe \code{A} here~~
}
  \item{gamma}{The lapse rate (C/km)
%%     ~~Describe \code{gamma} here~~
}
  \item{eta}{The overturning rate
%%     ~~Describe \code{eta} here~~
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
}
  \item{cs}{specific heat of the ground
%%     ~~Describe \code{cs} here~~
}
  \item{tau}{optic depth
%%     ~~Describe \code{tau} here~~
}
  \item{rho}{density of the air at surface
%%     ~~Describe \code{rho} here~~
}
  \item{plot}{TRUE - plot the results
%%     ~~Describe \code{plot} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
toymodel()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
