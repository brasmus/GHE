dT <- 30              # years - time interval
D <- 10               # m     - depth
DT <- 0.5             # K     - temperature change
  
melt <- 250 * 1000^3  #m^3
rho <- 0.92*1000      # kg/m^3
a <-  6378000         # m
s2y <- 3600*24*365.25 # s
H.fus <- 333.55*1000  # J/kg http://en.wikipedia.org/wiki/Enthalpy_of_fusion
C <- 4.18*1000        # J/(kg K) 

# F.in - F.out = F.deficit

E.ice <- H.fus * melt * rho
E.water <- C * 4*pi*a^2 * D * DT
  
F.deficit.ice <- E.ice/(4*pi*a^2*s2y*dT) 

F.deficit.water <- E.water/(4*pi*a^2*s2y*dT)

print(c(E.ice,E.water,E.ice+E.water))
print(c(F.deficit.ice,F.deficit.water,F.deficit.ice+F.deficit.water))
