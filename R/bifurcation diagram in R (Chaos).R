#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# Sunday, January 3, 2021 at 21:55
# Why: plot a bifurcation diagram in R 
# Output: 
# Requires: 
# NOTES: See also https://rpubs.com/DistribEcology/880 and https://magesblog.com/post/2012-03-17-logistic-map-feigenbaum-diagram/ 
#### ### ### ## #### ### ### ## #### ### ### ## 

# Source: https://magesblog.com/post/2012-03-17-logistic-map-feigenbaum-diagram/ from Markus Gesmann
logistic.map <- function(r, x, N, M){
  ## r: bifurcation parameter
  ## x: initial value
  ## N: number of iteration
  ## M: number of iteration points to be returned
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r *z[i]  * (1 - z[i])
  }
  ## Return the last M iterations 
  z[c((N-M):N)]
}

## Set scanning range for bifurcation parameter r
my.r <- seq(2.5, 4, by=0.003)
system.time(Orbit <- sapply(my.r, logistic.map,  x=0.1, N=1000, M=300))
##   user  system elapsed (on a 2.4GHz Core2Duo)
##   2.910   0.018   2.919 

Orbit <- as.vector(Orbit)
r <- sort(rep(my.r, 301))

plot(Orbit ~ r, pch=".")

library(compiler) ## requires R >= 2.13.0
logistic.map <- cmpfun(logistic.map) # same function as above
my.r <- seq(2.5, 4, by=0.001)
N <- 2000; M <- 500; start.x <- 0.1
orbit <- sapply(my.r, logistic.map,  x=start.x, N=N, M=M)
Orbit <- as.vector(orbit)
r <- sort(rep(my.r, (M+1)))
plot(Orbit ~ r, pch=".", col=rgb(0,0,0,0.05))

