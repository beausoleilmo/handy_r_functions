### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Created on November 29, 2017 at 10:02
# Marc-Olivier Beausoleil 
# Draws sections of the density plot to be highlighted 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# set.seed(1)
sd2 = 1.95996398454005423552
x = rnorm(10000,mean = 10,sd = 1)
y = x + rnorm(length(x))
# plot(y~x)
# hist(x)

plotdensity <- function(x, q1, q2) {
  dens <- density(x)
  plot(dens)
  q75 <- quantile(x, q1)
  q95 <- quantile(x, q2)
  x1 <- min(which(dens$x >= q75))  
  x2 <- max(which(dens$x <=  q95))
  with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="gray"))
}

plotdensity(x,
            q1 = 0.025,
            q2 = 0.975)
abline(v = mean(x))

x.s = scale(x)
plotdensity(x.s,
            q1 = 0.025,
            q2 = 0.975)
abline(v = c(mean(x.s), 1, 1.96,2))
# hist(x.s)
sd(x.s)
dens <- density(x)

hist(x.s[which(x.s >= -1.96 | x.s<=1.96)])
hist(x.s[which(x.s <= -1.96 | x.s>=1.96)])



#### Draw density on both sides 
set.seed(1)
draws <- rnorm(100)^2
dens <- density(draws)
plot(dens)

q2     <- 2
q65    <- 6.5
qn08   <- -0.8
qn02   <- -0.2

x1 <- min(which(dens$x >= q2))  
x2 <- max(which(dens$x <  q65))
x3 <- min(which(dens$x >= qn08))  
x4 <- max(which(dens$x <  qn02))

with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="gray"))
with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col="gray"))

