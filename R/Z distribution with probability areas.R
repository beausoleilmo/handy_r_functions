#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# November 4, 2020 at 13:17
# Why: Show the characteristics of a Z distribution
# Output: 
# Requires: 
# NOTES: 
#### ### ### ## #### ### ### ## #### ### ### ## 

library(scales)
x = seq(-3,3, length.out = 100)
plot(dnorm(x) ~ x, type = 'l', 
     main = "Normal distribution", 
     ylab = "Density")
mtext(side=3, line=0, 
      # at=0, adj=0, 
      cex=1, 
      text = expression("P(-z"[alpha*"/2"]*"<Z<"*"z"[alpha*"/2"]*")=1-"*alpha))

z.low <- qnorm(alpha/2)
z.up <- qnorm(1-(alpha/2))
s.x.low  <- c(z.low, seq(z.low, z.up, 0.01), z.up)
s.y.low  <- c(0, dnorm(seq(z.low, z.up, 0.01)), 0)
polygon(x = s.x.low, y = s.y.low, col="pink")

abline(v = 0, lty = 3)
text(0, .1,labels = expression("1-"*alpha), col = "black",cex =2)


# Area of shade (from https://www.r-bloggers.com/2012/06/shading-regions-of-the-normal-the-stanine-scale/)
transparency = 0.8

alpha = 0.10
from.z.low <- -100
to.z.low <- qnorm(alpha/2)
from.z.up <- qnorm(1-(alpha/2))
to.z.up <- 100

col = "red"
s.x.low  <- c(from.z.low, seq(from.z.low, to.z.low, 0.01), to.z.low)
s.y.low  <- c(0, dnorm(seq(from.z.low, to.z.low, 0.01)), 0)
polygon(x = s.x.low, y = s.y.low, col=col)

s.x.up  <- c(from.z.up, seq(from.z.up, to.z.up, 0.01), to.z.up)
s.y.up  <- c(0, dnorm(seq(from.z.up, to.z.up, 0.01)), 0)
polygon(x = s.x.up, y = s.y.up, col=col)

col.text = "white"
text( 2, .02,labels = expression(alpha*"/2"), col = col.text,cex =2)
text(-2, .02,labels = expression(alpha*"/2"), col = col.text,cex =2)



axis(side = 1, at = to.z.low, padj = 2,
     tcl = -2.0,
     # tick = 8,
     labels = expression("-Z"[alpha*"/2"]),
     lwd.ticks = 2,
     lty = 2)
axis(side = 1, at = from.z.up, padj = 2,
     tcl = -2.0,
     # tick = 8,
     labels = expression("Z"[alpha*"/2"]),
     lwd.ticks = 2,
     lty = 2)
# abline(v = c(to.z.low#,
#              # from.z.up
#              ),lwd = 2,lty = 2)
segments(x0 = from.z.up, x1 = from.z.up, y0 = 0, y1 = dnorm(from.z.up),lwd = 2,lty = 2)
segments(x0 = to.z.low, x1 = to.z.low, y0 = 0, y1 = dnorm(to.z.low),lwd = 2,lty = 2)
