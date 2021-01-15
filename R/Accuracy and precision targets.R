### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Created on October 28, 2020 at 14:58
# Marc-Olivier Beausoleil 
# Shows targets with accuracy and precision examples
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

library(scales)
# Accuracy vs precision  --------------------------------------------------
par(mfrow = c(2,2))
library(plotrix)
draw.acc.prec <- function(n, mean, sd,title, seed=NULL) {
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  x = rnorm(n, mean, sd)
  y = rnorm(n, mean, sd)
    plot(NA, 
         xlim = c(-12,12), 
         ylim = c(-12,12), 
         asp = 1,
         xaxt="n",yaxt="n", bty="n", axes = FALSE,
         ylab ="",xlab = "")
  x.circ = 0
  y.circ = 0
  blue.cool = rgb(53/256,146/256,173/256)
  red.hot = rgb(217/256,30/256,36/256)
  propsize = seq(from = 1, to = 10, by =2 )
  plotrix::draw.circle(x.circ, y.circ, radius =  propsize[5], 
                       nv = 1000, 
                       border = NULL,
                       col = blue.cool, lty = 1, lwd = 1)
  plotrix::draw.circle(x.circ, y.circ, radius =  propsize[4], 
                       nv = 1000, 
                       border = NULL,
                       col = "white", lty = 1, lwd = 1)
  plotrix::draw.circle(x.circ, y.circ, radius =  propsize[3], 
                       nv = 1000, 
                       border = NULL,
                       col = red.hot, lty = 1, lwd = 1)
  plotrix::draw.circle(x.circ, y.circ, radius =  propsize[2], 
                       nv = 1000, 
                       border = NULL,
                       col = "white", lty = 1, lwd = 1)
  
  plotrix::draw.circle(x.circ, y.circ, radius =  propsize[1], 
                       nv = 1000, 
                       border = NULL,
                       col = red.hot, lty = 1, lwd = 1)
  abline(h=0,v=0,lty = 3)
  points(x,y, pch =20, col = "darkgrey", cex = 2)
  mean.x = mean(x)
  mean.y = mean(y)
  alp.gold = alpha(colour = "gold",alpha = .5)
  points(mean.x,mean.y, pch =21, bg = alp.gold, col = "black", cex =2)
  title(title)
  
}
# Accuracy is the mean (when accuracy is HIGH, mean is 0)
# Precision is the sd (when precision is HIGH, sd is LOW)
draw.acc.prec(10,0,1,  title = "High accuracy \nHigh precision", seed = 1234)
draw.acc.prec(10,0,3,  title = "High accuracy \nLow precision", seed = 1234)
draw.acc.prec(10,4,1,  title = "Low accuracy \nHigh precision", seed = 1234)
draw.acc.prec(10,-3,4, title = "Low accuracy \nLow precision", seed = 1234)

