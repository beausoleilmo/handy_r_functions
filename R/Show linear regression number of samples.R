#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# 13 October 2020
# Why: This script is showing what is the effect of changing the sample size on the variability in the estimation of the linear regression parameters
# Output: Cool Graphs
# Requires: your head!
# NOTES: 
#### ### ### ## #### ### ### ## #### ### ### ## 

par(mfrow = c(2,2))
for (i in 1:4) {
  a = 0 # intercept
  b = 1 # slope 
  mu.error = 0
  sd.error = 1
  # Change the sample size to see the effect on the linear regression 
  sample.size = 5
  
  seq.x.from = 0
  seq.x.to = 10
  x = seq(seq.x.from,seq.x.to, length.out = 100)
  y = a + b*x + rnorm(length(x),mu.error,sd.error)
  
  my.lin.dat= cbind(x,y)
  pts= my.lin.dat[sample(nrow(my.lin.dat),size = sample.size),]
  
  plot(pts, pch = 20,  
       xaxt='n',yaxt='n',
       xlab='',ylab='',
       xlim = c(seq.x.from,seq.x.to), ylim = c(-5,seq.x.to*(b*1.1)))
  abline(a,b, col = "blue",lwd = 3)
  lm.out.sample = lm(pts[,"y"]~pts[,"x"])
  abline(lm.out.sample, col = "red", lty = 2, lwd = 3)
}
mtext(paste0("n=",sample.size), side = 3, line = -3, outer = TRUE)
legend("bottom", legend = c("True population","Least-squares estimate"), 
       col = c("blue","red"),
       lty = c(1,2),
       lwd = c(2),
       xpd = TRUE, horiz = FALSE, inset = c(0, -0.5), bty = "n")
