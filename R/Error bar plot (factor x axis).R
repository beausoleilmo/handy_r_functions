### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Created on Monday, June 6, 2016 at 14:42
# Marc-Olivier Beausoleil 
# Draw error bars in a plot if X is a factor 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

plot(NA,
     type = "p",
     ylim = c(0,4),
     xlim = c(0,3),
     xaxt="n",
     main = "yo",
     xlab = "Site Category",
     ylab = "Mean")
axis(side = 1, at = c(1,2), labels = levels(c("yo","man")), cex.lab =1, cex.axis =1)
x = as.numeric(c(1,2))
y = c(2,3)
mean = mean(y)

error.bar <- function(x,
                      y,
                      epsilon = NULL, 
                      se = NULL, 
                      se.mul = 1,
                      col = "black") {
  x = as.numeric(x)
  if(is.null(se)){
    stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
    se = se.mul*stderr(y)
    } else {se = se.mul*se}
  
  segments(x, y-se,x, y+se,col = col)
  if(is.null(epsilon)){
    epsilon = 0.02} else {epsilon = epsilon}
  segments(x-epsilon,y-se,x+epsilon,y-se,col = col)
  segments(x-epsilon,y+se,x+epsilon,y+se,col = col)
  
}

