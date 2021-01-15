#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# 03 November 2020
# Why: Show pedagogically how the correlation coefficient = 0 for NONE linear relationships even though there is a perfect relatioships between x and y 
# Output: 
# Requires: 
# NOTES: Inspired from a course named "Learning Statistics with R" from "The Great Courses"
#### ### ### ## #### ### ### ## #### ### ### ## 


# Define the variables 
x = seq(-1,1,length.out = 100)
y = sqrt(1-x^2)
y2 = -sqrt(1-x^2)

# Plot one half of the circle 
plot(y ~ x, 
     asp = 1,
     ylim = c(-1,1), 
     main = "Non linear models",
     pch = 20, 
     cex = 2)
abline(h = 0, v = 0, lty = 3)
# Add the second half of the circle 
points(y2 ~ x )

# Linear model of half of the circle 
lm.out = lm(y ~x)
summ.lm = summary(lm.out)
round(summ.lm$r.squared,2)

# Add the line to the circle 
abline(lm.out, lwd = 3, col = "red")

# Now fit a parabola to a half circle and look at the R^2
poly.out = lm(y ~poly(x,2))
summ.poly = summary(poly.out)
round(summ.poly$r.squared,2)

newdat = data.frame(x = seq(min(x), max(x), length.out = 100))
newdat$pred = predict(poly.out, newdata = newdat)
with(newdat, lines(x = x, y = pred, lwd = 3, col = "blue"))


# Add legend 
legend("bottomright", 
       # bg = NA, bty = "n",
       # horiz=TRUE,
       # ncol=3,
       # box.col = "white",
       # inset = c(-0.35,0),
       legend = c("Linear regression", 
                  "Non-linear regression"), 
       # density = c(30,NA), angle = c(-30,NA),
       lty = 1,
       lwd = 5,
        col = c("red","blue")
       )

