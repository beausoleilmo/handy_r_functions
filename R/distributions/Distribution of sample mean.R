#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# Sunday, April 5, 2020 at 11:37
# Why: 
# Output: 
# Requires: 
# NOTES: 
#### ### ### ## #### ### ### ## #### ### ### ## 


# Generate normal distribution from binomial ------------------------------
par(mfrow=c(1,1))
# Generate bimodal distribution 
n1 = 1000
n2 = 1000
x1 = abs(rnorm(n1, mean = 10))
x2 = abs(rnorm(n2, mean = 15))

par(mfrow=c(2,1))
hist(c(x1,x2), 
     main = "Distribution of population", 
     xlab = "Phenotype")

# Sampling a bimodal distribution and get its mean will generate a normal distribution
mn = NULL
for (i in 1:1000) {
  tmp = mean(sample(c(x1,x2),size = 100))
  mn = c(mn,tmp)
}
hist(mn, breaks = 100, main = "Distribution of sample mean")



# Normal vs t-distribution 1 DF -------------------------------------------
# Setting the graphical parameter to show 1 graph only 
par(mfrow=c(1,1))

# Sequence of number to draw the distributions
sequ = seq(-4,4, length.out = 100)

# Normal distribution 
plot(sequ,y = dnorm(sequ), 
     type = "l", 
     ylim = c(0,1), 
     main = "Normal and t-distributions", 
     ylab = "Density")
# Area of shade (from https://www.r-bloggers.com/2012/06/shading-regions-of-the-normal-the-stanine-scale/)
alpha = 0.10
from.z.low <- -100
to.z.low <- qnorm(alpha/2)
from.z.up <- qnorm(1-(alpha/2))
to.z.up <- 100

s.x.low  <- c(from.z.low, seq(from.z.low, to.z.low, 0.01), to.z.low)
s.y.low  <- c(0, dnorm(seq(from.z.low, to.z.low, 0.01)), 0)
polygon(x = s.x.low, y = s.y.low, col="red")

s.x.up  <- c(from.z.up, seq(from.z.up, to.z.up, 0.01), to.z.up)
s.y.up  <- c(0, dnorm(seq(from.z.up, to.z.up, 0.01)), 0)
polygon(x = s.x.up, y = s.y.up, col="red")

# T-distribution 
df = 1 # Setting the number of degrees of freedom for the t-distribution
lines(x = sequ,y = dt(sequ, df), type = "l", col = "red")

# Adding a legend to the plot 
legend("topright",legend = c("Normal","t-distribution"),col = c(1,2), lty =1)

