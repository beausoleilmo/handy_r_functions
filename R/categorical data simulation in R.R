#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# October 21, 2020 at 16:08
# Why: Simulate categorical data for testing purpose (test linear model with categorical data)
# Output: 
# Requires: 
# NOTES: 
#### ### ### ## #### ### ### ## #### ### ### ## 

set.seed(123)
# Simulation including all variables
n <- 1000
X <- t(rmultinom(n = n, size = 1, prob = rep(0.2,4)))
littlex = 1:nrow(X)
beta <- c(1,2,3,4)
e <- rnorm(n, 0, 0.25)
y <- 3*littlex+X%*%beta + e + 6
# Model fit
mod <- lm(y~X[,-1])
# Estimates
mod$coefficients
summary(mod)
plot(y~X[,4])
