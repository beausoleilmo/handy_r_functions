#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# November 4, 2020 at 12:59
# Why: Look at 95% confidence interval 
# Output: 
# Requires: 
# NOTES: I don't know why red confidence limit is ALWAYS on the left of 0... 
#### ### ### ## #### ### ### ## #### ### ### ## 

# Make the simulation repeatable
# set.seed(1234567890)

# True mean of the population 
mean.mu = 0

# population
x = rnorm(n = 100000, mean = mean.mu, sd = 1)

# Simulate X number of samples 
perm = 100

# Number of samples to be taken from the population 
sample.size = 50000

for (k in 1:10) {
  mean.temp = numeric(0)
  sd.temp = numeric(0)
  se.temp = numeric(0)
  ci.all = numeric(0)
  
  for (i in 1:perm) {
    sample.pop = sample(x = x,
                        size = sample.size,
                        replace = FALSE)
    mean.samp.i = mean(sample.pop)
    sd.samp.i = sd(sample.pop)
    mean.temp = c(mean.temp, mean.samp.i)
    sd.temp = c(sd.temp, sd.samp.i)
    se.temp = c(se.temp, sd.samp.i/sqrt(sample.size))
  } # end for {i} 
  
  ci.df = data.frame(mean = mean.temp,
                     sd = sd.temp,
                     se = se.temp)
  ci.df$ci.up = ci.df$mean + qnorm(0.95)*ci.df$se
  ci.df$ci.dw = ci.df$mean - qnorm(0.95)*ci.df$se
  
  # If the range contains the population mean, make it black. If not, red  
  ci.df$col <- ifelse(ci.df$ci.dw < mean.mu & ci.df$ci.up > mean.mu, "black","red")
  ci.df$overlap0 <- ifelse(ci.df$ci.dw < mean.mu & ci.df$ci.up > mean.mu, TRUE,FALSE)
  
  plot(NA, 
       main = "95% confidence interval for all permutations",
       ylab = "Permutation number",
       xlab = "Confidence limit",
       ylim = c(0,nrow(ci.df)), 
       xlim = c(min(ci.df$ci.dw), 
                max(ci.df$ci.up)))
  for (j in 1:nrow(ci.df)) {
    segments(x0 = ci.df$ci.dw[j], 
             x1 = ci.df$ci.up[j],
             y0 = j,
             y1 = j, 
             col = ci.df$col[j])
  } # end for {j}
  
  abline(v = mean.mu, 
         lty = 2)
  
  ci.int = table(ci.df$overlap0)/perm
  ci.all = c(ci.all, ci.int)
} # end for {k} 

mean(ci.all[(1:length(ci.all) %% 2) == 0])
