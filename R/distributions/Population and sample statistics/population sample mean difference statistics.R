#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# 13 October 2020
# Why: This script is intended in showing what it means to gather a "sample" from the population
# Output: Cool Graphs
# Requires: See the libraries list
# NOTES: 
#### ### ### ## #### ### ### ## #### ### ### ## 

# Libraries ---------------------------------------------------------------
library(scales)

# Define parameters  ------------------------------------------------------
pop.mean = 0
pop.sd = 1
nb.samples = 100
lwd = 3
nb.rep.samples = 100 # long if more than 1000 samples 
all.x = numeric(0)
all.x.1 = numeric(0)
ci.x = numeric(0)
alpha = 0.05
z.crit = qnorm(1-alpha/2)
CI.level = qnorm(0.975) # qnorm tells us what VALUE we should expect from the normal distribution (z-distribution, meaning centered and scaled)
# 0.975 in qnorm gives the VALUE at which on a normal z-distribution, there is 97.5 % of the data. THEN if we are interested in the 2.5% in the tail, we need to get the value — 97.5% 2.5% — => — 2.5% 95% 2.5% —. Thus BETWEEN the values ~-1.96 and ~1.96, we should have ~ 95% of the data points
qnorm(1-dnorm(qnorm(0.975)))
pnorm(q = qnorm(0.975))

vec = NULL
vec.dens = NULL
vec.mean = NULL
par(mfrow = c(1,1))


# Functions ---------------------------------------------------------------
## Confidence interval -----------------------------------------------------
ci <- function(mean.samp, sd.samp, n.samp, ci.level = CI.level) {
  se = sd.samp /sqrt(n.samp)
  up = mean.samp + ci.level * se
  dw = mean.samp - ci.level * se
  return(list(up,dw))
}
# The CI DOESN'T GUARANTEE that you have the population mean WITHIN the interval you calculated. 
# It means that WITH 95% confidence (if you'd repeat the sampling INFINITELY), the interval calculated contains μ (the population mean)

# Set the random generator seed -------------------------------------------
# In order to get CONSISTENT results 
# set.seed(1234)

# Make replicates of samples from population parameters -------------------
population.distribution = rnorm(nb.samples,pop.mean,pop.sd)
for (i in 1:nb.rep.samples) {
  # x = rnorm(nb.samples,pop.mean,pop.sd)
  # OR 
  x = sample(x = population.distribution, size = nb.samples, replace = TRUE)
  sd.x = sd(x)
  all.x = c(all.x,x)
  d.x = density(x)
  mean.x = mean(x)
  vec = c(vec, x)  
  vec.dens = c(vec.dens, list(d.x))  
  vec.mean = c(vec.mean, mean.x)  
  ci.values = ci(mean.samp = mean.x,sd.samp = sd.x,n.samp = nb.samples)
  sorted.ci = sort(unlist(ci.values))
  ci.x = rbind(ci.x,sorted.ci)
}
  rownames(ci.x) <- paste("samples",1:nb.rep.samples)
pnorm(q = 1.96,mean = 0,sd=1, lower.tail = TRUE,)

# Draw TRUE population distribution (normally distributed) ----------------
# Not that this is "Setting up the graph" but not DRAWING to it. We're going to draw later
z <- seq(from = -5, 
         to = 5, 
         by = .1)
y <- dnorm(z, 
           mean = pop.mean, 
           sd = pop.sd)
max.y = max(unlist(lapply(vec.dens,function(p) max(p$y))))
plot(y~z, 
     ylab = "Density",
     xlab = "X values",
     type = "n", 
     lwd = lwd, 
     ylim = c(-0.05,max(max.y,1)),
     main = bquote("Graph pop. mean " ~ mu ~ " & sample mean " ~ hat(X) ~ " with " ~ .(nb.rep.samples) ~ " indep. draws"))

# Define colours with transparency ----------------------------------------
col.alpha.red = scales::alpha("red", 
                              alpha = .25)
col.alpha.gray = scales::alpha("gray",
                                alpha = .8)
col.alpha.green = scales::alpha("darkgreen",
                                alpha = .8)
col.alpha.black = scales::alpha("black",
                                alpha = .5)
col.alpha.pink = scales::alpha("pink",
                                alpha = .5)

# Draw all the sample means -----------------------------------------------
abline(v = vec.mean, col = col.alpha.gray, lwd = lwd/3)

# Add all the samples density plots ---------------------------------------
lapply(vec.dens, function(denss) lines(denss, 
                                       col = col.alpha.red, 
                                       lwd = lwd))

# Add population distribution ---------------------------------------------
points(z, y, 
       type = "l", 
       lwd = lwd,
       col = "black")
# Add population true mean ------------------------------------------------
abline(v = pop.mean, 
       col = "blue", 
       lwd = lwd)

# Add sample mean of means ------------------------------------------------
abline(v = mean(vec.mean), 
       col = "gold", 
       lwd = lwd)

# Add the distribution of samples means -----------------------------------
# This should be more and more similar to the population mean as we increase the number of independent draws
lines(density(vec.mean), 
      lwd = lwd /1, 
      col = col.alpha.green)

# Add the samples means as points  ------------------------------------------------
points(x = all.x, y = rep(x = 0,length(all.x)), pch=20, col = col.alpha.pink, cex = 2)
points(x = vec.mean, y = rep(x = 0,length(vec.mean)), pch=20, col = col.alpha.black, cex = 2)

# Add boxplot to see the information from the distribution ----------------
boxplot(vec.mean, horizontal = TRUE, add = TRUE,at = -0.05, col = NA, pch =20, boxwex = .1)

# Add the range of simulated means ----------------------------------------
abline(v = range(vec.mean), lty = 3)
# Add legend --------------------------------------------------------------
legend(-5,1.0,
       legend = c("Population distribution", "Population mean (theoretical)",
                  "Samples distributions", "Samples means (calculated)",
                  "Overall samples mean (mean of means)",
                  "Distribution of samples means", 
                  "Range of samples means"),
       col = c("black","blue",
               "red", "gray",
               "gold",
               "darkgreen",
               "black"), 
       bty = "n",
       lty = c(rep(x = 1,6),3), 
       cex = 0.8,
       lwd = lwd *1)


# Add text to the graph  --------------------------------------------------
text(x = 3,y = .7,
     labels = "The units of\n X are in \nStandard \ndeviation \nunits. The line \nin the \nboxplot is \nthe median.",pos = 4)

for (j in 1:nrow(ci.x)) {
# fixed.y = runif(1,0.1,0.2)
fixed.y = seq(from = 0.1,to = 0.4, length.out = nrow(ci.x))
sample.not.representing.pop = which(ci.x[,1] > pop.mean | ci.x[,2] < pop.mean)
segments(x0 = ci.x[j,1], y0 = fixed.y[j],
         x1 = ci.x[j,2], y1 = fixed.y[j], 
         col = ifelse(j %in% sample.not.representing.pop, 
                      yes = "forestgreen",
                      no = "darkorange"))
}

arrows(x0 = c(-qnorm(0.975),qnorm(0.975)), y0 = 0.2,
       x1 = c(-qnorm(0.975),qnorm(0.975)), y1 = 0, 
       col = "black")


verificaiton.ci = length(sample.not.representing.pop)/nb.rep.samples
cat("There are",100* verificaiton.ci,"% of samples that have a CI that DOESN'T overlap with the population mean")
