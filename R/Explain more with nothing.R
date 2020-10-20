par(mfrow=c(1,1))
a = 0 # intercept
b = 1 # slope 
mu.error = 0
sd.error = 4
# Change the sample size to see the effect on the linear regression 
sample.size = 50

seq.x.from = 0
seq.x.to = 10
x = seq(seq.x.from,seq.x.to, length.out = 100)
y = a + b*x + rnorm(length(x),mu.error,sd.error)

my.lin.dat= cbind(x,y)
pts= my.lin.dat[sample(nrow(my.lin.dat), size = sample.size),]

plot(pts, pch = 20,  
     xaxt='n',yaxt='n',
     xlab='',ylab='',
     xlim = c(seq.x.from,seq.x.to), ylim = c(-5,seq.x.to*(b*1.1)))
abline(a,b, col = "blue",lwd = 3)
lm.out.sample = lm(pts[,"y"]~pts[,"x"])
abline(lm.out.sample, col = "red", lty = 2, lwd = 3)
lm.mod.sum.1x = summary(lm.out.sample)
lm.pop.modrsq = lm.mod.sum.1x$r.squared
lm.pop.mod.adjrsq = lm.mod.sum.1x$adj.r.squared






# Make nothing (noise)
# Make a empty number vector to store information in it 
max.additional.x = 31
simmulation.rsq = numeric(0)
simmulation.adj.rsq = numeric(0)
# make replicates 
# Add the number of "extra" X variables 
nbcol = k
nrow(pts)
nbreplicates.simulations = 25

for (m in 1:nbreplicates.simulations) {
  # Make a empty number vector to store information in it 
  rsq = numeric(0)
  adj.rsq = numeric(0)
  for (k in 2:max.additional.x) {
    nothing = matrix(rnorm(nrow(pts)*nbcol), nrow = nrow(pts))
    all.matrix = cbind(pts[,c("y","x")],nothing)
    colnames(all.matrix) <- c("y","x0",paste0("x",1:ncol(nothing)))
    lm.out.sample.nothing = lm(y~., data = as.data.frame(all.matrix)[,1:k])
    # summary(lm.out.sample)
    # summary(lm.out.sample.nothing)  
    mod.details.nothing = summary(lm.out.sample.nothing)
    rsq = c(rsq,mod.details.nothing$r.squared)
    adj.rsq = c(adj.rsq,mod.details.nothing$adj.r.squared)
  }
  simmulation.rsq = c(simmulation.rsq, rsq)
  simmulation.adj.rsq = c(simmulation.adj.rsq, adj.rsq)
}
# This should be equal
nbreplicates.simulations*max.additional.x == length(simmulation.rsq)

rsq.all = simmulation.rsq
length(rsq.all)
additional.x.all = rep(1:c(max.additional.x-1),nbreplicates.simulations)
length(additional.x.all)
all.sim = data.frame(cbind(rsq.all, additional.x.all))
all.sim = rbind(all.sim,
                c(lm.pop.modrsq,0))

all.sim.adj = data.frame(cbind(simmulation.adj.rsq, additional.x.all))
all.sim.adj = rbind(all.sim.adj,
                    c(lm.pop.mod.adjrsq,0))
all.sim.adj[all.sim.adj$additional.x.all==0,]

par(mfrow = c(1,2))
plot(all.sim$rsq.all~all.sim$additional.x.all, pch = 20, ylim = c(0,1), 
     col =ifelse(all.sim$additional.x.all == 0,"red","black"),
     cex =ifelse(all.sim$additional.x.all == 0,2,1),
     main = "Explain ++ w/ ø or random \ninfo (R-squared)",
     ylab = "Multiple R squared",
     xlab = "Additional random information")
smooth.fit.all.simul = loess(all.sim$rsq.all~all.sim$additional.x.all, span = 3)
lines(predict(smooth.fit.all.simul), col = "red", lwd = 2)

plot(all.sim.adj$simmulation.adj.rsq~all.sim.adj$additional.x.all, pch = 20, ylim = c(0,1), 
     col =ifelse(all.sim.adj$additional.x.all == 0,"red","black"),
     cex =ifelse(all.sim.adj$additional.x.all == 0,2,1),
     main = "Explain ++ w/ ø or random \ninfo (adjusted R-squared)",
     ylab = "Multiple R squared",
     xlab = "Additional random information")
smooth.fit.adj.all = loess(all.sim.adj$simmulation.adj.rsq~all.sim.adj$additional.x.all, span = 3)
lines(predict(smooth.fit.adj.all), col = "red", lwd = 2)




# Plot only one example 
# par(mfrow = c(1,2))
# plot(rsq~c(1:c(max.additional.x-1)), pch = 20, ylim = c(0,1),
#      main = "Explain more with nothing \nor random information",
#      ylab = "Multiple R squared",
#      xlab = "Additional random information")
# smooth.fit = loess(rsq~c(1:c(max.additional.x-1)), span = 3)
# lines(rsq)
# lines(predict(smooth.fit), col = "red", lwd = 2)
# 
# plot(adj.rsq~c(1:c(max.additional.x-1)), pch = 20, ylim = c(0,1),
#      main = "Explain more with nothing \nor random information",
#      ylab = "Multiple R squared",
#      xlab = "Additional random information")
# smooth.fit.adj = loess(adj.rsq~c(1:c(max.additional.x-1)), span = 3)
# lines(adj.rsq)
# lines(predict(smooth.fit.adj), col = "red", lwd = 2)
# 
