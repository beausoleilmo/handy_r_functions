par(mfrow=c(1,1))
n1 = 1000
n2 = 1000
x1 = abs(rnorm(n1, mean = 10))
x2 = abs(rnorm(n2, mean = 15))
par(mfrow=c(2,1))
hist(c(x1,x2), main = "Distribution of population", xlab = "Phenotype")
mn = NULL
for (i in 1:1000) {
  tmp = mean(sample(c(x1,x2),size = 100))
  mn = c(mn,tmp)
}
hist(mn, breaks = 100, main = "Distribution of sample mean")

par(mfrow=c(1,1))
# T-distribution 
df = 3
sequ = seq(-4,4, length.out = 100)
plot(dnorm(sequ), type = "l", ylim = c(0,1), main = "Normal and t-distributions", ylab = "Density")
lines(dt(sequ, df), type = "l", col = "red")
legend("topright",legend = c("Normal","t-distribution"),col = c(1,2), lty =1)
