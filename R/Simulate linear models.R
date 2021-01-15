#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# November 2, 2020 at 22:06
# Why: Different data structure and its effect on linear models 
# Output: 
# Requires: 
# NOTES: 
#### ### ### ## #### ### ### ## #### ### ### ## 


# Reproducible example ----------------------------------------------------
set.seed(12349)

# Setting the simulation --------------------------------------------------
x = 1:20
n = length(x)
y = 10 + 2*x
y.err = 10 + 2*x + rnorm(n, mean = 0, sd = 3)
y.LARGE.err = 10 + 2*x + rnorm(n, mean = 0, sd = 15)
y.err.not.linear = 10 + 2*x + 1.2*x^2 + rnorm(n, mean = 0, sd = 3)
y.err = 10 + 2*x + rnorm(n, mean = 0, sd = 3)


# Making the function -----------------------------------------------------
lm.test <- function(x,y, title, ylim = NULL) {
  lm.res1 = lm(y~x)
  sum.mod = summary(lm.res1)
  plot(y~x, pch =20, cex = 3, main=title)
  abline(lm.res1)
  if (!is.null(ylim)) {
  plot(residuals(lm.res1), pch =20, cex = 3, main=title, ylim = ylim)
  } else {plot(residuals(lm.res1), pch =20, cex = 3, main=title)}
  abline(h = 0, lty = 2)
  return(list(model = lm.res1, sum.mod = sum.mod))
}

# Showing the simulated data ----------------------------------------------
par(mfrow=c(5,2))
out1 = lm.test(x,y,"Perfect sampling", ylim = c(-2,2))
out1$sum.mod

out2 = lm.test(x,y.err,"Imperfect sampling")
out2$sum.mod

out3 = lm.test(x,y.LARGE.err,"VERY imperfect sampling")
out3$sum.mod

out4 = lm.test(x,y.err.not.linear,"Not linear")
out4$sum.mod


# Show the quadratic fit --------------------------------------------------
lm.res4 = lm(y.err.not.linear~x + I(x^2))
sum.mod = summary(lm.res4)
plot(y.err.not.linear~x, pch =20, cex = 3, main="Quadratic fit")

newdat = data.frame(x = seq(min(x), max(x), length.out = 100))
newdat$pred = predict(lm.res4, newdata = newdat)
with(newdat, lines(x = x, y = pred))

plot(residuals(lm.res4), pch =20, cex = 3, main="Quadratic fit")
abline(h = 0, lty = 2)

mean(residuals(lm.res4))
hist(residuals(lm.res4))
plot(lm.res4)

shapiro.test(residuals(lm.res4))
shapiro.test(y.err)

par(mfrow=c(1,1))
boxplot(chickwts$weight~chickwts$feed)
res.aov = aov(chickwts$weight~chickwts$feed)
summary(res.aov)
tout = TukeyHSD(res.aov)
tout$`chickwts$feed`[tout$`chickwts$feed`[,4]<0.05,]

x2= 1:100
a=0
b = 1
sigma2 = x2^1.3
eps = rnorm(x2,mean=0,sd=sqrt(sigma2))
y=a+b*x2 + eps
mod <- lm(y ~ x2)
plot(x2, y, pch = 20)
plot(residuals(mod), pch =20);abline(h=0,lty =3)





shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))

x = rnorm(1000,mean = 0,sd = 1)
y = rnorm(1000,50)

x = rnorm(1000,mean = 0,sd = 1)
t.test(x = x, alternative = "greater")
t.test(x = x)
hist(x)
t.test(x = y)


t.test(x = x, y = y)

hist(c(x,y), breaks = 50)







boxplot(weight ~ feed, data = chickwts, col = "lightgray",
        # varwidth = TRUE, notch = TRUE, 
        main = "chickwt data",
        ylab = "Weight at six weeks (gm)")
lm.out = lm(weight ~ feed, data = chickwts)
aov.out = aov(weight ~ feed, data = chickwts)
summary(lm.out)
summary(aov.out)

anova(fm1 <- lm(weight ~ feed, data = chickwts))
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)

par(opar)




dput(unique(chickwts$feed))


head(chickwts)
hsc = chickwts[chickwts$feed %in% c("horsebean", "soybean"),]
hc = chickwts[chickwts$feed %in% c("horsebean"),]
sc = chickwts[chickwts$feed %in% c("soybean"),]
t.test(x = hc$weight,y = sc$weight)
a.out = aov(hsc$weight~hsc$feed)
anova(a.out)
summary(a.out)

plot(chickwts$weight ~ chickwts$feed)

sum.weigh = aggregate(chickwts$weight, by=list(Category=chickwts$feed), FUN=sum)
barplot(sum.weigh$x,legend.text = sum.weigh$Category, names.arg = sum.weigh$Category, col = as.factor(sum.weigh$Category))
