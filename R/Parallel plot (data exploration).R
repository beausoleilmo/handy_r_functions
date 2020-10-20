library(datasets)
data("chickwts")
summary(chickwts)

meat = chickwts[chickwts$feed =="meatmeal",1]
horse = chickwts[chickwts$feed =="horsebean",1]
qqplot(meat, horse, xlim  = c(100,400),ylim  = c(100,400))
abline(a=0,b=1,col = "red")

# parallel plot 
plot(x = c(meat, horse), 
     y = c(rep(0,length(meat)), rep(1, length(horse))),
     pch = "*",
     ylim = c(-1,2), cex = 3, 
     ylab = "",xlab = "Weigth", main = 'Parallel plot')
text(x = 300, y = -0.4, labels = "Meatmeal", cex = 1)
text(x = 160, y = 1.9, labels = "Horsebean", cex = 1)
abline(h=c(0,1))
