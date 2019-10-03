### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Created on October 17, 2016 at 14:31
# Marc-Olivier Beausoleil 
# Draws binomial distributions 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# Binomial distribution (Bernoulli)  --------------------------------------
par(mfrow = c(4,5))
for(i in seq(0,1,length.out = 20)){
  plot(dbinom(x = 1,size = 1:100,prob = i), type = "l", 
       ylab = "Binom", xlab = "Index", main = paste("p",round(i,2)),
       ylim= c(0,1))
}

par(mfrow = c(4,5))
for(i in seq(0,1,length.out = 20)){
  plot(dbinom(x = 1,size = 1,prob = i), type = "p", 
       ylab = "Binom", xlab = "Index", main = paste("p",round(i,2)),
       ylim= c(0,1))
}
