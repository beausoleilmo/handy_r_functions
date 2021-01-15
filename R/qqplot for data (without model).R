#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# 03 Nomvember 2020
# Why: Get the qqplot for any data 
# Output: 
# Requires: 
# NOTES: Inspired from a course named "Learning Statistics with R" from "The Great Courses"
#### ### ### ## #### ### ### ## #### ### ### ## 


# Get the data 
ir.virg.data.plgth = iris$Petal.Length[iris$Species == "virginica"]

# Get sample size 
n = length(ir.virg.data.plgth)

# Generate probabilities 
ps = seq(from = 0, 
         to = 1,
         length.out = n/2)

# Calculate the quantiles 
q.virg = quantile(ir.virg.data.plgth, 
                  probs = ps)         

# Get the qqplot 
qqnorm(q.virg, pch = 20, cex = 2)

# add qqline for normal distribution (implied)
qqline(q.virg)
