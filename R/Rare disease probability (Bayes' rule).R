#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# November 3, 2020 at 10:41
# Why: an example showing colouring and shading showing the probability of having a disease from a positive test (bayesian rule)
# Output: 
# Requires: 
# NOTES: Inspired from a course named "Learning Statistics with R" from "The Great Courses"
#### ### ### ## #### ### ### ## #### ### ### ## 


# Prevalence of a disease in a population 
preval = 1/100
accuracy.of.test = 0.85

par(mar=c(2, 2, 4.1, 12), xpd=TRUE)
# Show graphically what is that proportion 
plot(c(0, 1), c(0, 1), type= "n", xlab = "", ylab = "", 
     asp=1, 
     axes=F,
     main = paste("Population prevalence of",preval, "disease in population\n(Area is proportional to the population)"))

# Show the population as a large square 1X1 
rect(0, 0, 1, 1, density = 30, col = "blue", angle = -30, border = "transparent")

# Show the proportion of people having the disease 
rect(0, 0, 0 + sqrt(preval), 0 + sqrt(preval), #density = 160, 
     col = "black", angle = 30, border = "transparent")

# Show the proportion of people that have the disease, that are tested and found a positive result 
rect(0, 0, 0+ sqrt(preval*accuracy.of.test), 0+ sqrt(preval*accuracy.of.test), #density = 160, 
     col = "red", angle = 30, border = "transparent")

# Add legend to distinguish the areas 
legend("right", 
       bg = NA, bty = "n",
       # horiz=TRUE,
       # ncol=3,
       # box.col = "white",
       inset = c(-0.35,0),
       legend = c("Population", 
                  paste0("Diseased group (",preval*100,"%)"),
                  paste0("Accuracy of test (",accuracy.of.test*100,"%)")), 
       fill = c("blue","black", "red"),
       density = c(30,NA,NA), 
       angle = c(-30,NA,NA))


# Calculate the posterior probability of having a disease -----------------

# Define the variables  
p.BgivenA = accuracy.of.test
p.BgivenA.c = 1-p.BgivenA
p.A = preval
p.A.c = 1-p.A

# Calcualte groups of probabilities 
p.BgivenA*p.A

p.BgivenA*p.A
p.BgivenA.c*p.A.c

p.BgivenA*p.A + p.BgivenA.c*p.A.c

# Actual calculation of the posterior 
p.AgivenB = p.BgivenA*p.A/(p.BgivenA*p.A+p.BgivenA.c*p.A.c)
p.AgivenB

# Get the percentage 
percent.having.rare.disease = p.AgivenB * 100 

# Conclusion 
cat("You have a",
    round(percent.having.rare.disease,2),"% chance of having the rare disease, provided that the accuracy is",
    accuracy.of.test,"% and that the probability of having the disease in the population is", preval*100,"%")
