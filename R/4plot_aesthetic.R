### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Created on  November 16, 2018 at 21:53
# Marc-Olivier Beausoleil 
# Shows 4 different types of aesthetics in ggplot2 and arrange plots in a grid
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

library(ggplot2)
library(gridExtra)

g1 = ggplot(iris,
            aes(Sepal.Length,
                Sepal.Width,
                color = Species)) +
  geom_point() +
  xlab("Sepal Length") +
  ylab("Sepal Width") +    
  ggtitle("Colour") +
  theme( legend.position = "none") + 
  theme_bw()

g2 = ggplot(iris,
            aes(Sepal.Length,
                Sepal.Width,
                alpha = Petal.Length)) +
  geom_point() +
  xlab("Sepal Length") +
  ylab("Sepal Width") +    
  ggtitle("Alpha") +
  theme( legend.position = "none") + 
  theme_bw()

g3 = ggplot(iris,
            aes(Sepal.Length,
                Sepal.Width,
                shape = Species)) +
  geom_point() +
  xlab("Sepal Length") +
  ylab("Sepal Width") +    
  ggtitle("Shapes") +
  theme( legend.position = "none") + 
  theme_bw()

g4 = ggplot(CO2,
            aes(x = conc,
                y = uptake,
                colour = Treatment)) +
  geom_point() +
  geom_line(aes(group = Plant)) +
  xlab(expression(paste(CO[2], " Concentration (mL/L)"))) +
  ylab(expression(paste(CO[2], " Uptake (", mu, mol/m^2, " sec)"))) +    
  ggtitle("Grouping lines") +
  geom_line(aes(group = Plant)) +
  theme( legend.position = "none") + 
  theme_bw()

grid.arrange(g1, g2, g3, g4, nrow = 2)
