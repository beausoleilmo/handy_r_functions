### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Created on Friday, January 19, 2018 at 12:40
# Marc-Olivier Beausoleil 
# This script will make a grid on top of points that are positioned on a map and 
# calculate the number of points within that grid 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# I have a large dataset of individuals that are in an area. But I want to change the sampling design by separating my field or space with a predefined grid. Here is the dataset:
set.seed(1456)
n = 100
x = rnorm(n)
df = data.frame(x = x, 
                sp = sample(letters[1:5], 
                            size = 100,
                            replace = TRUE),
                stringsAsFactors = TRUE)

plot(df$x,
     ylab = "Points position",
     pch =21, 
     bg = df$sp,
     col = df$sp, 
     cex = .4)

# This will create a grid of the area that I'm studying  
xytransect <- expand.grid(seq(0, n, 5), seq(min(x), max(x), .6))


# This is just showing the actual grid on the area. 
abline(v = seq(0, n, 5), h = seq(min(x), max(x), .6))

# This is to show the "nodes" of the grid
points(xytransect, cex= 0.3, pch = 21, 
       bg  = "pink", 
       col = "pink")

# The idea in this is to group the species and see how many are present within a square of the grid. 

# I was able to group the species (here letters) based on their name on the whole area. But how can I group them on the grid that I created? 
library(dplyr)
df %>%  
  group_by(sp) %>% 
  summarise(n()) 

# Would it be possible to get the center of each square and colour the square by the amount of species (letters) it had inside? 

ibins <- seq(0, nrow(df)+5, 5)
jbins <- seq(min(df$x)-0.6, max(df$x)+0.6, .6)
xytransect <- expand.grid(seq(0, n, 5), seq(min(x), max(x), .6))

out <- df %>% 
  mutate(i = min(ibins) + 5*(cut(row_number(), breaks= ibins,labels=FALSE)-1),
         j = min(jbins) + 0.6*(cut(x,breaks=jbins,labels=FALSE)-1)) %>%
  group_by(i,j) %>% 
  summarise(count=n()) %>%
  ungroup() %>%
  mutate(i_center = i+2.5,
         j_center = j+0.3)


points(out$i_center, out$j_center, cex = out$count/max(out$count), pch = 21, col ="orange", bg = "orange")
# abline(v = seq(0, n, 5), 
#        h = seq(min(x), max(x), .6))
