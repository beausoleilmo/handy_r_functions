### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Created on Sunday, March 19, 2017 at 23:09
# Marc-Olivier Beausoleil 
# Makes plots with black background (base R) 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# Create the base plotting window
# type = "n" does not plot the points
# Set the background color to "yellow"
# background = "black"
# background = NA # This one will remove the background
background = "#222222" # Default colour background in theme "Modern type"
par(bg = background, mfrow=c(1,1))

black.friendly.color.palette = c(
  "#0092CC", # Sky blue
  "#FF3333", # Hyper red
  "#779933", # Green
  "#DCD427", # Sulphur Yellow
  "#FF6600", # Orange
  "#FFF2AF", # Beige
  "#7CB490", # Green hospital
  "#969696", # Gray 
  "#323232", # Light gray
  "#F0F0F0"#, # White
                                )
# A Silly Axis Example

Colour.text = black.friendly.color.palette[10]

col.points1 = black.friendly.color.palette[1]
col.points2 = black.friendly.color.palette[2]
col.points3 = black.friendly.color.palette[3]
col.points4 = black.friendly.color.palette[4]
col.points5 = black.friendly.color.palette[5]
col.points6 = black.friendly.color.palette[6]
col.points7 = black.friendly.color.palette[7]
col.points8 = black.friendly.color.palette[8]
col.points9 = black.friendly.color.palette[9]

cex = 1.5

# specify the data 
x <- c(1:10); y <- x; z <- 10/x
# create extra margin room on the right for an axis 
par(mar=c(5, 4, 4, 8) + 0.1)

# plot x vs. y 
plot(x, y,type="b", 
     pch=21, 
     bg = col.points1, 
     col=col.points1, 
     yaxt="n",xaxt="n", 
     lty=3, 
     cex = cex,
     xlab="", ylab="", 
     frame.plot = FALSE)
box(which = "plot", lty = "solid", col = Colour.text)

# add x vs. 1/x 
lines(x, z, type="b", pch=22, col=col.points2, lty=2, bg = col.points2, cex = cex)

lines(x, y+sin(x-1), 
      type = "b", 
      pch = 23, 
      col = col.points3, 
      bg = col.points3, 
      lty = 3)
lines(x, y+sin(x+4), 
      type = "b", 
      pch = 23, 
      col = col.points4, 
      bg = col.points4, 
      lty = 3)
lines(x, y+sin(x+5), 
      type = "b", 
      pch = 24, 
      col = col.points5, 
      bg = col.points5, 
      lty = 3)

axis(1, at=x,labels=x, 
     col = Colour.text,
     col.axis=Colour.text,
     col.ticks = Colour.text,
     las=1)

# draw an axis on the left 
axis(2, at=x,labels=x, 
     col = Colour.text,
     col.axis=Colour.text,
     col.ticks = Colour.text,
     las=2)

# draw an axis on the right, with smaller text and ticks 
axis(4, at=z,labels=round(z,digits=2),
     col = Colour.text,
     col.axis=Colour.text, 
     col.ticks = Colour.text,
     las=2, cex.axis=0.7, tck=-.01)

# add a title for the right axis 
mtext("y=1/x", side=4, line=3, cex.lab=1,las=2, 
      col=Colour.text)

# add a main title and bottom and left axis labels 
title(main = "An Example of Black Background", 
      # sub = "test",
      xlab="X values",
      ylab="Y=X", 
      col.main = Colour.text, 
      col.lab = Colour.text,
      col.sub = "#969696")

