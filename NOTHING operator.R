### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Created on Tuesday, June 21, 2016 at 17:19
# Marc-Olivier Beausoleil 
# And nothing else matter
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

`%notin%` = function(x,y) !(x %in% y)
'%!in%' <- function(x,y)!('%in%'(x,y)) #--  x without y
"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
