### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Created on November 12, 2015 at 10:06
# Marc-Olivier Beausoleil 
# Function to clean the memory 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# rm(list=ls(all=TRUE))

clean.mem <- function () {
  rm(list = ls(all.names = TRUE, envir = globalenv()),
     envir = globalenv())
  return(invisible(NULL))
}
