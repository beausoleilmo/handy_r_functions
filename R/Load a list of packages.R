#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# 27 October 2020
# Why: 
# Output: 
# Requires: 
# NOTES: Inspired from a course named "Learning Statistics with R" from "The Great Courses"
#### ### ### ## #### ### ### ## #### ### ### ## 

# Make a list of packages that will need to be installed and called
spatial.pkgs = c("sp","sf","spdep","raster","rasterVis",
                 "maptools","rgeos","geoR","gstat",
                 "UScensus2000cdp","mapproj","ggmap","maps",
                 "RColorBrewer","SpatialEpi")

# Function ----------------------------------------------------------------
call.packages <- function(list.packages) {
  # Install the packages that are not already installed 
  install.packages(list.packages[!(list.packages %in% installed.packages())])
  
  # Call the libraries 
  lapply(X = list.packages, 
         FUN = library, 
         character.only = TRUE) # in order to work, you must set character.only = TRUE
  
  # See if the packages were loaded properly 
  sapply(X = list.packages,
         FUN = require, 
         character.only = TRUE) 
}

call.packages(spatial.pkgs)
