### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Created on February 17, 2019 at 19:05
# Marc-Olivier Beausoleil 
# function to remove the duplicated value as well as the first value that is not said to be duplicated  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

remove.all.duplicates <- function(df,column) {
  tmp =df[! df[,column] %in% unique(df[duplicated(df[,column]), column]), ]
  return(tmp)
}
