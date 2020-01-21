### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Created on October 21, 2015 at 12:52
# Marc-Olivier Beausoleil 
# Change nemeric vectors to factors or factors to characters 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
as.numeric.character2 <- function(x) {as.numeric(as.character(x))}

change.factor.to.character <- function(data) {
  fact.col = which(sapply(data, class)=="factor")
  data[,fact.col] <- lapply(data[,fact.col], as.character)
  return(data)
}


change.factor.to.numbers <- function(data) {
  fact.col = which(sapply(data, class)=="factor")
  data[,fact.col] <- lapply(data[,fact.col], as.character)
  data[,fact.col] <- lapply(data[,fact.col], as.numeric)
  return(data)
}


# For multiple columns 
factor2numeric <- function(data,columns.names.vector) {
  data[,cols] = apply(data[,cols], 2, function(x) as.numeric(as.character(x)))  
}

