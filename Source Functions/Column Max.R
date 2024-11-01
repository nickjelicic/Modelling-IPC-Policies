colMax <- function(Data){
  
  maxs <- NULL
  for(k in 1:ncol(Data)){
    maxs <- c(maxs, max(Data[, k]))
  }
  
  return(maxs)
}