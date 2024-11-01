colMin <- function(Data){
  
  mins <- NULL
  for(k in 1:ncol(Data)){
    mins <- c(mins, min(Data[, k]))
  }
  
  return(mins)
}