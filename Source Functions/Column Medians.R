colMedians <- function(Data){
  
  medians <- NULL
  for(k in 1:ncol(Data)){
    medians <- c(medians, median(Data[, k]))
  }
  
  return(medians)
}