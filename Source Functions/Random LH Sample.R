random.LH.sample <- function(Integer.limits, continuous.limits, N){
  
  n.int <- nrow(Integer.limits)
  Integer.limits[, 2] <- Integer.limits[, 2] + 1
  
  Inputs <- NULL
  if(length(n.int)>0){
    for(i in 1:n.int){
      values <- seq(Integer.limits[i, 1], Integer.limits[i, 2]-1e-6, length=N)
      Inputs <- cbind(Inputs, floor(values[sample(1:N, N)]))
    }
  }
  
  
  n.cts <- nrow(continuous.limits)
  if(length(n.cts)>0){
    for(i in 1:n.cts){
      values <- seq(continuous.limits[i, 1], continuous.limits[i, 2], length=N)
      Inputs <- cbind(Inputs, values[sample(1:N, N)])
    }
  }
 
  
  return(Inputs)
}