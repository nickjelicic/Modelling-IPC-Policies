random.LH.sample <- function(integer.limits, continuous.limits, N){
  
  integer.limits[, 2] <- integer.limits[, 2] + 0.999
  
  n.int <- nrow(integer.limits)
  Hypercube.int <- NULL
  for(i in 1:n.int){
    Hypercube.int <- rbind(Hypercube.int, 
                           seq(integer.limits[i, 1], integer.limits[i, 2], length=N))
  }
  

  n.cts <- nrow(continuous.limits)
  Hypercube.cts <- NULL
  for(i in 1:n.cts){
    Hypercube.cts <- rbind(Hypercube.cts, 
                           seq(continuous.limits[i, 1], continuous.limits[i, 2], length=N))
  }
  
  input.parameters <- matrix(NA, nrow=N, ncol=(n.int+n.cts))
  
  for(i in 1:n.int){
    samp <- sample(1:N, N)
    cts.inputs <- Hypercube.int[i, samp]
    input.parameters[, i] <- floor(cts.inputs)
  }
  for(i in 1:n.cts){
    sample <- sample(1:N, N)
    input.parameters[, n.int+i] <- Hypercube.cts[i, sample]
  }
  return(input.parameters)
}
