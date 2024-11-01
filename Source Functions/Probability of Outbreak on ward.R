outbreak.probability.ward <- function(Q, A, t, outbreak.states){
  
  n <- nrow(Q)
  
  outbreak.prob <- numeric(n)
  End.states <- end.states(Q, A, t)
  
  for(i in 1:n){
    
    outbreak.prob[i] <- sum(End.states[i, outbreak.states])
  }
  
  return(outbreak.prob)
}