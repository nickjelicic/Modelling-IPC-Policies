end.states <- function(Q, A, t){
  
  Q[A, ] <- 0
  
  return(expm(Q * t))
}