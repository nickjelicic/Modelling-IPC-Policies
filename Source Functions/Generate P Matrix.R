P.matrix <- function(Q){
  
  n <- nrow(Q)
  P <- matrix(0, nrow=n, ncol=n)
  
  for(i in 1:n){
    
    if(Q[i,i]==0){
      P[i,i] <- 1
    }else{
      
      for(j in 1:n){
        P[i,j] <- -Q[i,j]/Q[i,i]
      }
      P[i,i] <- 0
    }
    
  }
  
  return(P)
}