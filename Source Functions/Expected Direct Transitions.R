direct.transitions <- function(Q, D, A, t){
  
  P <- P.matrix(Q)
  n <- nrow(Q)
  S <- 1:n
  unit <- rep(1, n)
  
  I <- diag(1, nrow=n, ncol=n)
  
  Lambda <- diag(diag(-Q), nrow=n, ncol=n)
  
  B <- matrix(0, nrow=n, ncol=n)
  M <- matrix(0, nrow=n, ncol=n)
  G <- matrix(0, nrow=n, ncol=n)
  
  for(i in 1:n){
    if(! i %in% A){
      for(j in 1:n){
        
        
        if(D[i,j]==1){
          
          if(j %in% A){
            G[i,j] <- P[i,j]
          }else{
            M[i,j] <- P[i,j]
          }
        }else{
          
          if(!j %in% A){
            B[i,j] <- P[i,j]
          }
        }
      }
    }
  }
  
  A.bar <- cbind(-Lambda%*%(I-B-M), I)
  zero.matrix <- matrix(0, nrow=n, ncol=n)
  A.bar <- rbind(A.bar, cbind(zero.matrix, zero.matrix))

  Integral <- cbind(I, zero.matrix)%*% expm(A.bar*t) %*%rbind(zero.matrix, I)
  
  expected.transitions <- Integral%*%Lambda%*%(G+M)%*%unit
  
  return(expected.transitions)
  
}