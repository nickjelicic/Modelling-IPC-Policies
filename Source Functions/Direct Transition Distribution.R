direct.transition.distribution <- function(Q, D, A, t, k){
  
  P <- P.matrix(Q)
  n <- nrow(Q)
  S <- 1:n
  unit <- rep(1, n)
  
  I <- diag(1, nrow=n, ncol=n)
  zero.matrix <- matrix(0, nrow=n, ncol=n)
  
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
  
  if(k == 0){
    transition.distribution <- unit
  }else{
    
    Mat <- rbind(cbind(-Lambda%*%(I-B), Lambda%*%(G+M)), cbind(zero.matrix, zero.matrix))
    
    if(k>1){
      
      for(i in 2:k){
        
        zero.matrix.k.1 <- matrix(0, nrow=n, ncol=ncol(Mat)-n)
        zero.matrix.k.2 <- matrix(0, nrow=ncol(Mat), ncol=n)
        
        
        Mat <- rbind(cbind(-Lambda%*%(I-B), Lambda%*%M, zero.matrix.k.1), 
                     cbind(zero.matrix.k.2 , Mat))
      }
    }
    
    zero.matrix.k <- matrix(0, nrow=n, ncol=k*n)
    transition.distribution <- cbind(I, zero.matrix.k)%*%expm(Mat*t)%*%rbind(t(zero.matrix.k), I)%*%unit
  }
  
  
  return(transition.distribution)
  
}