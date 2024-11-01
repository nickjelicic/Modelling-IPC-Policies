expected.transitions.derivative <- function(initial.distribution, initial.distribution.derivative, Q, Q.diff, D, A, t){
  
  
  if(t==0){
    return(0)
  }else{
    n <- nrow(Q)
    
    I <- diag(1, nrow=n)
    zero.matrix <- matrix(0, nrow=n, ncol=n)
    zero.matrix.3n <- matrix(0, nrow=n, ncol=3*n)
    unit <- rep(1, n)
    
    
    
    Lambda <- diag(diag(-Q), nrow=n, ncol=n)
    Lambda.diff <- diag(diag(-Q.diff), nrow=n, ncol=n)
    
    B <- matrix(0, nrow=n, ncol=n)
    M <- matrix(0, nrow=n, ncol=n)
    G <- matrix(0, nrow=n, ncol=n)
    G.diff <- matrix(0, nrow=n, ncol=n)
    M.diff <- matrix(0, nrow=n, ncol=n)
    B.diff <- matrix(0, nrow=n, ncol=n)
    P <- P.matrix(Q)
    
    for(i in 1:n){
      if(! i %in% A){
        for(j in 1:n){
          
          
          if(D[i,j]==1){
            
            if(j %in% A){
              G[i,j] <- P[i,j]
              if(!j==i & !Q[j, j]==0){
                G.diff[i,j] <- (Q.diff[i,i]*Q[i,j] - Q.diff[i,j]*Q[i,i])/(Q[i,i]^2)
              }
            }else{
              M[i,j] <- P[i,j]
              if(!j==i & !Q[j, j]==0){
                M.diff[i,j] <- (Q.diff[i,i]*Q[i,j] - Q.diff[i,j]*Q[i,i])/(Q[i,i]^2)
              }
            }
          }else{
            
            if(!j %in% A){
              B[i,j] <- P[i,j]
              if(!j==i & !Q[j, j]==0){
                B.diff[i,j] <- (Q.diff[i,i]*Q[i,j] - Q.diff[i,j]*Q[i,i])/(Q[i,i]^2)
              }
            }
          }
        }
      }
    }
    
    Theta <- rbind( cbind(-Lambda%*%(I-B-M), Lambda%*%(G+M)), cbind(zero.matrix, zero.matrix))
    
    Theta.diff <- rbind(cbind(-Lambda.diff%*%(I-B-M) + Lambda%*%(B.diff + M.diff), Lambda.diff%*%(G+M) + Lambda%*%(G.diff + M.diff)),
                        cbind(zero.matrix, zero.matrix)
                        
    )
    Theta.large <- rbind( cbind(Theta, Theta.diff), cbind(matrix(0, nrow=2*n, ncol=2*n), Theta))
    
    derivative.1st.term <- initial.distribution %*% cbind(I, zero.matrix.3n) %*% expm(Theta.large*t)%*%rbind(matrix(0, nrow=3*n, ncol=n), I)%*%unit
    
    if(any(initial.distribution.derivative!=0)){
      derivative.2nd.term <- initial.distribution.derivative%*%cbind(I, zero.matrix)%*%expm(Theta*t)%*%rbind(zero.matrix, I)%*%unit
    }else{
      derivative.2nd.term <- 0
    }
    
    
    return(derivative.1st.term + derivative.2nd.term)
  }

}