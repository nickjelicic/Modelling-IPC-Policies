estimate.derivative <- function(Q, parameter, D, gen.matrices, restrictions=FALSE){
  
  n <- nrow(Q)
  P <- P.matrix(Q)
  if(parameter=="Arrival Rate" & restrictions==FALSE){
    D.alpha <- derivative.Q.matrix(gen.matrices, "Arrival Rate") + derivative.Q.matrix(gen.matrices, "Restriction arrival rate")
  }else{
    D.alpha <- derivative.Q.matrix(gen.matrices, parameter)
  }
  
  
  B <- matrix(0, nrow=n, ncol=n)
  M <- matrix(0, nrow=n, ncol=n)
  G <- matrix(0, nrow=n, ncol=n)
  
  B.diff <- matrix(0, nrow=n, ncol=n)
  M.diff <- matrix(0, nrow=n, ncol=n)
  G.diff <- matrix(0, nrow=n, ncol=n)
  
  for(i in 1:n){
    if(! i %in% A){
      for(j in 1:n){
        
        if(Q[i,i]!=0 & i!=j){
          P.diff <- (-D.alpha[i,j]*Q[i,i] + (D.alpha[i,i])*Q[i,j])/(Q[i,i]^2)
        }else{
          P.diff <- 0
        }
        
        
        if(D[i,j]==1){
          
          if(j %in% A){
            G[i,j] <- P[i,j]
            G.diff[i,j] <- P.diff
          }else{
            M[i,j] <- P[i,j]
            M.diff[i,j] <- P.diff
          }
        }else{
          
          if(!j %in% A){
            B[i,j] <- P[i,j]
            B.diff[i,j] <- P.diff
            
          }
        }
      }
    }
  }
  
  
  Lambda.diff <- diag(diag(-D.alpha), nrow=n)
  Lambda <- -diag(diag(Q), nrow=n)
  zero.matrix <- matrix(0, nrow=n, ncol=n)
  I <- diag(1, nrow=n)
  unit <- rep(1, n)
  
  
  
  Theta.1 <- -Lambda.diff%*%(I-B-M) + Lambda%*%(B.diff + M.diff)
  Theta.2 <- Lambda.diff%*%(G+M) + Lambda%*%(G.diff + M.diff)
  Theta.diff <- rbind(cbind(Theta.1, Theta.2), cbind(zero.matrix, zero.matrix))
  
  A.bar <- cbind(-Lambda%*%(I-B-M), Lambda%*%(G+M))
  Theta <- rbind(A.bar, cbind(zero.matrix, zero.matrix))
  
  
  zero.matrix.2 <- diag(0, nrow=nrow(Theta))
  I.2 <- diag(1, nrow=nrow(Theta))
  X.bar <- rbind(cbind(Theta*t, Theta.diff*t), cbind(zero.matrix.2, Theta*t))
  diff <- cbind(I.2, zero.matrix.2)%*%expm(X.bar)%*%rbind(zero.matrix.2, I.2)
  
  derivative <- cbind(I, zero.matrix)%*%diff%*%rbind(zero.matrix, I)%*%unit
  
  return(derivative)
  
}