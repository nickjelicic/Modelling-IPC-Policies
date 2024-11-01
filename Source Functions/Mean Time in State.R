time.in.state <- function(Q, C, A, t){
  
  Q[A, ] <- 0
  n <- nrow(Q)
  I <- diag(1, nrow=n, ncol=n)
  
  A.bar <- cbind(Q, I)
  zero.matrix <- matrix(0, nrow=n, ncol=n)
  A.bar <- rbind(A.bar, cbind(zero.matrix, zero.matrix))
  
  Integral <- cbind(I, zero.matrix)%*%expm(A.bar * t)%*%rbind(zero.matrix, I)
  
  if(length(C)>1){
    Times <- Integral[, C] %*% rep(1, length(C))
  }else{
    Times <- Integral[,C]
  }
  
  
  return(Times)
}