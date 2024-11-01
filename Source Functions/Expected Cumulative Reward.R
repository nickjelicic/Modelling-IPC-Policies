expected.cumulative.reward <- function(Q, R, t, A){
  
  Q[A, ] <- 0
  
  n <- nrow(Q)
  
  zero.matrix <- matrix(0, nrow=n, ncol=n)
  I <- diag(1, nrow=n, ncol=n)
  
  A.bar <- rbind(cbind(Q, A), 
                 cbind(zero.matrix, zero.matrix))
  
  Integral <- cbind(I, zero.matrix) %*% expm(A.bar*t) %*% rbind(zero.matrix, I)
  
  
  unit <- rep(1, n)
  
  expected.reward <- Integral%*%unit
  
  return(expected.reward)
}