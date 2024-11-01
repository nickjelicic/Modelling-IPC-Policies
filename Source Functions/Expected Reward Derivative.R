expected.reward.derivative <- function(initial.distribution, initial.distribution.derivative, Q, Q.diff, R, A, t){
  
  if(t==0){
    return(0)
  }else{
    n <- nrow(Q)
    
    Q.bar <- Q
    Q.bar[A, ] <- 0
    
    Q.bar.diff <- Q.diff
    Q.bar.diff[A, ] <- 0
    
    I <- diag(1, nrow=n)
    zero.matrix <- matrix(0, nrow=n, ncol=n)
    unit <- rep(1, n)
    
    
    Phi <- rbind(cbind(Q.bar, R), cbind(zero.matrix, zero.matrix))
    Phi.diff <- rbind(cbind(Q.bar.diff, zero.matrix), cbind(zero.matrix, zero.matrix))
    
    Mat <- rbind( cbind(Phi, Phi.diff), cbind(matrix(0, nrow=2*n, ncol=2*n), Phi))
    
    derivative.term.1 <- initial.distribution %*% cbind(I, matrix(0, nrow=n, ncol=3*n))%*%expm(Mat*t)%*%rbind(matrix(0, nrow=3*n, ncol=n), I)%*%unit
    
    if(any(initial.distribution.derivative!=0)){
      derivative.term.2 <- initial.distribution.derivative %*% cbind(I, zero.matrix) %*% expm(Phi*t) %*% rbind(zero.matrix, I) %*% unit
    }else{
      derivative.term.2 <- 0
    }
    
    
    return(derivative.term.1 + derivative.term.2)
  }

}