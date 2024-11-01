unused.bed.days.ward <- function(bay.size, ward.size, infection.states, Q, A, t){
  
  n <- nrow(Q)
  states.lab <- gen.ward.states(bay.size, ward.size, infection.states,TRUE, FALSE)
  
  C <- NULL
  bed.days.r <- NULL
  unused.bed.days <- numeric(n)
  
  Q[A, ] <- 0
  I <- diag(1, nrow=n, ncol=n)
  
  A.bar <- cbind(Q, I)
  zero.matrix <- matrix(0, nrow=n, ncol=n)
  A.bar <- rbind(A.bar, cbind(zero.matrix, zero.matrix))
  
  Integral <- cbind(I, zero.matrix)%*%expm(A.bar * t)%*%rbind(zero.matrix, I)
  
  for(r in 0:(ward.size-bay.size)){
    
    C.r <- which(apply(states.lab, 1, function(x) x[length(x)]==r))
    
    Times <- Integral[, C.r]%*%rep(1, length(C.r))
    
    bed.days.r <- cbind(bed.days.r, (ward.size-bay.size - r) * Times)
    
  }
  
  for(i in 1:n){
    unused.bed.days[i] <- sum(bed.days.r[i, ])
  }
  
  
  return(unused.bed.days)
}