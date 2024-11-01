unused.bed.days.bay <- function(bay.size, ward.size, infection.states, Q, A, t){
  
  n <- nrow(Q)
  states.lab <- gen.ward.states(bay.size, ward.size, infection.states,TRUE, FALSE)
  
  C <- NULL
  bed.days.r <- NULL
  unused.bed.days <- NULL
  
  Q[A, ] <- 0
  n <- nrow(Q)
  I <- diag(1, nrow=n, ncol=n)
  
  A.bar <- cbind(Q, I)
  zero.matrix <- matrix(0, nrow=n, ncol=n)
  A.bar <- rbind(A.bar, cbind(zero.matrix, zero.matrix))
  
  Integral <- cbind(I, zero.matrix)%*%expm(A.bar * t)%*%rbind(zero.matrix, I)
  
  for(r in 1:bay.size){
    
    C.r <- which(apply(states.lab, 1, function(x) length(which(x =="Empty"))==r))
    
    if(length(C.r)>1){
      Times <- Integral[, C.r]%*%rep(1, length(C.r))
    }else{
      Times <- Integral[, C.r]
    }
    
    bed.days.r <- cbind(bed.days.r, r * Times)

  }
  
  for(i in 1:n){
    unused.bed.days[i] <- sum(bed.days.r[i, ])
  }
  
  
  return(unused.bed.days)
}