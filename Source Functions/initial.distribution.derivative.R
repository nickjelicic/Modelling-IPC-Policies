initial.distribution.derivative <- function(bay.size, ward.size, Q.restrictions, Q.restrictions.diff, quarantine.time, initial.distribution, time.in.contact, ward.occupants,
                                            D.infectious.dep, D.bay.arrival, D.k.ward.arrivals, Q.I, Q.I.diff){
  
  n <- nrow(Q.restrictions)
  states.lab <- gen.ward.states(bay.size, ward.size, infection.states, TRUE, FALSE)
  
  I <- diag(1, nrow=n)
  zero.matrix <- matrix(0, nrow=n,ncol=n)
  
  Mat <- rbind(cbind(Q.restrictions, Q.restrictions.diff), cbind(zero.matrix, Q.restrictions))
  matrix.derivative <- cbind(I, zero.matrix)%*%expm(Mat*quarantine.time)%*%rbind(zero.matrix, I)
  
  time.in.contact <- c(sort(time.in.contact, decreasing=TRUE), 0)
  
  initial.states.lab <- c(rep("Empty", bay.size-1),"Infectious", "No outbreak", 0)
  
  infectious.distribution <- which(apply(states.lab, 1, function(x) all.equal(x,initial.states.lab)==TRUE)) 
  
  initial.distribution <- matrix(0, nrow=1, ncol=n)
  initial.distribution[infectious.distribution] <- 1
  
  
  zero.matrix <- matrix(0, nrow=n, ncol=n)
  I <- diag(1, nrow=n)
  
  initial.dist.deriv <- numeric(n)
  
  for(i in 1:(length(time.in.contact)-1)){
    initial.dist.i <- I
    if(i>1){
      for(k in 1:(i-1)){
        initial.dist.i <- initial.dist.i%*%(D.bay.arrival %*% expm(Q.I*(time.in.contact[k]-time.in.contact[k+1])))
      }
    }
    
    Mat <- rbind(cbind(Q.I, Q.I.diff), cbind(zero.matrix, zero.matrix))
    
    derivative <- D.bay.arrival %*% cbind(I, zero.matrix)%*%expm(Mat*(time.in.contact[i]-time.in.contact[i+1]))%*%rbind(zero.matrix, I)
    
    initial.dist.i <- initial.dist.i%*%derivative
    
    if(i<(length(time.in.contact)-1)){
      for(k in (i+1):(length(time.in.contact)-1)){
        initial.dist.i <- initial.dist.i%*%(D.bay.arrival %*% expm(Q.I*(time.in.contact[k]-time.in.contact[k+1])))
      }
    }
    
    initial.dist.i <- initial.dist.i %*% D.infectious.dep %*% D.k.ward.arrivals
    
    initial.dist.deriv <- initial.dist.deriv+ initial.distribution%*%initial.dist.i
  }
  
  initial.dist.derivative <- initial.dist.deriv%*%expm(Q.restrictions*quarantine.time) + initial.distribution%*%matrix.derivative
  
  return(initial.dist.derivative)
}