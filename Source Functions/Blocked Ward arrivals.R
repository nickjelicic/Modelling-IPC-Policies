blocked.ward.arrivals <- function(bay.size, ward.size, infection.states, Q, A, t, lambda, restrictions=FALSE){
  
  n <- nrow(Q)
  S <- 1:n
  
  states.lab <- gen.ward.states(bay.size, ward.size, infection.states,TRUE, FALSE)
  
  if(restrictions==TRUE){
    
    C <- which(apply(states.lab, 1, function(x) x[bay.size + 2]==ward.size-bay.size))
    
  }else{
    
    C <- which(apply(states.lab, 1, function(x) (!any(x =="Empty")) == "TRUE" & x[bay.size+2]==ward.size-bay.size))
    
    
  }
  
  blocked.arrivals <- lambda * time.in.state(Q, C, A, t)
  
  return(blocked.arrivals)
}


