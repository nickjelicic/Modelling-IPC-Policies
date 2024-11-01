direct.departures <- function(bay.size, ward.size, infectious.states, infection.states){
  
  states.lab <- gen.ward.states(bay.size,ward.size, infection.states, TRUE, FALSE)
  states <- gen.ward.states(bay.size, ward.size, infection.states, TRUE, TRUE)
  n.states <- nrow(states.lab)
  D.departures <- matrix(0, nrow=n.states, ncol=n.states)
  
  for(i in 1:n.states){
    
    occupied.beds <- which(states.lab[i, 1:bay.size]!="Empty")
    
    if(length(occupied.beds)>0){
      for(j in occupied.beds){
        
        new.state <- states[i, ]
        new.state[j] <- 1
        new.state[1:bay.size] <- sort(new.state[1:bay.size])
        new.state.num <- which(apply(states, 1, function(x) all.equal(x, new.state))=="TRUE")
        
        D.departures[i, new.state.num] <- 1
      }
    }
    
    occupied.ward.beds <- as.numeric(states.lab[i, ncol(states.lab)])
    
    if(occupied.ward.beds>0){
      new.state <- states.lab[i, ]
      new.state[ncol(states.lab)] <- occupied.ward.beds - 1
      
      new.state.num <- which(apply(states.lab, 1, function(x) all.equal(x, new.state))=="TRUE")
      
      D.departures[i, new.state.num] <- 1
    }
    
  }
  
  return(D.departures)
}