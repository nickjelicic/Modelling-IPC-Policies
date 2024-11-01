direct.arrivals <- function(bay.size, ward.size, infection.states, bay.focus=FALSE){
  
  states.lab <- gen.ward.states(bay.size,ward.size, infection.states,TRUE,  FALSE)
  states <- gen.ward.states(bay.size, ward.size, infection.states,TRUE, TRUE)
  n.states <- nrow(states.lab)
  D.arrivals <- matrix(0, nrow=n.states, ncol=n.states)
  
  for(i in 1:n.states){
    
    empty <- which(states.lab[i, ]=="Empty")
    
    if(length(empty)>0){
      
      new.state <- states[i, ]
      new.state[empty[1]] <- which(infection.states=="Empty") + 1
      new.state[1:bay.size] <- sort(new.state[1:bay.size])
      
      state.label <- which(apply(states, 1, function(x) all.equal(x, new.state)) == "TRUE")
      
      D.arrivals[i, state.label] <- 1
    }
    
    if(bay.focus==FALSE){
      ward.occupancy <- as.numeric(states.lab[i, ncol(states.lab)])
      if(ward.occupancy<(ward.size-bay.size)){
        new.state <- states.lab[i, ]
        new.state[ncol(states.lab)] <- ward.occupancy + 1
        
        state.label <- which(apply(states.lab, 1, function(x) all.equal(x, new.state)) == "TRUE")
        
        D.arrivals[i, state.label] <- 1
      }
    }
    

  }
  
  return(D.arrivals)
}