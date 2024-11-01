direct.ward.arrivals <- function(bay.size, ward.size, infection.states, ward.occupants){
  
  states.lab <- gen.ward.states(bay.size,ward.size, infection.states,TRUE,  FALSE)
  n.states <- nrow(states.lab)
  D.ward.arrivals <- matrix(0, nrow=n.states, ncol=n.states)
  
  empty.ward <- which(states.lab[, ncol(states.lab)]=="0")
  for(i in empty.ward){
    new.state <- states.lab[i, ]
    new.state[length(new.state)] <- ward.occupants
    
    new.state.num <- which(apply(states.lab, 1, function(x) all.equal(x, new.state)) == "TRUE")
    D.ward.arrivals[i, new.state.num] <- 1
  }
  
  return(D.ward.arrivals)
}