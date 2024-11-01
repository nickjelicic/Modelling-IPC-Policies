direct.infections <- function(bay.size, ward.size, infection.states){
  
  states.lab <- gen.ward.states(bay.size, ward.size, infection.states, TRUE, FALSE)
  states <- gen.ward.states(bay.size, ward.size, infection.states, TRUE, TRUE)
  n.states <- nrow(states.lab)
  
  D.I <- matrix(0, nrow=n.states, ncol=n.states)
  
  for(i in 1:n.states){
    
    susceptibles <- which(states.lab[i, ]=="Susceptible")
    
    if(length(susceptibles)>0){
      
      new.state <- states[i, ]
      new.state[susceptibles[1]] <- which(infection.states=="Susceptible") + 1
      new.state[1:bay.size] <- sort(new.state[1:bay.size])
      
      state.label <- which(apply(states, 1, function(x) all.equal(x, new.state)) == "TRUE")
      
      D.I[i, state.label] <- 1
    }
    
    
  }
  
  
  return(D.I)
}