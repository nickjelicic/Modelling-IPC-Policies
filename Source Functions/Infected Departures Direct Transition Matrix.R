direct.infected.dep <- function(bay.size, ward.size, infectious.states, infection.states){
  
  states.lab <- gen.ward.states(bay.size,ward.size, infection.states, TRUE, FALSE)
  states <- gen.ward.states(bay.size, ward.size, infection.states, TRUE, TRUE)
  n.states <- nrow(states.lab)
  D.infectious.dep <- matrix(0, nrow=n.states, ncol=n.states)
  
  for(i in 1:n.states){
    
    infectious <- which(states.lab[i, ]%in%c("Latent", infectious.states))
    
    if(length(infectious)>0){
      
      for(j in 1:length(infectious)){
        
        new.state <- states[i, ]
        new.state[infectious[j]] <- which(infection.states=="Empty")
        new.state[1:bay.size] <- sort(new.state[1:bay.size])
        
        state.label <- which(apply(states, 1, function(x) all.equal(x, new.state)) == "TRUE")
        
        D.infectious.dep[i, state.label] <- 1
      }
      
    }
    
    
  }
  
  return(D.infectious.dep)
}