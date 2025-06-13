direct.infected.dep.row <- function(bay.size, ward.size, infectious.states, infection.states, row){
  
  states.lab <- gen.ward.states(bay.size,ward.size, infection.states, TRUE, FALSE)
  states <- gen.ward.states(bay.size, ward.size, infection.states, TRUE, TRUE)
  n.states <- nrow(states.lab)
  D.infectious.dep.row <- numeric(n.states)
    
  infectious <- which(states.lab[row, ]%in%c("Latent", infectious.states))
    
    if(length(infectious)>0){
      
      for(j in 1:length(infectious)){
        
        new.state <- states[row, ]
        new.state[infectious[j]] <- which(infection.states=="Empty")
        new.state[1:bay.size] <- sort(new.state[1:bay.size])
        
        state.label <- which(apply(states, 1, function(x) all.equal(x, new.state)) == "TRUE")
        
        D.infectious.dep[state.label] <- 1
      }
      
    }
  
  return(D.infectious.dep)
}