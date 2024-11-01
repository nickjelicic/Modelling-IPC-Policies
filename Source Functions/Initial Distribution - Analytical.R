analytical.initial.distribution <- function(bay.size, ward.size, infection.states, infectious.states,gen.matrices, infection.rates,
                                            time.in.contact, ward.occupants){
  
  A <- NULL
  states.lab <- gen.ward.states(bay.size, ward.size, infection.states,TRUE, FALSE)
  n <- nrow(states.lab)
  
  initial.infection.state <- infectious.states[1]
  
  initial.state.lab <- c(rep("Empty", bay.size-1), initial.infection.state,"No outbreak","0")
  
  initial.state <- which(apply(states.lab, 1, function(x) all.equal(x,initial.state.lab)==TRUE))
  
  
  initial.distribution <- numeric(n)
  initial.distribution[initial.state] <- 1
  
  if(bay.size>1){
    
    arrival.rate <- 0
    departure.rates <- c(0,0)
    
    
    recovery.rates[2:length(recovery.rates)] <- 0
    
    Q <- transition.rate.matrix.ward(bay.size, ward.size, gen.matrices, arrival.rate, departure.rates, 
                                     departure.rates, recovery.rates, infection.rates, 0, 0)
    
    l <- length(time.in.contact)
    times <- c(sort(time.in.contact, decreasing=TRUE), 0)
    
    
    
    D.arrivals <- direct.arrivals(bay.size, ward.size, infection.states, TRUE)
    
    
    
    for(r in 1:l){
      initial.distribution <- initial.distribution%*%D.arrivals
      
      initial.distribution <- initial.distribution%*%expm(Q*(times[r]-times[r+1]))
    }
  }

  
  initial.distribution <- initial.distribution%*%direct.infectious.dep(bay.size, ward.size, infectious.states, infection.states)
  
  initial.distribution <- initial.distribution%*%direct.ward.arrivals(bay.size, ward.size, infection.states, ward.occupants)
  
  
  return(initial.distribution)
  
}