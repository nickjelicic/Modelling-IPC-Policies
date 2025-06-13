infection.times.sample <- function(T, infection.status, infection.rates, focus.bay, infection.states, infectious.states, spread.to.focus.bay){
  
  infection.times <- matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  n.bay <- nrow(infection.status)
  ward.occupancy <- length(which(infection.status %in% infection.states[-1]))
  
  if(spread.to.focus.bay==FALSE){
    
    for(bay in 1:n.bay){
      infection.hazard <- infection.rates[1]
      for(k in 1:length(infectious.states)){
        
        if(bay==1){
          infection.hazard <- infection.hazard + infection.rates[2, k]*length(which(infection.status[focus.bay, ]==infectious.states[k]))
        }else if(bay==focus.bay){
          infection.hazard <- infection.hazard + infection.rates[3, k]*length(which(infection.status[focus.bay, ]==infectious.states[k]))
        }else{
          infection.hazard <- infection.hazard + infection.rates[4, k]*length(which(infection.status[focus.bay, ]==infectious.states[k]))
        }
      }
      
      susceptibles <- which(infection.status[bay, ]=="Susceptible")
      if(length(susceptibles)>0 & infection.hazard>0){
        infection.times[bay, susceptibles] <- T + rexp(length(susceptibles), infection.hazard)
      }
    }
   
  }else{
    
    for(bay in 1:n.bay){
      infection.hazard <- infection.rates[1]
      
      for(k in 1:length(infectious.states)){
        if(bay==1){
          infection.hazard <- infection.hazard + infection.rates[2, k]*length(which(infection.status[1, ]==infectious.states[k])) + 
            infection.rates[2, k]*length(which(infection.status[-1, ]==infectious.states[k]))
        }else{
          infection.hazard <- infection.hazard + infection.rates[2, k]*length(which(infection.status[1, ]==infectious.states[k])) + 
            infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k])) + 
            infection.rates[4, k]*length(which(infection.status[-c(1, bay), ]==infectious.states[k]))
        }
      }
      susceptibles <- which(infection.status[bay, ]=="Susceptible")
      if(length(susceptibles)>0 & infection.hazard>0){
        infection.times[bay, susceptibles] <- T + rexp(length(susceptibles), infection.hazard)
      }
    }
  }
 
  return(infection.times)
}