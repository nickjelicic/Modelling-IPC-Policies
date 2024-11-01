infection.probability.initial.distribution <- function(infection.prob, bay.size, ward.size, infection.states, ward.occupants){
  
  states.num <- gen.ward.states(bay.size, ward.size, infection.states, TRUE, TRUE)
  states.lab <- gen.ward.states(bay.size, ward.size, infection.states, TRUE, FALSE)
  n <- nrow(states.num)
  
  infection.prob <- c(infection.prob, 1)
  infection.distribution <- numeric(length(infection.states))
  infection.distribution[1] <- infection.prob[1]
  for(i in 2:length(infection.states)){
    infection.distribution[i] <- infection.prob[i]*(1-sum(infection.distribution[1:(i-1)]))
  }
  
  
  initial.distribution <- numeric(n)
  
  for(i in 1:n){
    if(states.lab[i, ncol(states.lab)]==ward.occupants & states.lab[i, ncol(states.lab)-1]=="No outbreak"){
      
      unique.values <- unique(states.num[i, 1:bay.size])
      initial.distribution[i] <- factorial(bay.size)
      for(k in 1:length(unique.values)){
        initial.distribution[i] <- initial.distribution[i]/factorial(length(which(states.num[i, 1:bay.size]==unique.values[k])))
      }
      for(j in 1:bay.size){
        initial.distribution[i] <- initial.distribution[i]*infection.distribution[states.num[i, j]]
      }
    }
  }
  
  return(initial.distribution)
}