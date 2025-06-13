general.ward.matrices.element <- function(bay.size, ward.size, infection.states, infectious.states, outbreak, random.arrivals=TRUE, i){
  
  
  
  string.matrix <- t(matrix(1:(10*ward.size), nrow=ward.size))
  
  infection.string <- t(matrix(1:(3*(bay.size^2)), nrow=bay.size^2))
  
  
  
  rates <- matrix(0, nrow=length(infection.states), ncol=length(infection.states))
  
  rates[which(infection.states=="Empty"), which(infection.states=="Susceptible")] <- "lambda"
  rates[which(infection.states=="Susceptible"), which(infection.states=="Empty")] <- "mu.1"
  rates[which(infection.states=="Susceptible"), which(infection.states=="Latent")] <- "eta.background"
  rates[which(infection.states=="Latent"), which(infection.states=="Empty")] <- "mu.2"
  rates[which(infection.states=="Latent"), which(infection.states==infectious.states[1])] <- "rho"
  rates[which(infection.states==infectious.states[1]), which(infection.states=="Empty")] <- "mu.2"
  rates[which(infection.states=="Recovered"), which(infection.states=="Empty")] <- "mu.1"
  
  if(length(infection.states)==5){
    rates[which(infection.states==infectious.states[1]), which(infection.states=="Recovered")] <- "gamma.1"
  }
  
  if(length(infection.states)==6){
    rates[which(infection.states==infectious.states[1]), which(infection.states==infectious.states[2])] <- "gamma.1"
    rates[which(infection.states==infectious.states[2]), which(infection.states=="Empty")] <- "mu.2"
    rates[which(infection.states==infectious.states[2]), which(infection.states=="Recovered")] <- "gamma.2"
    
  }
  
  if(length(infection.states)==7){
    rates[which(infection.states==infectious.states[1]), which(infection.states==infectious.states[2])] <- "gamma.1"
    rates[which(infection.states==infectious.states[2]), which(infection.states=="Empty")] <- "mu.2"
    rates[which(infection.states==infectious.states[2]), which(infection.states==infectious.states[3])] <- "gamma.2"
    rates[which(infection.states==infectious.states[3]), which(infection.states=="Empty")] <- "mu.2"
    rates[which(infection.states==infectious.states[3]), which(infection.states=="Recovered")] <- "gamma.3"
    
  }
  
  
  
  states <- gen.ward.states(bay.size, ward.size, infection.states, outbreak, TRUE)
  states.lab <- gen.ward.states(bay.size, ward.size, infection.states, outbreak, FALSE)
  n <- nrow(states)
  
  Q <- numeric(n)
  Eta.1 <- numeric(n)
  Eta.2 <- numeric(n)
  Eta.3 <- numeric(n)
  Outbreak <- numeric(n)
      
  
  for(j in 1:n){
    diff <- vector.difference(states[i, ], states[j, ], TRUE)
    
    if(diff == 1){
      
      state.changes <- vector.difference(states[i, ], states[j, ], FALSE)
      state.1 <- state.changes[1]
      state.2 <- state.changes[2]
      
      base.states <- length(which(states[i, ]==state.1))
      
      if(any(c(state.1, state.2)==0)){
        if(state.1==0){
          Outbreak[j] <- 1
          
          if(length(which(states.lab[i, ]==infectious.states[1]))>0){
            Eta.1[j] <- infection.string[1, base.states*length(which(states.lab[i, ]==infectious.states[1]))]
            
          }
          
          if(length(infectious.states)>1){
            if(length(which(states.lab[i, ]==infectious.states[2]))>0){
              
              Eta.2[j] <- infection.string[2, base.states*length(which(states.lab[i, ]==infectious.states[2]))]
              
            }
          }
          if(length(infectious.states)>2){
            if(length(which(states.lab[i, ]==infectious.states[3]))>0){
              
              Eta.3[j]  <- infection.string[3, base.states*length(which(states.lab[i, ]==infectious.states[3]))]
            }
          }
          
        }
        
        
        
      }else if(state.1>length(infection.states)){
        
        if(state.2==state.1+1){
          Q[j] <- string.matrix[1, 1]
        }else if(state.1==state.2 + 1){
          Q[j] <- string.matrix[10, as.numeric(states.lab[i, ncol(states.lab)])]
        }
        
      }else{
        
        if(rates[state.1, state.2]=="lambda"){
          
          if(random.arrivals==FALSE){
            if((states[i, ncol(states)]==(ward.size-bay.size)+length(infection.states)+1)){
              
              Q[j] <- string.matrix[9, 1]
            }
          }else{
            Q[j] <- string.matrix[9, 1]
          }
          
          
          
          
        }else if(rates[state.1, state.2]=="mu.1"){
          
          Q[j] <- string.matrix[2, base.states]
          
        }else if(rates[state.1, state.2]=="mu.2"){
          
          Q[j] <- string.matrix[3, base.states]
          
        }else if(rates[state.1, state.2]=="eta.background"){
          
          Q[j] <- string.matrix[4, base.states]
        }else if(rates[state.1, state.2]=="rho"){
          
          Q[j] <- string.matrix[5, base.states]
        }else if(rates[state.1, state.2]=="gamma.1"){
          
          Q[j] <- string.matrix[6, base.states]
        }else if(rates[state.1, state.2]=="gamma.2"){
          
          Q[j] <- string.matrix[7, base.states]
        }else if(rates[state.1, state.2]=="gamma.3"){
          
          Q[j] <- string.matrix[8, base.states]
        }
        
        if(which(infection.states=="Susceptible")==state.1 & which(infection.states=="Latent")==state.2){
          
          if(length(which(states.lab[i, ]==infectious.states[1]))>0){
            Eta.1[j] <- infection.string[1, base.states*length(which(states.lab[i, ]==infectious.states[1]))]
            
          }
          if(length(infectious.states)>1){
            if(length(which(states.lab[i, ]==infectious.states[2]))>0){
              
              Eta.2[j] <- infection.string[2, base.states*length(which(states.lab[i, ]==infectious.states[2]))]
              
            }
          }
          if(length(infectious.states)>2){
            if(length(which(states.lab[i, ]==infectious.states[3]))>0){
              
              Eta.3[j]  <- infection.string[3, base.states*length(which(states.lab[i, ]==infectious.states[3]))]
            }
            
          }
          
          
          
          
          
        }
      }
    }
    
    
  }
  
  
  return(list(Q, Eta.1, Eta.2, Eta.3, Outbreak))
  
  
}
