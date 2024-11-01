derivative.Q.matrix <- function(gen.matrices, arrival.rate, discharge.rates, departure.rates, infection.rates, recovery.rates, spread.coef,
                                restrictions.arrival.rate, parameter){
  n <- nrow(gen.matrices[[1]])
  D <- matrix(0, nrow=n, ncol=n)
  sequence <- 1:ward.size
  rates.matrix <- t(matrix(c(arrival.rate*sequence, discharge.rates[1]*sequence, discharge.rates[2]*sequence, infection.rates[1]*sequence,
                             recovery.rates[1]*sequence, 
                             recovery.rates[2]*sequence, recovery.rates[3]*sequence, recovery.rates[4]*sequence,
                             restrictions.arrival.rate*sequence, 
                             departure.rates[1]*sequence), nrow=ward.size))
  
  string.matrix <- t(matrix(1:(10*ward.size), nrow=ward.size))
  string.matrix <- data.frame(string.matrix, row.names=c("Arrival Rate", "Uninfected Discharge Rate", "Infected Discharge Rate", 
                                                         "Background Infection Rate", "Latency Rate", "Recovery Rate", "Early symptomatic Rate", 
                                                         "Late symptomatic Rate", "Restriction arrival rate", "Departure Rate"))
  
  
  infection.string <- t(matrix(1:(3*(bay.size^2)), nrow=bay.size^2))
  
  if(parameter=="Infection Rate"){
    string <- infection.string[1, ]
    
    for(i in 1:length(string)){
      transitions <- which(gen.matrices[[2]]==as.numeric(string[i]))
      transitions.outbreak <- transitions[which(gen.matrices[[5]][transitions]==1)]
      transitions.no.outbreak <- transitions[which(gen.matrices[[5]][transitions]==0)]
      D[transitions.no.outbreak] <- i
      D[transitions.outbreak] <- i*spread.coef
    }
  }else if(parameter=="Early symptomatic Infection Rate"){
    string <- infection.string[2, ]
    
    for(i in 1:length(string)){
      transitions <- which(gen.matrices[[3]]==as.numeric(string[i]))
      D[transitions] <- i
    }
  }else if(parameter=="Late symptomatic Infection Rate"){
    string <- infection.string[3, ]
    
    for(i in 1:length(string)){
      transitions <- which(gen.matrices[[4]]==as.numeric(string[i]))
      D[transitions] <- i
    }
  }else if(parameter=="Arrival Rate"){
    string <- string.matrix[parameter, ]
    for(i in 1:length(string)){
      transitions <- which(gen.matrices[[1]]==as.numeric(string[i]))
      D[transitions] <- i
    }
    
    if(restrictions.arrival.rate>0){
      string <- string.matrix["Restriction arrival rate", ]
      for(i in 1:length(string)){
        transitions <- which(gen.matrices[[1]]==as.numeric(string[i]))
        D[transitions] <- i
      }
    }
  }else if(parameter=="Discharge Rate"){
    
    string <- string.matrix["Uninfected Discharge Rate", ]
    
    for(i in 1:length(string)){
      transitions <- which(gen.matrices[[1]]==as.numeric(string[i]))
      D[transitions] <- i
    }
    
    string <- string.matrix["Departure Rate", ]
    
    for(i in 1:length(string)){
      transitions <- which(gen.matrices[[1]]==as.numeric(string[i]))
      D[transitions] <- i
    }
    
    if(discharge.rates[1]==discharge.rates[2]){
      string <- string.matrix["Infected Discharge Rate", ]
      
      for(i in 1:length(string)){
        transitions <- which(gen.matrices[[1]]==as.numeric(string[i]))
        D[transitions] <- i
      }
    }
    
   
    
  }else if(parameter=="Transfer Rate"){
    
    string <- string.matrix["Departure Rate",]
    for(i in 1:length(string)){
      transitions <- which(gen.matrices[[1]]==as.numeric(string[i]))
      D[transitions] <- i
    }
    
    if(restrictions.arrival.rate==arrival.rate){
      string <- string.matrix["Uninfected Discharge Rate", ]
      for(i in 1:length(string)){
        transitions <- which(gen.matrices[[1]]==as.numeric(string[i]))
        D[transitions] <- i
      }
      
      if(discharge.rates[1]==discharge.rates[2]){
        string <- string.matrix["Infected Discharge Rate", ]
        for(i in 1:length(string)){
          transitions <- which(gen.matrices[[1]]==as.numeric(string[i]))
          D[transitions] <- i
        }
      }
      
    }
    
    
    
  }else{
    string <- string.matrix[parameter, ]
    
    for(i in 1:length(string)){
      transitions <- which(gen.matrices[[1]]==as.numeric(string[i]))
      D[transitions] <- i
    }
    
    if(parameter=="Background Infection Rate"){
      outbreak.transitions <- which(gen.matrices[[5]]==1)
      D[outbreak.transitions] <- D[outbreak.transitions] + 1
    }
  }
  

  
  for(i in 1:n){
    D[i, i] <- -sum(D[i, ])
  }
  
  return(D)
}