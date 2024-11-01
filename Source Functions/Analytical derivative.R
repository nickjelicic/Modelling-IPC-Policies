derivative.Q.matrix <- function(gen.matrices, parameter){
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
                                                         "Background Infection Rate", "Latency Rate", "Incubation Rate", "Early symptomatic Rate", 
                                                         "Late symptomatic Rate", "Restriction arrival rate", "Departure Rate"))
  
  infection.string <- t(matrix(1:(3*(bay.size^2)), nrow=bay.size^2))
  
  if(parameter=="Infection Rate"){
    string <- infection.string[1, ]
    
    for(i in 1:length(string)){
      transitions <- which(gen.matrices[[2]]==string[i])
      D[transitions] <- i
    }
  }else if(parameter=="Early symptomatic Infection Rate"){
    string <- infection.string[2, ]
    
    for(i in 1:length(string)){
      transitions <- which(gen.matrices[[3]]==string[i])
      D[transitions] <- i
    }
  }else if(parameter=="Late symptomatic Infection Rate"){
    string <- infection.string[3, ]
    
    for(i in 1:length(string)){
      transitions <- which(gen.matrices[[4]]==string[i])
      D[transitions] <- i
    }
  }else{
    string <- string.matrix[parameter, ]
    
    for(i in 1:length(string)){
      transitions <- which(gen.matrices[[1]]==string[i])
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