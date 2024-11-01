transition.rate.matrix.ward <- function(bay.size, ward.size, gen.matrices, arrival.rate, discharge.rates, departure.rates, recovery.rates, infection.rates, spread.coef, restrictions.arrival.rate){
  
  sequence <- 1:ward.size
  rates.matrix <- t(matrix(c(arrival.rate*sequence, discharge.rates[1]*sequence, discharge.rates[2]*sequence, infection.rates[1]*sequence,
                             recovery.rates[1]*sequence, 
                             recovery.rates[2]*sequence, recovery.rates[3]*sequence, recovery.rates[4]*sequence,
                             restrictions.arrival.rate*sequence, 
                             departure.rates[1]*sequence), nrow=ward.size))
  seq.2 <- 1:(bay.size^2)
  infection.rates.matrix <- t(matrix(c(infection.rates[2] * seq.2, infection.rates[3]*seq.2, infection.rates[4]*seq.2), nrow=bay.size^2))
  
  string.matrix <- t(matrix(1:(10*ward.size), nrow=ward.size))
  infection.string <- t(matrix(1:(3*(bay.size^2)), nrow=bay.size^2))
  
  Q.general <- gen.matrices[[1]]
  Eta.1.general <- gen.matrices[[2]]
  Eta.2.general <- gen.matrices[[3]]
  Eta.3.general <- gen.matrices[[4]]
  
  n <- nrow(Q.general)
  Q <- matrix(0, nrow=n, ncol=n)
  
  
  if(length(gen.matrices)==5){
    Outbreak.general <- gen.matrices[[5]]
  }else{
    Outbreak.general <- matrix(1, nrow=n, ncol=n)
  }
  
  
  
    
  states.lab <- gen.ward.states(bay.size, ward.size, infection.states, TRUE, FALSE)
  occupancy <- as.numeric(states.lab[, bay.size+2])
  total.occupancy <- NULL
  for(i in 1:n){
    total.occupancy <- c(total.occupancy, length(which(states.lab[i, 1:bay.size]!="Empty")) + as.numeric(states.lab[i, bay.size + 2]))
  }
  
  
  
  background.infection <- which(Q.general%in%string.matrix[4, ])
  for(i in background.infection){
    label <- which(string.matrix==Q.general[i])
    Q[i] <- rates.matrix[label]/total.occupancy[i%%n]
  }
  
  w <- which(!Q.general%in%c(0, string.matrix[4, ]))
  for(i in w){
    
    label <- which(string.matrix==Q.general[i])
    
    Q[i] <- rates.matrix[label]
  }
  
  outbreak <- which(Outbreak.general==1)
  
  w <- which(Eta.1.general!="0")
  for(i in w){
    label <- which(infection.string==Eta.1.general[i])
    if(i %in% outbreak){
      Q[i] <- Q[i] + infection.rates.matrix[label]*spread.coef*occupancy[i%%n]/total.occupancy[i%%n]
    }else{
      Q[i] <- Q[i] + infection.rates.matrix[label]/total.occupancy[i%%n]
    }
    
    
  }
  
  w <- which(Eta.2.general!="0")
  for(i in w){
    label <- which(infection.string==Eta.2.general[i])
    if(i %in% outbreak){
      Q[i] <- Q[i] + infection.rates.matrix[label]*spread.coef*occupancy[i%%n]/total.occupancy[i%%n]
    }else{
      Q[i] <- Q[i] + infection.rates.matrix[label]/total.occupancy[i%%n]
    }
    
    
  }
  
  
  w <- which(Eta.3.general!="0")
  for(i in w){
    label <- which(infection.string==Eta.3.general[i])
    if(i %in% outbreak){
      Q[i] <- Q[i] + infection.rates.matrix[label]*spread.coef*occupancy[i%%n]/total.occupancy[i%%n]
    }else{
      Q[i] <- Q[i] + infection.rates.matrix[label]/total.occupancy[i%%n]
    }
    
    
  }
  
  for(i in outbreak){
    if(occupancy[i%%n]>0){
      Q[i] <- Q[i] + infection.rates[1]*occupancy[i%%n]/total.occupancy[i%%n]
    }
    
  }
  

  
  for(i in 1:n){
    Q[i, i] <- -sum(Q[i, ])
  }
  
  return(Q)
  
}
