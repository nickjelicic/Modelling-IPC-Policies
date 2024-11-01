gen.ward.states <- function(bay.size, ward.size, infection.states,outbreak, numeric.states){
  
  ## We need to give an order to the states so that they can be given numerical labels
  ## For each bed (with all other beds being the same), the states occur in the order empty, S, L, I1, I2, I3, R (lowest to highest)
  ## Using the order of states above, the k beds must occur in ascending order (e.g. S, S, L, L, NOT S, L, S,L) 
  
  # since there are 
  
  finish <- FALSE
  
  state <- rep(1, bay.size)
  Mat.states <- state
  
  while(finish==FALSE){

    
    for(i in bay.size:1){
      
      if(state[i] < length(infection.states)){
        
        state[i:bay.size] <- state[i] + 1
        break
      }
    }
    
    Mat.states <- rbind(Mat.states, state)
    
    if(all(state==length(infection.states))){
      finish <- TRUE
    }
    
  }
  
  if(outbreak){
    Mat.states <- rbind(cbind(Mat.states, 0), cbind(Mat.states, -1))
    n.row <- nrow(Mat.states)
    
    empty.beds <- NULL
    Ward.states <- NULL
    for(i in 0:(ward.size-bay.size)){
      empty.beds <- rep(length(infection.states) + 1 + i, n.row)
      Ward.states <- rbind(Ward.states, cbind(Mat.states, empty.beds))
    }
    
    Ward.states <- unname(Ward.states)
    if(numeric.states==TRUE){
      return(Ward.states)
    }else{
      
      no.outbreak.states <- which(Ward.states[, (bay.size + 1)]==0)
      Ward.states[no.outbreak.states, (bay.size + 1)] <- "No outbreak"
      Ward.states[-no.outbreak.states, (bay.size + 1)] <- "Outbreak"
      
      
      
      for(i in 1:length(infection.states)){
        Ward.states[which(Ward.states==i)] <- infection.states[i]
      }
      
      Ward.states[, (bay.size + 2)] <- as.numeric(Ward.states[, (bay.size + 2)])-length(infection.states) - 1
      
      return(unname(Ward.states))
    }
    
  }else{
    n.row <- nrow(Mat.states)
    
    empty.beds <- NULL
    Ward.states <- NULL
    for(i in 0:(ward.size-bay.size)){
      empty.beds <- rep(length(infection.states) + 1 + i, n.row)
      Ward.states <- rbind(Ward.states, cbind(Mat.states, empty.beds))
    }
    
    if(numeric.states==TRUE){
      return(Ward.states)
    }else{
      for(i in 1:length(infection.states)){
        Ward.states[which(Ward.states==i)] <- infection.states[i]
      }
      
      Ward.states[, (bay.size + 1)] <- as.numeric(Ward.states[, (bay.size + 1)])-length(infection.states) - 1
      
      return(unname(Ward.states))
    }
  }

  
}