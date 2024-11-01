post.restriction.distribution <- function(states.lab, initial.distribution, Q.restrictions, A, quarantine.time, transfer.rates, bay.size, n.states, queue.departures){
  
  final.states <- matrix(initial.distribution, nrow=1)%*%end.states(Q.restrictions, A, quarantine.time)
  
  if(queue.departures==FALSE){
    return(final.states)
  }else{
    
    p <- 1-exp(-transfer.rates[1]*quarantine.time)
    
    final.states.new <- numeric(n.states)
    for(i in 1:n.states){
      occupied.beds <- which(states.lab[i, 1:bay.size]!="Empty")
      
      if(length(occupied.beds)==0){
        final.states.new[i] <- final.states.new[i] + final.states[i]
      }else{
        
        ## don't remove any beds
        
        final.states.new[i] <- final.states.new[i] + final.states[i]*(1-p)^(length(occupied.beds))
        
        for(j in 1:length(occupied.beds)){
          ## remove j of the occupied beds
          
          if(length(occupied.beds)>1){
            combinations <- combn(occupied.beds, j)
            
            for(l in 1:ncol(combinations)){
              new.state <- states.lab[i, ]
              new.state[combinations[, l]] <- "Empty"
              
              new.state.num <- which(apply(states.lab, 1, function(x) all(x==new.state)))
              final.states.new[new.state.num] <- final.states.new[new.state.num] + final.states[i]*(p^j)*(1-p)^(length(occupied.beds)-j)
            }
          }else{
            new.state <- states.lab[i, ]
            new.state[occupied.beds] <- "Empty"
            
            new.state.num <- which(apply(states.lab, 1, function(x) all(x==new.state)))
            final.states.new[new.state.num] <- final.states.new[new.state.num] + final.states[i]*(p^j)*(1-p)^(length(occupied.beds)-j)
            
            
          }
          
          
          
          
        }
      }
    }
    return(final.states.new)
  }

}