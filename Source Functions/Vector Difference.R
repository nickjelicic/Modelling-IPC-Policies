vector.difference <- function(u, v, Diff){
  
  elements <- unique( c(unique(u), unique(v)))
  
  table.1 <- as.data.frame(table(factor(u, levels=elements)))
  table.2 <- as.data.frame(table(factor(v, levels=elements)))
  
  Table <- cbind(table.1, table.2[, 2])
  
  if(Diff==TRUE){
    difference <- sum(abs(Table[, 2] - Table[, 3]))/2
    
    return(difference)
  }else{
    
    state.1 <- elements[as.numeric(Table[which(Table[, 2]>Table[, 3]), 1])]
    state.2 <- elements[as.numeric(Table[which(Table[, 2] < Table[, 3]), 1])]
    
    return(c(state.1, state.2))
  }
  
  
}

