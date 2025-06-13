infection.estimates <- function(Data){

  n <- nrow(Data)
  number.patients <- as.numeric(Data[, 3])
  Incubating <- as.numeric(Data[, 4])
  Incubating.infectious <- as.numeric(Data[, 5])
  New.Infectious <- as.numeric(Data[,6])
  Still.Infectious <- as.numeric(Data[, 7])
  New.Deaths <- as.numeric(Data[, 8])
  
  Infectious <- New.Infectious + Still.Infectious + Incubating.infectious
  Susceptible <- number.patients - Incubating - Infectious
  
  
  
  
  ## find clusters of infections
  ## define a cluster to start when there is a new incubating patient(s) after no infections
  
  No.susceptibles <- which(Susceptible<=0)
  
  
  if(length(No.susceptibles)>0){
    X <- cbind(Susceptible, Susceptible*(New.Infectious + Still.Infectious), 
               Susceptible*Incubating.infectious)[-No.susceptibles, ]
    Y <- Incubating[-No.susceptibles]
  }else{
    X <- cbind(Susceptible, Susceptible*(New.Infectious + Still.Infectious), 
               Susceptible*Incubating.infectious)
    Y <- Incubating
  }
  
  if(sum(Y)<=1){
    estimates <- t(t(rep(NA, 3)))
  }else{
    
    if(all(X[, 3]==0)){
      X <- X[, -3]
      obj <- function(b) -sum(dpois(Y, X %*% b, log = TRUE))
      st <- coef(lm(Y ~ X + 0))
      opt <- optim(pmax(st, 0.5), obj, lower = c(1e-3, 1e-3), method = "L-BFGS-B")
      
      estimates <- rbind(t(t(unname(opt$par))), NA)
      
    }else{
      obj <- function(b) -sum(dpois(Y, X %*% b, log = TRUE))
      st <- coef(lm(Y ~ X + 0))
      opt <- optim(pmax(st, 0.5), obj, lower = c(1e-3, 1e-3, 1e-3), method = "L-BFGS-B")
      
      estimates <- t(t(unname(opt$par)))
    }
    
  }
  
 
  rownames(estimates) <- c("Background Infection Rate", "Side Room Infection Rate", "Open Bay Infection Rate")
  
  return(estimates)
  
  
  
}
