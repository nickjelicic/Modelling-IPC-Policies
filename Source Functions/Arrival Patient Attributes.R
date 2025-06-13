arrival.patient.attributes <- function(end.time, arrival.rate, transfer.rates, discharge.rates, recovery.rates, discharge.sd=c(1, 1), 
                                       transfer.sd=c(1, 1), latent.sd=1, LOS.distribution="Exponential", latent.distribution="Exponential"){
  
  
  mu.ward <- log((1/transfer.rates[1]^2)/sqrt(transfer.sd[1]^2 + 1/transfer.rates[1]^2))
  sigma.ward <- sqrt(log((transfer.sd[1]*transfer.rates[1])^2 + 1))
  scale.ward <- transfer.sd[1]^2 * transfer.rates[1]
  shape.ward <- 1/(transfer.rates[1]^2 * transfer.sd[1]^2)
  
  mu.ward.infectious <- log((1/transfer.rates[2]^2)/sqrt(transfer.sd[2]^2 + 1/transfer.rates[2]^2))
  sigma.ward.infectious <- sqrt(log((transfer.sd[2]*transfer.rates[2])^2 + 1))
  scale.ward.infectious <- transfer.sd[2]^2 * transfer.rates[2]
  shape.ward.infectious <- 1/(transfer.rates[2]^2 * transfer.sd[2]^2)
  
  mu.ward.discharge <- log((1/discharge.rates[1]^2)/sqrt(discharge.sd[1]^2 + 1/discharge.rates[1]^2))
  sigma.ward.discharge <- sqrt(log((discharge.sd[1]*discharge.rates[1])^2 + 1))
  scale.ward.discharge <- discharge.sd[1]^2 * discharge.rates[1]
  shape.ward.discharge <- 1/(discharge.rates[1]^2 * discharge.sd[1]^2)
  
  mu.ward.discharge.infectious <- log((1/discharge.rates[2]^2)/sqrt(discharge.sd[2]^2 + 1/discharge.rates[2]^2))
  sigma.ward.discharge.infectious <- sqrt(log((discharge.sd[2]*discharge.rates[2])^2 + 1))
  scale.ward.discharge.infectious <- discharge.sd[2]^2 * discharge.rates[2]
  shape.ward.discharge.infectious <- 1/(discharge.rates[2]^2 * discharge.sd[2]^2)
  
  
  scale.latent <- latent.sd^2 * recovery.rates[1]
  shape.latent <- 1/(recovery.rates[1]^2 * latent.sd^2)
  
  T <- 0
  scheduled.arrival.times <- NULL
  
  while(T <= end.time){
    new.arrival.time <- T + rexp(1, arrival.rate)
    
    if(new.arrival.time<=end.time){
      scheduled.arrival.times <- c(scheduled.arrival.times, new.arrival.time)
    }
    T <- new.arrival.time
  }
  
  n.arrivals <- length(scheduled.arrival.times)
  
  if(n.arrivals>0){
    if(recovery.rates[1]>0){
      if(latent.distribution=="Exponential"){
        arrival.latent.period <- rexp(n.arrivals, recovery.rates[1])
      }else if(latent.distribution=="Gamma"){
        arrival.latent.period <- rgamma(n.arrivals, shape=shape.latent, scale=scale.latent)
      }
    }else{
      arrival.latent.period <- rep(Inf, n.arrivals)
    }
    
    
    if(recovery.rates[2]>0){
      arrival.incubation.period <- rexp(n.arrivals, recovery.rates[2])
    }else{
      arrival.incubation.period <- rep(Inf, n.arrivals)
    }
    
    if(length(recovery.rates)>2){
      arrival.early.symptomatic.period <- rexp(n.arrivals, recovery.rates[3])
    }else{
      arrival.early.symptomatic.period <- rep(Inf, n.arrivals)
    }
    
    if(length(recovery.rates)>3){
      arrival.late.symptomatic.period <- rexp(n.arrivals, recovery.rates[4])
    }else{
      arrival.late.symptomatic.period <- rep(Inf, n.arrivals)
    }
    
    
    if(transfer.rates[1]>0){
      if(LOS.distribution=="Exponential"){
        arrival.transfer.times <- rexp(n.arrivals, transfer.rates[1])
      }else if(LOS.distribution=="Lognormal"){
        arrival.transfer.times <- rlnorm(n.arrivals, mu.ward, sigma.ward)
      }else if(LOS.distribution=="Gamma"){
        arrival.transfer.times <- rgamma(n.arrivals, shape=shape.ward, scale=scale.ward)
      }
    }else{
      arrival.transfer.times <- rep(Inf, n.arrivals)
    }
    
    if(discharge.rates[1]>0){
      if(LOS.distribution=="Exponential"){
        arrival.discharge.times <- rexp(n.arrivals, discharge.rates[1])
      }else if(LOS.distribution=="Lognormal"){
        arrival.discharge.times <- rlnorm(n.arrivals, mu.ward.discharge, sigma.ward.discharge)
      }else if(LOS.distribution=="Gamma"){
        arrival.discharge.times <- rgamma(n.arrivals, shape=shape.ward.discharge, scale.ward.discharge)
      }
    }else{
      arrival.discharge.times <- rep(Inf, n.arrivals)
    }
    
    
  }else{
    arrival.latent.period <- NULL
    arrival.incubation.period <- NULL
    arrival.early.symptomatic.period <- NULL
    arrival.late.symptomatic.period <- NULL
    arrival.transfer.times <- NULL
    arrival.discharge.times <- NULL
  }
  
  return(list(scheduled.arrival.times, arrival.transfer.times, arrival.discharge.times, arrival.latent.period, arrival.incubation.period,
              arrival.early.symptomatic.period, arrival.late.symptomatic.period))
}