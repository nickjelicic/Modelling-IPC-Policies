initial.patient.attributes <- function(infection.status,infectious.states, transfer.rates, discharge.rates, recovery.rates,
                                        discharge.sd=c(1, 1), 
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
  
  latent.period <- matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  incubation.period <- matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  early.symptomatic.period <- matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  late.symptomatic.period <- matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  transfer.period <- matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  discharge.period <- matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  
  
  occupied.beds <- which(infection.status!="No Bed")
  n.occupied.beds <- length(occupied.beds)
  
  if(n.occupied.beds>0){
    
    if(recovery.rates[1]>0){
      if(latent.distribution=="Exponential"){
        latent.period[occupied.beds] <- rexp(n.occupied.beds, recovery.rates[1])
      }else if(latent.distribution=="Gamma"){
        latent.period[occupied.beds] <- rgamma(n.occupied.beds, shape=shape.latent, scale=scale.latent)
      }
    }
    
    if(recovery.rates[2]>0){
      incubation.period[occupied.beds] <- rexp(n.occupied.beds, recovery.rates[2])
    }
    
    if(length(recovery.rates)>2){
      early.symptomatic.period[occupied.beds] <- rexp(n.occupied.beds, recovery.rates[3])
    }
    
    if(length(recovery.rates)>3){
      late.symptomatic.period[occupied.beds] <- rexp(n.occupied.beds, recovery.rates[4])
    }
    
    if(transfer.rates[1]>0){
      if(LOS.distribution=="Exponential"){
        transfer.period[occupied.beds] <- rexp(n.occupied.beds, transfer.rates[1])
      }else if(LOS.distribution=="Lognormal"){
        transfer.period[occupied.beds] <- rlnorm(n.occupied.beds, mu.ward, sigma.ward)
      }else if(LOS.distribution=="Gamma"){
        transfer.period[occupied.beds] <- rgamma(n.occupied.beds, shape.ward, scale.ward)
      }
    }
    
    if(discharge.rates[1]>0){
      if(LOS.distribution=="Exponential"){
        discharge.period[occupied.beds] <- rexp(n.occupied.beds, discharge.rates[1])
      }else if(LOS.distribution=="Lognormal"){
        discharge.period[occupied.beds] <- rlnorm(n.occupied.beds, mu.ward.discharge, sigma.ward.discharge)
      }else if(LOS.distribution=="Gamma"){
        discharge.period[occupied.beds] <- rgamma(n.occupied.beds, shape.ward.discharge, scale.ward.discharge)
      }
    }
    
  }

  initial.patient.attributes <- list(transfer.period, discharge.period, latent.period, incubation.period, early.symptomatic.period, late.symptomatic.period)
  
  
  
  transfer.times <- transfer.period
  discharge.times <- discharge.period
  
  latent.times <- latent.period
  latent.times[which(infection.status!="Latent")] <- Inf
  
  incubation.times <- incubation.period
  incubation.times[which(infection.status!=infectious.states[1])] <- Inf
  
  early.symptomatic.times <- early.symptomatic.period
  if(length(infectious.states)>1){
    early.sympatomatic.times[which(infection.status!=infectious.states[2])] <- Inf
  }
  
  late.symptomatic.times <- late.symptomatic.period
  if(length(infectious.states)>2){
    late.sympatomatic.times[which(infection.status!=infectious.states[3])] <- Inf
  }
  

  
  
  event.times <- list(transfer.times, discharge.times,  latent.times, incubation.times, early.symptomatic.times, late.symptomatic.times)
  
  return(list(initial.patient.attributes, event.times))
}