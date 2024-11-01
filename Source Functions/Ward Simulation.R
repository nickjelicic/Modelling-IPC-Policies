ward.simulation <- function(focus.bay, ward.size, infectious.states,arrival.rate, discharge.rates, transfer.rates,
                           infection.rates, recovery.rates, time.seq, 
                           infection.status,
                           LOS.distribution, discharge.sd, transfer.sd, scheduled.arrival.times=NULL, return.metrics,
                           restrictions=FALSE, quarantine.time=0, close.bay.coef=0, spread.to.focus.bay=FALSE, queue.departures=TRUE, 
                           latent.distribution="Exponential", latent.sd=1){
  ## Initial Conditions
  #####
  ## 
  if(restrictions==TRUE){
    end.restriction.time <- quarantine.time
    
    unrestricted.infection.rate <- infection.rates[4, ]
    infection.rates[4, ] <- infection.rates[4, ]*close.bay.coef
    
  }else{
    end.restriction.time <- Inf
  }
  
  infection.states <- c("Empty", "Susceptible", "Latent", infectious.states, "Recovered")
  T <- 0
  focus.bay.infections <- 0
  other.bay.infections <- 0
  new.arrivals.admitted <- 0
  blocked.arrivals <- 0
  unused_bed.days.focus.bay <- 0
  unused_bed.days.other.bays <- 0
  infectious.discharged <- 0
  infectious.transferred <- 0
  infected.discharged <- 0
  infected.transferred <- 0
  days.discharges.delayed <- 0
  discharges <- 0
  transfers <- 0
  ward.occupancy <- 0
  outbreak <- FALSE
  
  time.seq <- sort(time.seq, decreasing=FALSE)
  end.time <- time.seq[1]
  
  Metrics <- NULL
  
  bay.occupancy <- length(which(infection.status!="Empty"))
  
  Finish <- FALSE
  
  
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
  
  n.bay <- nrow(infection.status)

  if(arrival.rate>0){
    arrival.times <- rexp(1, arrival.rate)
  }else{
    arrival.times <- Inf
  }
  
  
  
  infection.times <- matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  latent.times <- matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  early.symptomatic.times <- matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  late.symptomatic.times <-matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  transfer.times <-  matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  discharge.times <- matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  incubation.times <- matrix(Inf, nrow=nrow(infection.status), ncol=ncol(infection.status))
  

  
  for(bay in 2:n.bay){
    if(bay==focus.bay & spread.to.focus.bay==FALSE){
      infection.hazard <- infection.rates[1]
      for(k in 1:length(infectious.states)){
        infection.hazard <- infection.hazard + infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k]))
      }
        
    }else{
      infection.hazard <- infection.rates[1]
      
      for(k in 1:length(infectious.states)){
        infection.hazard <- infection.hazard + infection.rates[2, k]*length(which(infection.status[1, ]==infectious.states[k])) + 
          infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k])) + 
          infection.rates[4, k]*length(which(infection.status[-c(1, bay), ]==infectious.states[k]))
      }
      
    }
    infection.hazard <- infection.hazard/length(which(!infection.status%in%c("Empty", "No Bed")))
    
    susceptibles <- which(infection.status[bay, ]=="Susceptible")
    if(length(susceptibles)>0 & infection.hazard>0){
      infection.times[bay, susceptibles] <- T + rexp(length(susceptibles), infection.hazard)
    }
  }
  
  
  ####
  ## departure times from the bay
  infected <- which(!infection.status%in%c("No Bed", "Empty", "Susceptible", "Recovered"))
  not.infected <- which(infection.status=="Susceptible" | infection.status=="Recovered")
  if(transfer.rates[1]>0){
    if(length(not.infected)>0){
      
      if(LOS.distribution=="Exponential"){
        transfer.times[not.infected] <- T + rexp(length(not.infected), transfer.rates[1])
      }else if(LOS.distribution=="Lognormal"){
        
        transfer.times[not.infected] <- T +rlnorm(length(not.infected), mu.ward, sigma.ward)
      }else if(LOS.distribution=="Gamma"){
        
        transfer.times[not.infected] <- T + rgamma(length(not.infected), shape=shape.ward, scale=scale.ward)
      }
      
    }
  }
  if(transfer.rates[2]>0){
    if(length(infected)>0){
      
      if(LOS.distribution=="Exponential"){
        transfer.times[infected] <- T + rexp(length(infected), transfer.rates[2])
      }else if(LOS.distribution=="Lognormal"){
        
        transfer.times[infected] <- T + rlnorm(length(infected), mu.ward.infectious, sigma.ward.infectious)
      }else if(LOS.distribution=="Gamma"){
        
        transfer.times[infected] <- T + rgamma(length(infected), shape=shape.ward.infectious, scale=scale.ward.infectious)
      }
      
      
    }
  }
  
  if(discharge.rates[1]>0){
    if(length(not.infected)>0){
      
      if(LOS.distribution=="Exponential"){
        discharge.times[not.infected] <- T + rexp(length(not.infected), discharge.rates[1])
      }else if(LOS.distribution=="Lognormal"){
        
        discharge.times[not.infected] <- T + rlnorm(length(not.infected), mu.ward.discharge, sigma.ward.discharge)
      }else if(LOS.distribution=="Gamma"){
        
        discharge.times[not.infected] <- T + rgamma(length(not.infected), shape=shape.ward.discharge, scale=scale.ward.discharge)
      }
      
    }
  }
  if(discharge.rates[2]>0){
    if(length(infected)>0){
      
      if(LOS.distribution=="Exponential"){
        discharge.times[infected] <- T + rexp(length(infected), discharge.rates[2])
      }else if(LOS.distribution=="Lognormal"){
        
        discharge.times[infected] <- T + rlnorm(length(infected), mu.ward.discharge.infectious, sigma.ward.discharge.infectious)
      }else if(LOS.distribution=="Gamma"){
        
        discharge.times[infected] <- T + rgamma(length(infected), shape=shape.ward.discharge.infectious, scale=scale.ward.discharge.infectious)
      }
      
      
    }
  }
  
  
  ## recovery times
  latents <- which(infection.status=="Latent")
  if(length(latents)>0 & recovery.rates[1]>0){
    if(latent.distribution=="Exponential"){
      latent.times[latents] <- T + rexp(length(latents), recovery.rates[1])
    }else if(latent.distribution=="Gamma"){
      latent.times[latents] <- T + rgamma(length(latents), shape=shape.latent, scale=scale.latent)
    }
  }
  infectious <- which(infection.status==infectious.states[1])
  if(length(infectious)>0 & recovery.rates[2]>0){
    incubation.times[infectious] <- rexp(length(infectious), recovery.rates[2])
  }
  if(length(infectious.states)>1){
    early.symptomatic <- which(infection.status==infectious.states[2])
    if(length(early.symptomatic)>0 & recovery.rates[3]>0){
      early.symptomatic.times[early.symptomatic] <- rexp(length(early.symptomatic), recovery.rates[3])
    }
  }
  if(length(infectious.states)>2){
     late.symptomatic <- which(infection.status==infectious.states[3])
    if(length(late.symptomatic)>0 & recovery.rates[4]>0){
      late.symptomatic.times[early.symptomatic] <- rexp(length(late.symptomatic), recovery.rates[4])
    }
  }
  
  
  #####
  ##simulate bay
  
  
  while(Finish==FALSE){
    
    if(length(scheduled.arrival.times)>0){
      first.arrival.time <- min(arrival.times, scheduled.arrival.times)
    }else{
      first.arrival.time <- min(arrival.times)
    }
    
    first.infection.time <- min(infection.times)
    first.transfer.time <- min(transfer.times)
    first.discharge.time <- min(discharge.times)
    
    first.latent.time <- min(latent.times)
    first.incubation.time <- min(incubation.times)
    
    
    first.early.symptomatic.time <- min(early.symptomatic.times)
    first.late.symptomatic.time <- min(late.symptomatic.times)
    
    
    event.time <- min(first.arrival.time, first.infection.time, first.transfer.time, first.discharge.time,
                      first.latent.time, first.incubation.time, 
                      first.early.symptomatic.time, first.late.symptomatic.time, end.restriction.time)
    
    empty.beds.focus.bay <- length(which(infection.status[focus.bay, ]=="Empty"))
    empty.beds.other.bays <- length(which(infection.status[-focus.bay, ]=="Empty"))
    
    if(event.time > end.time){
      time.seq <- time.seq[-1]
      
      unused_bed.days.focus.bay <- unused_bed.days.focus.bay + (end.time-T) * empty.beds.focus.bay
      unused_bed.days.other.bays <- unused_bed.days.other.bays + (end.time-T) * empty.beds.other.bays
      
      T <- end.time
      
      if(length(time.seq)>0){
        end.time <- time.seq[1]
      }else{
        Finish <- TRUE
      }
      
      
      
      metrics <- c(focus.bay.infections, other.bay.infections, new.arrivals.admitted, blocked.arrivals, unused_bed.days.focus.bay, unused_bed.days.other.bays, outbreak,infected.discharged, infected.transferred, days.discharges.delayed, discharges, transfers)
      
      Metrics <- rbind(Metrics, metrics)
      
    }else{
      
      unused_bed.days.focus.bay <- unused_bed.days.focus.bay + (event.time - T) * empty.beds.focus.bay
      unused_bed.days.other.bays <- unused_bed.days.other.bays + (event.time - T) * empty.beds.other.bays
      
      T <- event.time
      
      if(event.time==first.arrival.time){
        
        ## Arrivals should be placed in other beds on the ward if available
        empty.beds.focus.bay <- which(infection.status[focus.bay, ]=="Empty")
        empty.beds.other.bays <- which(infection.status[-c(1, focus.bay), ]=="Empty")
        empty.side.rooms <- which(infection.status[1, ]=="Empty")
        
        ## Priority order of placing new patients (if occupied move to next one)
        # 1. Other open bay
        # 2. Focus bay
        # 3. Side Room
        
        if(length(empty.beds.other.bays)>0){
          ward.occupancy <- ward.occupancy + 1
          new.arrivals.admitted <- new.arrivals.admitted + 1
          # choose one of these beds at random to place the new patient in 
          if(length(empty.beds.other.bays)>1){
            new.patient.location <- sample(empty.beds.other.bays, 1)
          }else{
            new.patient.location <- empty.beds.other.bays
          }
          
          infection.status[-c(1, focus.bay), ][new.patient.location] <- "Susceptible"
          
          if(transfer.rates[1]>0){
              
              if(LOS.distribution=="Exponential"){
                transfer.times[-c(1, focus.bay), ][new.patient.location] <- T + rexp(1, transfer.rates[1])
              }else if(LOS.distribution=="Lognormal"){
                
                transfer.times[-c(1, focus.bay), ][new.patient.location] <- T +rlnorm(1, mu.ward, sigma.ward)
              }else if(LOS.distribution=="Gamma"){
                
                transfer.times[-c(1, focus.bay), ][new.patient.location] <- T + rgamma(1, shape=shape.ward, scale=scale.ward)
              }
              
          }
          
          if(discharge.rates[1]>0){
              
              if(LOS.distribution=="Exponential"){
                discharge.times[-c(1, focus.bay), ][new.patient.location] <- T + rexp(1, discharge.rates[1])
              }else if(LOS.distribution=="Lognormal"){
                
                discharge.times[-c(1, focus.bay), ][new.patient.location] <- T + rlnorm(1, mu.ward.discharge, sigma.ward.discharge)
              }else if(LOS.distribution=="Gamma"){
                
                discharge.times[-c(1, focus.bay), ][new.patient.location] <- T + rgamma(1, shape=shape.ward.discharge, scale=scale.ward.discharge)
              }
              
          }
          
        }else if(length(empty.beds.focus.bay)>0 & restrictions==FALSE){
          ward.occupancy <- ward.occupancy + 1
          new.arrivals.admitted <- new.arrivals.admitted + 1
          
          if(length(empty.beds.focus.bay)>1){
            new.patient.location <- sample(empty.beds.focus.bay, 1)
          }else{
            new.patient.location <- empty.beds.focus.bay
          }
          
          infection.status[focus.bay, new.patient.location] <- "Susceptible"
          
          if(transfer.rates[1]>0){
              
              if(LOS.distribution=="Exponential"){
                transfer.times[focus.bay, new.patient.location] <- T + rexp(1, transfer.rates[1])
              }else if(LOS.distribution=="Lognormal"){
                
                transfer.times[focus.bay, new.patient.location] <- T +rlnorm(1, mu.ward, sigma.ward)
              }else if(LOS.distribution=="Gamma"){
                
                transfer.times[focus.bay, new.patient.location] <- T + rgamma(1, shape=shape.ward, scale=scale.ward)
              }
              
          }
          
          if(discharge.rates[1]>0){
              
              if(LOS.distribution=="Exponential"){
                discharge.times[focus.bay, new.patient.location] <- T + rexp(1, discharge.rates[1])
              }else if(LOS.distribution=="Lognormal"){
                
                discharge.times[focus.bay, new.patient.location] <- T + rlnorm(1, mu.ward.discharge, sigma.ward.discharge)
              }else if(LOS.distribution=="Gamma"){
                
                discharge.times[focus.bay, new.patient.location] <- T + rgamma(1, shape=shape.ward.discharge, scale=scale.ward.discharge)
              }
              
          }
          
        }else if(length(empty.side.rooms)>0){
          ward.occupancy <- ward.occupancy + 1
          new.arrivals.admitted <- new.arrivals.admitted + 1
          
          if(length(empty.side.rooms)>1){
            new.patient.location <- sample(empty.side.rooms, 1)
          }else{
            new.patient.location <- empty.side.rooms
          }
          
          infection.status[1, new.patient.location] <- "Susceptible"
          
          if(transfer.rates[1]>0){
              
              if(LOS.distribution=="Exponential"){
                transfer.times[1, new.patient.location] <- T + rexp(1, transfer.rates[1])
              }else if(LOS.distribution=="Lognormal"){
                
                transfer.times[1, new.patient.location] <- T +rlnorm(1, mu.ward, sigma.ward)
              }else if(LOS.distribution=="Gamma"){
                
                transfer.times[1, new.patient.location] <- T + rgamma(1, shape=shape.ward, scale=scale.ward)
              }
              
          }
          
          if(discharge.rates[1]>0){
              
              if(LOS.distribution=="Exponential"){
                discharge.times[1, new.patient.location] <- T + rexp(1, discharge.rates[1])
              }else if(LOS.distribution=="Lognormal"){
                
                discharge.times[1, new.patient.location] <- T + rlnorm(1, mu.ward.discharge, sigma.ward.discharge)
              }else if(LOS.distribution=="Gamma"){
                
                discharge.times[1, new.patient.location] <- T + rgamma(1, shape=shape.ward.discharge, scale=scale.ward.discharge)
              }
              
          }
          
        }else{
          ## arrival is blocked (no free beds)
          blocked.arrivals <- blocked.arrivals + 1
        }
        
        ward.occupancy <- length(which(infection.status %in% infection.states[-1]))
        
        for(bay in 2:n.bay){
          if(bay==focus.bay & spread.to.focus.bay==FALSE){
            infection.hazard <- infection.rates[1]
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k]))
            }
            
          }else{
            infection.hazard <- infection.rates[1]
            
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[2, k]*length(which(infection.status[1, ]==infectious.states[k])) + 
                infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k])) + 
                infection.rates[4, k]*length(which(infection.status[-c(1, bay), ]==infectious.states[k]))
            }
            
          }
          infection.hazard <- infection.hazard/length(which(!infection.status%in%c("Empty", "No Bed")))
          
          susceptibles <- which(infection.status[bay, ]=="Susceptible")
          if(length(susceptibles)>0 & infection.hazard>0){
            infection.times[bay, susceptibles] <- T + rexp(length(susceptibles), infection.hazard)
          }
        }
        
        
        if(arrival.rate>0){
          arrival.times <- T + rexp(1, arrival.rate)
        }
        
        
        if(event.time%in%scheduled.arrival.times){
          scheduled.arrival.times <- scheduled.arrival.times[-which(scheduled.arrival.times==event.time)[1]]
        }
        
      }
      ## Infection
      if(event.time==first.infection.time){
        
       
        
        infection <- which.min(infection.times)
        
        ## record subsequent infection if in focus bay
        if(infection%%nrow(infection.status)!=focus.bay){
          outbreak <- TRUE
          other.bay.infections <- other.bay.infections + 1
        }else{
          focus.bay.infections <- focus.bay.infections + 1
        }
        
        infection.status[infection] <- "Latent"
        infection.times[infection] <- Inf
        
        if(transfer.rates[2]>0){
            
            if(LOS.distribution=="Exponential"){
              transfer.times[infection] <- T + rexp(1, transfer.rates[2])
            }else if(LOS.distribution=="Lognormal"){
              
              transfer.times[infection] <- max(T, transfer.times[infection] + rlnorm(1, mu.ward.infectious, sigma.ward.infectious) - rlnorm(1, mu.ward, sigma.ward))
            }else if(LOS.distribution=="Gamma"){
              
              transfer.times[infection] <- T + rgamma(1, shape=shape.ward.infectious, scale=scale.ward.infectious)
            }
            
        }else{
          transfer.times[infection] <- Inf
        }
        if(discharge.rates[2]>0){
            
            if(LOS.distribution=="Exponential"){
              discharge.times[infection] <- T + rexp(1, discharge.rates[2])
            }else if(LOS.distribution=="Lognormal"){
              
              discharge.times[infection] <- max(T, discharge.times[infection] + rlnorm(1, mu.ward.discharge.infectious, sigma.ward.discharge.infectious)-rlnorm(1, mu.ward.discharge, sigma.ward.discharge))
            }else if(LOS.distribution=="Gamma"){
              
              discharge.times[infection] <- T + rgamma(1, shape=shape.ward.discharge.infectious, scale=scale.ward.discharge.infectious)
            }
            
        }else{
          discharge.times[infection] <- Inf
        }
        
        
        if(recovery.rates[1]>0){
          if(latent.distribution=="Exponential"){
            latent.times[infection] <- T + rexp(1, recovery.rates[1])
          }else if(latent.distribution=="Gamma"){
            latent.times[infection] <- T + rgamma(1, shape=shape.latent, scale=scale.latent)
          }
          
        }else{
          latent.times[infection] <- Inf
        }
       
      }
      ## Departure from the bay
      if(event.time==first.discharge.time){
        
        discharge <- which.min(discharge.times)
        
        if((infection.status[discharge]%in%infectious.states) & ((discharge%%n.bay)==focus.bay)){
          infectious.discharged <- infectious.discharged + 1
        }
        if((infection.status[discharge]%in%c("Latent",infectious.states)) & ((discharge%%n.bay)==focus.bay)){
          infected.discharged <- infected.discharged + 1
        }
        discharges <- discharges + 1
        
        infection.status[discharge] <- "Empty"
        infection.times[discharge] <- Inf
        
        ward.occupancy <- length(which(infection.status %in% infection.states[-1]))
        
        for(bay in 2:n.bay){
          if(bay==focus.bay & spread.to.focus.bay==FALSE){
            infection.hazard <- infection.rates[1]
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k]))
            }
            
          }else{
            infection.hazard <- infection.rates[1]
            
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[2, k]*length(which(infection.status[1, ]==infectious.states[k])) + 
                infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k])) + 
                infection.rates[4, k]*length(which(infection.status[-c(1, bay), ]==infectious.states[k]))
            }
            
          }
          infection.hazard <- infection.hazard/length(which(!infection.status%in%c("Empty", "No Bed")))
          
          susceptibles <- which(infection.status[bay, ]=="Susceptible")
          if(length(susceptibles)>0 & infection.hazard>0){
            infection.times[bay, susceptibles] <- T + rexp(length(susceptibles), infection.hazard)
          }
        }
        
        
        discharge.times[discharge] <- Inf
        transfer.times[discharge] <- Inf
        latent.times[discharge] <- Inf
        incubation.times[discharge] <- Inf
        early.symptomatic.times[discharge] <- Inf
        late.symptomatic.times[discharge] <- Inf
        
      }
      if(event.time==first.transfer.time){
        
        transfer <- which.min(transfer.times)
        
        if(transfer%%n.bay==focus.bay & restrictions==TRUE){
          if(queue.departures==TRUE){
            transfer.times[transfer] <- quarantine.time
            days.discharges.delayed <- days.discharges.delayed + (quarantine.time - T)
          }else{
            transfer.times[transfer] <- Inf
          }
          
        }else{
          if((infection.status[transfer]%in%infectious.states) & ((transfer%%n.bay)==focus.bay)){
            infectious.transferred <- infectious.transferred + 1
          }
          if((infection.status[transfer]%in%c("Latent", infectious.states)) & ((transfer%%n.bay)==focus.bay)){
            infected.transferred <- infected.transferred + 1
          }
          transfers <- transfers + 1
          
          infection.status[transfer] <- "Empty"
          infection.times[transfer] <- Inf
          
          ward.occupancy <- length(which(infection.status %in% infection.states[-1]))
          
          for(bay in 2:n.bay){
            if(bay==focus.bay & spread.to.focus.bay==FALSE){
              infection.hazard <- infection.rates[1]
              for(k in 1:length(infectious.states)){
                infection.hazard <- infection.hazard + infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k]))
              }
              
            }else{
              infection.hazard <- infection.rates[1]
              
              for(k in 1:length(infectious.states)){
                infection.hazard <- infection.hazard + infection.rates[2, k]*length(which(infection.status[1, ]==infectious.states[k])) + 
                  infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k])) + 
                  infection.rates[4, k]*length(which(infection.status[-c(1, bay), ]==infectious.states[k]))
              }
              
            }
            infection.hazard <- infection.hazard/length(which(!infection.status%in%c("Empty", "No Bed")))
            
            susceptibles <- which(infection.status[bay, ]=="Susceptible")
            if(length(susceptibles)>0 & infection.hazard>0){
              infection.times[bay, susceptibles] <- T + rexp(length(susceptibles), infection.hazard)
            }
          }
          
          
          discharge.times[transfer] <- Inf
          transfer.times[transfer] <- Inf
          latent.times[transfer] <- Inf
          incubation.times[transfer] <- Inf
          early.symptomatic.times[transfer] <- Inf
          late.symptomatic.times[transfer] <- Inf
        }
        
        
        
        
      }
      if(event.time==first.latent.time){
        
        infectious <- which.min(latent.times)
        
        infection.status[infectious] <- infectious.states[1]
        
        ward.occupancy <- length(which(infection.status %in% infection.states[-1]))
        
        for(bay in 2:n.bay){
          if(bay==focus.bay & spread.to.focus.bay==FALSE){
            infection.hazard <- infection.rates[1]
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k]))
            }
            
          }else{
            infection.hazard <- infection.rates[1]
            
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[2, k]*length(which(infection.status[1, ]==infectious.states[k])) + 
                infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k])) + 
                infection.rates[4, k]*length(which(infection.status[-c(1, bay), ]==infectious.states[k]))
            }
            
          }
          infection.hazard <- infection.hazard/length(which(!infection.status%in%c("Empty", "No Bed")))
          
          susceptibles <- which(infection.status[bay, ]=="Susceptible")
          if(length(susceptibles)>0 & infection.hazard>0){
            infection.times[bay, susceptibles] <- T + rexp(length(susceptibles), infection.hazard)
          }
        }
        
        
        latent.times[infectious] <- Inf
        if(recovery.rates[2]>0){
          incubation.times[infectious] <- T + rexp(1, recovery.rates[2])
        }else{
          incubation.times[infectious] <- Inf
        }
        
        
      }
      
      
      if(event.time==first.incubation.time){
        
        incubation <- which.min(incubation.times)
        
        infection.status[incubation] <- infection.states[which(infection.states==infectious.states[1]) + 1] 
        
        
        ward.occupancy <- length(which(infection.status %in% infection.states[-1]))
        
        for(bay in 2:n.bay){
          if(bay==focus.bay & spread.to.focus.bay==FALSE){
            infection.hazard <- infection.rates[1]
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k]))
            }
            
          }else{
            infection.hazard <- infection.rates[1]
            
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[2, k]*length(which(infection.status[1, ]==infectious.states[k])) + 
                infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k])) + 
                infection.rates[4, k]*length(which(infection.status[-c(1, bay), ]==infectious.states[k]))
            }
            
          }
          infection.hazard <- infection.hazard/length(which(!infection.status%in%c("Empty", "No Bed")))
          
          susceptibles <- which(infection.status[bay, ]=="Susceptible")
          if(length(susceptibles)>0 & infection.hazard>0){
            infection.times[bay, susceptibles] <- T + rexp(length(susceptibles), infection.hazard)
          }
        }
        
        incubation.times[incubation] <- Inf
        
        
        if(length(infection.states)>(which(infection.states==infectious.states[1]) + 1) & recovery.rates[3]>0){
          early.symptomatic.times[incubation] <- T + rexp(1, recovery.rates[3])
        }else{
          early.symptomatic.times[incubation] <- Inf
        }
        
        if(length(infectious.states)==1){
          if(transfer.rates[1]>0){
              
              if(LOS.distribution=="Exponential"){
                transfer.times[incubation] <- T + rexp(1, transfer.rates[1])
              }else if(LOS.distribution=="Lognormal"){
                
                transfer.times[incubation] <- max(T, transfer.times[incubation] +rlnorm(1, mu.ward, sigma.ward)-rlnorm(1, mu.ward.infectious, sigma.ward.infectious))
              }else if(LOS.distribution=="Gamma"){
                
                transfer.times[incubation] <- T + rgamma(1, shape=shape.ward, scale=scale.ward)
              }
              
            }
          
          if(discharge.rates[1]>0){
              
              if(LOS.distribution=="Exponential"){
                discharge.times[incubation] <- T + rexp(1, discharge.rates[1])
              }else if(LOS.distribution=="Lognormal"){
                
                discharge.times[incubation] <- max(T, discharge.times[incubation] + rlnorm(1, mu.ward.discharge, sigma.ward.discharge)-rlnorm(1, mu.ward.discharge.infectious, sigma.ward.discharge.infectious))
              }else if(LOS.distribution=="Gamma"){
                
                discharge.times[incubation] <- T + rgamma(1, shape=shape.ward.discharge, scale=scale.ward.discharge)
              }
              
            }
          }
        
      }
      
      if(event.time==first.early.symptomatic.time){
        
        early.symptomatic <- which.min(early.symptomatic.times)
        
        infection.status[early.symptomatic] <- infection.states[which(infection.states==infectious.states[1]) + 2] 
        
        ward.occupancy <- length(which(infection.status %in% infection.states[-1]))
        
        for(bay in 2:n.bay){
          if(bay==focus.bay & spread.to.focus.bay==FALSE){
            infection.hazard <- infection.rates[1]
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k]))
            }
            
          }else{
            infection.hazard <- infection.rates[1]
            
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[2, k]*length(which(infection.status[1, ]==infectious.states[k])) + 
                infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k])) + 
                infection.rates[4, k]*length(which(infection.status[-c(1, bay), ]==infectious.states[k]))
            }
            
          }
          infection.hazard <- infection.hazard/length(which(!infection.status%in%c("Empty", "No Bed")))
          
          susceptibles <- which(infection.status[bay, ]=="Susceptible")
          if(length(susceptibles)>0 & infection.hazard>0){
            infection.times[bay, susceptibles] <- T + rexp(length(susceptibles), infection.hazard)
          }
        }
        
        early.symptomatic.times[early.symptomatic] <- Inf
        if((length(infection.states)>which(infection.states==infectious.states[1]) + 2)&recovery.rates[4]>0){
          late.symptomatic.times[early.symptomatic] <- T + rexp(1, recovery.rates[4])
        }else{
          late.symptomatic.times[early.symptomatic] <- Inf
        }
        
        if(length(infectious.states)==2){
            if(transfer.rates[1]>0){
                
                if(LOS.distribution=="Exponential"){
                  transfer.times[early.symptomatic] <- T + rexp(1, transfer.rates[1])
                }else if(LOS.distribution=="Lognormal"){
                  
                  transfer.times[early.symptomatic] <- T +rlnorm(1, mu.ward, sigma.ward)
                }else if(LOS.distribution=="Gamma"){
                  
                  transfer.times[early.symptomatic] <- T + rgamma(1, shape=shape.ward, scale=scale.ward)
                }
                
            }
            
            if(discharge.rates[1]>0){
                
                if(LOS.distribution=="Exponential"){
                  discharge.times[early.symptomatic] <- T + rexp(1, discharge.rates[1])
                }else if(LOS.distribution=="Lognormal"){
                  
                  discharge.times[early.symptomatic] <- T + rlnorm(1, mu.ward.discharge, sigma.ward.discharge)
                }else if(LOS.distribution=="Gamma"){
                  
                  discharge.times[early.symptomatic] <- T + rgamma(1, shape=shape.ward.discharge, scale=scale.ward.discharge)
                }
                
            }
        }
        
      }
      
      if(event.time==first.late.symptomatic.time){
        
        recovered <- which.min(late.symptomatic.times)
        
        infection.status[recovered] <- "Recovered"
        
        ward.occupancy <- length(which(infection.status %in% infection.states[-1]))
        
        for(bay in 2:n.bay){
          if(bay==focus.bay & spread.to.focus.bay==FALSE){
            infection.hazard <- infection.rates[1]
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k]))
            }
            
          }else{
            infection.hazard <- infection.rates[1]
            
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[2, k]*length(which(infection.status[1, ]==infectious.states[k])) + 
                infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k])) + 
                infection.rates[4, k]*length(which(infection.status[-c(1, bay), ]==infectious.states[k]))
            }
            
          }
          infection.hazard <- infection.hazard/length(which(!infection.status%in%c("Empty", "No Bed")))
          
          susceptibles <- which(infection.status[bay, ]=="Susceptible")
          if(length(susceptibles)>0 & infection.hazard>0){
            infection.times[bay, susceptibles] <- T + rexp(length(susceptibles), infection.hazard)
          }
        }
        
        if(transfer.rates[1]>0){
            
            if(LOS.distribution=="Exponential"){
              transfer.times[recovered] <- T + rexp(1, transfer.rates[1])
            }else if(LOS.distribution=="Lognormal"){
              
              transfer.times[recovered] <- T +rlnorm(1, mu.ward, sigma.ward)
            }else if(LOS.distribution=="Gamma"){
              
              transfer.times[recovered] <- T + rgamma(1, shape=shape.ward, scale=scale.ward)
            }
            
        }
        
        if(discharge.rates[1]>0){
            
            if(LOS.distribution=="Exponential"){
              discharge.times[recovered] <- T + rexp(1, discharge.rates[1])
            }else if(LOS.distribution=="Lognormal"){
              
              discharge.times[recovered] <- T + rlnorm(1, mu.ward.discharge, sigma.ward.discharge)
            }else if(LOS.distribution=="Gamma"){
              
              discharge.times[recovered] <- T + rgamma(1, shape=shape.ward.discharge, scale=scale.ward.discharge)
            }
            
        }
        
        
        
        late.symptomatic.times[recovered] <- Inf
      }
      
      if(event.time==end.restriction.time){
        restrictions <- FALSE
        end.restriction.time <- Inf
        infection.rates[4, ] <- unrestricted.infection.rate
        
        
        if(queue.departures==FALSE){
          infected <- which(!infection.status%in%c("No Bed", "Empty", "Susceptible", "Recovered"))
          not.infected <- which(infection.status=="Susceptible" | infection.status=="Recovered")
          if(transfer.rates[1]>0){
            if(length(not.infected)>0){
              
              if(LOS.distribution=="Exponential"){
                transfer.times[not.infected] <- T + rexp(length(not.infected), transfer.rates[1])
              }else if(LOS.distribution=="Lognormal"){
                
                transfer.times[not.infected] <- T +rlnorm(length(not.infected), mu.ward, sigma.ward)
              }else if(LOS.distribution=="Gamma"){
                
                transfer.times[not.infected] <- T + rgamma(length(not.infected), shape=shape.ward, scale=scale.ward)
              }
              
            }
          }
          if(transfer.rates[2]>0){
            if(length(infected)>0){
              
              if(LOS.distribution=="Exponential"){
                transfer.times[infected] <- T + rexp(length(infected), transfer.rates[2])
              }else if(LOS.distribution=="Lognormal"){
                
                transfer.times[infected] <- T + rlnorm(length(infected), mu.ward.infectious, sigma.ward.infectious)
              }else if(LOS.distribution=="Gamma"){
                
                transfer.times[infected] <- T + rgamma(length(infected), shape=shape.ward.infectious, scale=scale.ward.infectious)
              }
              
              
            }
          }
        }
        
        for(bay in 2:n.bay){
          if(bay==focus.bay & spread.to.focus.bay==FALSE){
            infection.hazard <- infection.rates[1]
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k]))
            }
            
          }else{
            infection.hazard <- infection.rates[1]
            
            for(k in 1:length(infectious.states)){
              infection.hazard <- infection.hazard + infection.rates[2, k]*length(which(infection.status[1, ]==infectious.states[k])) + 
                infection.rates[3, k]*length(which(infection.status[bay, ]==infectious.states[k])) + 
                infection.rates[4, k]*length(which(infection.status[-c(1, bay), ]==infectious.states[k]))
            }
            
          }
          infection.hazard <- infection.hazard/length(which(!infection.status%in%c("Empty", "No Bed")))
          
          susceptibles <- which(infection.status[bay, ]=="Susceptible")
          if(length(susceptibles)>0 & infection.hazard>0){
            infection.times[bay, susceptibles] <- T + rexp(length(susceptibles), infection.hazard)
          }
        }
        
      }
    }
    
  }
  
  
  if(return.metrics==TRUE){
    return(Metrics)
  }else{
    return(infection.status)
  }
  
}