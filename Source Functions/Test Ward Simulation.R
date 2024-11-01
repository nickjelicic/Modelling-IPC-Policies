ward.simulation.paired.test <- function(focus.bay, infectious.states,
                                   infection.rates, time.seq, 
                                   infection.status, patient.attributes, event.times, infection.times,return.metrics,
                                   restrictions=FALSE, quarantine.time=0, close.bay.coef=0, spread.to.focus.bay=FALSE, queue.departures=TRUE, 
                                   new.patient.attributes){
  ## Initial Conditions
  #####
  
  transfer.period <- patient.attributes[[1]]
  discharge.period <- patient.attributes[[2]]
  latent.period <- patient.attributes[[3]]
  incubation.period <- patient.attributes[[4]]
  early.symptomatic.period <- patient.attributes[[5]]
  late.symptomatic.period <- patient.attributes[[6]]
  
  transfer.times <- event.times[[1]]
  discharge.times <- event.times[[2]]
  latent.times <- event.times[[3]]
  incubation.times <- event.times[[4]]
  early.symptomatic.times <- event.times[[5]]
  late.symptomatic.times <- event.times[[6]]
  
  
  scheduled.arrival.times <- new.patient.attributes[[1]]
  arrival.transfer.times <- new.patient.attributes[[2]]
  arrival.discharge.times <- new.patient.attributes[[3]]
  arrival.latent.period <- new.patient.attributes[[4]]
  arrival.incubation.period <- new.patient.attributes[[5]]
  arrival.early.symptomatic.period <- new.patient.attributes[[6]]
  arrival.late.symptomatic.period <- new.patient.attributes[[7]]
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
  
  n.bay <- nrow(infection.status)
  
  
  #####
  ##simulate bay
  
  
  while(Finish==FALSE){
    
    if(length(scheduled.arrival.times)>0){
      first.arrival.time <- min(scheduled.arrival.times)
    }else{
      first.arrival.time <- Inf
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
      
      
      
      metrics <- c(focus.bay.infections, other.bay.infections, new.arrivals.admitted, blocked.arrivals, unused_bed.days.focus.bay, unused_bed.days.other.bays, outbreak, infectious.discharged,
                   infectious.transferred, infected.discharged, infected.transferred, days.discharges.delayed, discharges, transfers)
      
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
          
          transfer.times[-c(1, focus.bay), ][new.patient.location] <- T + arrival.transfer.times[1]
          discharge.times[-c(1, focus.bay), ][new.patient.location] <- T + arrival.discharge.times[1]
          
          transfer.period[-c(1, focus.bay), ][new.patient.location] <- arrival.transfer.times[1]
          discharge.period[-c(1, focus.bay), ][new.patient.location] <- arrival.discharge.times[1]
          latent.period[-c(1, focus.bay), ][new.patient.location] <- arrival.latent.period[1]
          incubation.period[-c(1, focus.bay), ][new.patient.location] <- arrival.incubation.period[1]
          early.symptomatic.period[-c(1, focus.bay), ][new.patient.location] <- arrival.early.symptomatic.period[1]
          late.symptomatic.period[-c(1, focus.bay), ][new.patient.location] <- arrival.late.symptomatic.period[1]
          
        }else if(length(empty.beds.focus.bay)>0 & restrictions==FALSE){
          ward.occupancy <- ward.occupancy + 1
          new.arrivals.admitted <- new.arrivals.admitted + 1
          
          if(length(empty.beds.focus.bay)>1){
            new.patient.location <- sample(empty.beds.focus.bay, 1)
          }else{
            new.patient.location <- empty.beds.focus.bay
          }
          
          infection.status[focus.bay, new.patient.location] <- "Susceptible"
          
          if(restrictions==FALSE){
            transfer.times[focus.bay, new.patient.location] <- T + arrival.transfer.times[1]
          }
          
          discharge.times[focus.bay, new.patient.location] <- T + arrival.discharge.times[1]
          
          transfer.period[focus.bay, new.patient.location] <- arrival.transfer.times[1]
          discharge.period[focus.bay, new.patient.location] <- arrival.discharge.times[1]
          latent.period[focus.bay, new.patient.location] <- arrival.latent.period[1]
          incubation.period[focus.bay, new.patient.location] <- arrival.incubation.period[1]
          early.symptomatic.period[focus.bay, new.patient.location] <- arrival.early.symptomatic.period[1]
          late.symptomatic.period[focus.bay, new.patient.location] <- arrival.late.symptomatic.period[1]
          
          
        }else if(length(empty.side.rooms)>0){
          ward.occupancy <- ward.occupancy + 1
          new.arrivals.admitted <- new.arrivals.admitted + 1
          
          if(length(empty.side.rooms)>1){
            new.patient.location <- sample(empty.side.rooms, 1)
          }else{
            new.patient.location <- empty.side.rooms
          }
          
          infection.status[1, new.patient.location] <- "Susceptible"
          
          transfer.times[1, new.patient.location] <- T + arrival.transfer.times[1]
          discharge.times[1, new.patient.location] <- T + arrival.discharge.times[1]
          
          transfer.period[1, new.patient.location] <-  arrival.transfer.times[1]
          discharge.period[1, new.patient.location] <- arrival.discharge.times[1]
          latent.period[1, new.patient.location] <- arrival.latent.period[1]
          incubation.period[1, new.patient.location] <- arrival.incubation.period[1]
          early.symptomatic.period[1, new.patient.location] <- arrival.early.symptomatic.period[1]
          late.symptomatic.period[1, new.patient.location] <- arrival.late.symptomatic.period[1]
          
          
        }else{
          ## arrival is blocked (no free beds)
          blocked.arrivals <- blocked.arrivals + 1
        }
        scheduled.arrival.times <- scheduled.arrival.times[-1]
        arrival.transfer.times <- arrival.transfer.times[-1]
        arrival.discharge.times <- arrival.discharge.times[-1]
        arrival.latent.period <- arrival.latent.period[-1]
        arrival.incubation.period <- arrival.incubation.period[-1]
        arrival.early.symptomatic.period <- arrival.early.symptomatic.period[-1]
        arrival.late.symptomatic.period <- arrival.late.symptomatic.period[-1]
        
        infection.times <- infection.times.sample(T, infection.status, infection.rates, focus.bay, infection.states, infectious.states, spread.to.focus.bay)
        
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
        
        ## Additional Transfer + Discharge LOS HERE!!
        
        latent.times[infection] <- T + latent.period[infection]
        
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
        
        infection.times <- infection.times.sample(T, infection.status, infection.rates, focus.bay, infection.states, 
                                                  infectious.states, spread.to.focus.bay)
        
        discharge.times[discharge] <- Inf
        transfer.times[discharge] <- Inf
        latent.times[discharge] <- Inf
        incubation.times[discharge] <- Inf
        early.symptomatic.times[discharge] <- Inf
        late.symptomatic.times[discharge] <- Inf
        
        latent.period[discharge] <- Inf
        incubation.period[discharge] <- Inf
        early.symptomatic.period[discharge] <- Inf
        late.symptomatic.period[discharge] <- Inf
        
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
          
          infection.times <- infection.times.sample(T, infection.status, infection.rates, focus.bay, infection.states, 
                                                    infectious.states, spread.to.focus.bay)
          
          discharge.times[transfer] <- Inf
          transfer.times[transfer] <- Inf
          latent.times[transfer] <- Inf
          incubation.times[transfer] <- Inf
          early.symptomatic.times[transfer] <- Inf
          late.symptomatic.times[transfer] <- Inf
          
          latent.period[transfer] <- Inf
          incubation.period[transfer] <- Inf
          early.symptomatic.period[transfer] <- Inf
          late.symptomatic.period[transfer] <- Inf
        }
        
        
        
        
      }
      if(event.time==first.latent.time){
        
        infectious <- which.min(latent.times)
        
        infection.status[infectious] <- infectious.states[1]
        
        infection.times <- infection.times.sample(T, infection.status, infection.rates, focus.bay, infection.states, 
                                                  infectious.states, spread.to.focus.bay)
        
        latent.times[infectious] <- Inf
        latent.period[infectious] <- Inf
        incubation.times[infectious] <- T + incubation.period[infectious]
        
        
      }
      
      
      if(event.time==first.incubation.time){
        
        incubation <- which.min(incubation.times)
        
        infection.status[incubation] <- infection.states[which(infection.states==infectious.states[1]) + 1] 
        
        infection.times <- infection.times.sample(T, infection.status, infection.rates, focus.bay, infection.states, 
                                                  infectious.states, spread.to.focus.bay)
        
        incubation.times[incubation] <- Inf
        
        
        if(length(infection.states)>(which(infection.states==infectious.states[1]) + 1)){
          early.symptomatic.times[incubation] <- T + early.symptomatic.period[incubation]
        }else{
          early.symptomatic.times[incubation] <- Inf
        }
      }
      
      if(event.time==first.early.symptomatic.time){
        
        early.symptomatic <- which.min(early.symptomatic.times)
        
        infection.status[early.symptomatic] <- infection.states[which(infection.states==infectious.states[1]) + 2] 
        
        infection.times <- infection.times.sample(T, infection.status, infection.rates, focus.bay, infection.states, 
                                                  infectious.states, spread.to.focus.bay)
        
        early.symptomatic.times[early.symptomatic] <- Inf
        if((length(infection.states)>which(infection.states==infectious.states[1]) + 2)){
          late.symptomatic.times[early.symptomatic] <- T + late.symptomatic.period[incubation]
        }else{
          late.symptomatic.times[early.symptomatic] <- Inf
        }
      }
      
      if(event.time==first.late.symptomatic.time){
        
        recovered <- which.min(late.symptomatic.times)
        
        infection.status[recovered] <- "Recovered"
        
        infection.times <- infection.times.sample(T, infection.status, infection.rates, focus.bay, infection.states, 
                                                  infectious.states, spread.to.focus.bay)
        
        
        
        late.symptomatic.times[recovered] <- Inf
      }
      
      if(event.time==end.restriction.time){
        restrictions <- FALSE
        end.restriction.time <- Inf
        infection.rates[4, ] <- unrestricted.infection.rate
        
        transfer.times[focus.bay, ] <- T + transfer.period[focus.bay, ]
        
        
        infection.times <- infection.times.sample(T, infection.status, infection.rates, focus.bay, infection.states, 
                                                  infectious.states, spread.to.focus.bay)
      }
    }
    
  }
  
  
  if(return.metrics==TRUE){
    return(Metrics)
  }else{
    return(infection.status)
  }
  
}