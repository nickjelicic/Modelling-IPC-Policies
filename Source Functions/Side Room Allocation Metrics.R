SR.allocation.metrics <- function(initial.ward.configuration,
                                  destination.ward.configuration,
                                  initial.ward.side.room.occupancy, initial.ward.focus.bay.occupants, initial.ward.other.bay.occupants,
                                  alternative.ward.side.room.occupany, destination.ward.focus.bay.occupants, destination.ward.other.bay.occupants,
                                  side.room.infections, patient.infection.status,
                                  initial.ward.arrival.rate, initial.ward.discharge.rates, initial.ward.transfer.rates,
                                  initial.ward.infection.rates,
                                  destination.ward.arrival.rate, destination.ward.discharge.rates, destination.ward.transfer.rates,
                                  destination.ward.infection.rates,
                                  infectious.states, recovery.rates, times,
                                  LOS.distribution, latent.distribution, discharge.sd=c(1, 1), departure.sd=c(1, 1), spread.to.focus.bay){
  
  infection.states <- c("Empty", "Susceptible", "Latent", infectious.states, "Recovered")
  
  initial.ward.size <- initial.ward.configuration[1]
  initial.ward.bay.size <- initial.ward.configuration[2]
  initial.ward.side.rooms <- initial.ward.configuration[3]
  
  destination.ward.size <- destination.ward.configuration[1]
  destination.ward.bay.size <- destination.ward.configuration[2]
  destination.ward.side.rooms <- destination.ward.configuration[3]
  
  
  ### setup wards
  initial.ward.infection.status <- matrix("No Bed", nrow=3, ncol=max(initial.ward.bay.size, initial.ward.size-initial.ward.bay.size-initial.ward.side.rooms,
                                                                     initial.ward.side.rooms))
  
  initial.ward.infection.status[1, 1:initial.ward.side.rooms] <- "Empty"
  initial.ward.infection.status[1, 1:initial.ward.side.room.occupancy] <- "Susceptible"
  
  initial.ward.infection.status[2, 1:initial.ward.bay.size] <- "Empty"
  initial.ward.infection.status[2, 1:initial.ward.focus.bay.occupants] <- "Susceptible"
  
  initial.ward.infection.status[3, 1:(initial.ward.size-initial.ward.side.rooms-initial.ward.bay.size)] <- "Empty"
  initial.ward.infection.status[3, 1:initial.ward.other.bay.occupants] <- "Susceptible"
  
  initial.ward.infection.status[2, 1] <- patient.infection.status
  
  
  destination.ward.infection.status <- matrix("No Bed", nrow=3, ncol=max(destination.ward.bay.size, destination.ward.size-destination.ward.bay.size-destination.ward.side.rooms,
                                                                     destination.ward.side.rooms))
  
  destination.ward.infection.status[1, 1:destination.ward.side.rooms] <- "Empty"
  destination.ward.infection.status[1, 1:alternative.ward.side.room.occupancy] <- "Susceptible"
  
  destination.ward.infection.status[2, 1:destination.ward.bay.size] <- "Empty"
  destination.ward.infection.status[2, 1:destination.ward.focus.bay.occupants] <- "Susceptible"
  
  destination.ward.infection.status[3, 1:(destination.ward.size-destination.ward.side.rooms-destination.ward.bay.size)] <- "Empty"
  destination.ward.infection.status[3, 1:destination.ward.other.bay.occupants] <- "Susceptible"
  
  
  alternative.ward.infection.status <- destination.ward.infection.status
  
 
  
  ## input initial conditions - location of patient, full SRs
  destination.ward.infection.status[1, 1:side.room.infections] <- "Infectious"
  if(side.room.infections < destination.ward.side.rooms){
    destination.ward.infection.status[1, (side.room.infections+1):destination.ward.side.rooms] <- "Recovered"
  }
  end.time <- max(times)
  
  ## Initial Ward Attributes
  initial.ward.new.patient.attributes <- arrival.patient.attributes(end.time, initial.ward.arrival.rate, initial.ward.transfer.rates, initial.ward.discharge.rates, 
                                                                    recovery.rates, discharge.sd=c(1, 1), 
                                                       transfer.sd=c(1, 1), latent.sd=1, LOS.distribution, latent.distribution)
 
  
  initial.ward.patient.attributes <-  initial.patient.attributes(initial.ward.infection.status,infectious.states, initial.ward.transfer.rates, initial.ward.discharge.rates, 
                                                                      recovery.rates, discharge.sd=c(1, 1), 
                                                         transfer.sd=c(1, 1), latent.sd=1, LOS.distribution="Exponential", latent.distribution="Exponential")
  
  initial.ward.infection.times <- infection.times.sample(0, initial.ward.infection.status, initial.ward.infection.rates, 2, infection.states, infectious.states, spread.to.focus.bay)
  
  
  ## Destination ward attributes
  destination.ward.patient.attributes <-  initial.patient.attributes(destination.ward.infection.status,infectious.states, destination.ward.transfer.rates, destination.ward.discharge.rates, 
                                                                      recovery.rates, discharge.sd=c(1, 1), 
                                                                      transfer.sd=c(1, 1), latent.sd=1, LOS.distribution="Exponential", latent.distribution="Exponential")
  
  destination.ward.infection.times <- infection.times.sample(0, destination.ward.infection.status, destination.ward.infection.rates, 2, infection.states, infectious.states, spread.to.focus.bay)
  
  
  destination.ward.new.patient.attributes <- arrival.patient.attributes(end.time, destination.ward.arrival.rate, destination.ward.transfer.rates, destination.ward.discharge.rates, 
                                                                    recovery.rates, discharge.sd=c(1, 1), 
                                                                    transfer.sd=c(1, 1), latent.sd=1, LOS.distribution, latent.distribution)
  
  
  ## Alternative ward attributes
  alternative.ward.patient.attributes <-  initial.patient.attributes(destination.ward.infection.status,infectious.states, destination.ward.transfer.rates, destination.ward.discharge.rates, 
                                                                         recovery.rates, discharge.sd=c(1, 1), 
                                                                         transfer.sd=c(1, 1), latent.sd=1, LOS.distribution="Exponential", latent.distribution="Exponential")
  
  alternative.ward.infection.times <- infection.times.sample(0, alternative.ward.infection.status, destination.ward.infection.rates, 1, infection.states, infectious.states, spread.to.focus.bay)
  
  
  alternative.ward.new.patient.attributes <- arrival.patient.attributes(end.time, destination.ward.arrival.rate, destination.ward.transfer.rates, destination.ward.discharge.rates, 
                                                                        recovery.rates, discharge.sd=c(1, 1), 
                                                                        transfer.sd=c(1, 1), latent.sd=1, LOS.distribution, latent.distribution)
  
  
  ## Policy 1 - keep patient on initial ward
  Metrics <- NULL
  
  for(policy in 1:3){
    initial.ward.infection.status.policy <- initial.ward.infection.status
    destination.ward.infection.status.policy <- destination.ward.infection.status
    alternative.ward.infection.status.policy <- alternative.ward.infection.status
    
    initial.ward.patient.attributes.policy <- initial.ward.patient.attributes
    initial.ward.new.patient.attributes.policy <- initial.ward.new.patient.attributes
    
    destination.ward.patient.attributes.policy <- destination.ward.patient.attributes
    destination.ward.new.patient.attributes.policy <- destination.ward.new.patient.attributes
    
    alternative.ward.patient.attributes.policy <- alternative.ward.patient.attributes
    alternative.ward.new.patient.attributes.policy <- alternative.ward.new.patient.attributes
    
    initial.ward.infection.times.policy <- initial.ward.infection.times
    destination.ward.infection.times.policy <- destination.ward.infection.times
    alternative.ward.infection.times.policy <- alternative.ward.infection.times
    
    
    if(policy==2){
      ## Move Patient from initial ward to open bay on destination ward
      
      initial.ward.infection.status.policy[2, 1] <- "Empty"
      empty.beds <- which(destination.ward.infection.status.policy[2, ]=="Empty")
      destination.ward.infection.status.policy[2, empty.beds[1]] <- patient.infection.status
      
      for(i in 1:2){
        for(j in 1:length(initial.ward.patient.attributes[[i]])){
          initial.ward.patient.attributes.policy[[i]][[j]][2, 1] <- Inf
          destination.ward.patient.attributes.policy[[i]][[j]][2, empty.beds[1]] <- initial.ward.patient.attributes[[i]][[j]][2, 1]
        }
      }
      
      initial.ward.infection.times.policy <- infection.times.sample(0, initial.ward.infection.status.policy, initial.ward.infection.rates, 2, infection.states, infectious.states, spread.to.focus.bay)
      destination.ward.infection.times.policy <- infection.times.sample(0, destination.ward.infection.status.policy, destination.ward.infection.rates, 2, infection.states, infectious.states, spread.to.focus.bay)
      
    }else if(policy==3){
      
      initial.ward.infection.status.policy[2, 1] <- "Empty"
      empty.beds <- which(alternative.ward.infection.status.policy[1, ]=="Empty")
      alternative.ward.infection.status.policy[1, empty.beds[1]] <- patient.infection.status
      
      for(i in 1:2){
        for(j in 1:length(initial.ward.patient.attributes[[i]])){
          initial.ward.patient.attributes.policy[[i]][[j]][2, 1] <- Inf
          alternative.ward.patient.attributes.policy[[i]][[j]][1, empty.beds[1]] <- initial.ward.patient.attributes[[i]][[j]][2, 1]
        }
      }
      initial.ward.infection.times.policy <- infection.times.sample(0, initial.ward.infection.status.policy, initial.ward.infection.rates, 2, infection.states, infectious.states, spread.to.focus.bay)
      alternative.ward.infection.times.policy <- infection.times.sample(0, alternative.ward.infection.status.policy, destination.ward.infection.rates, 2, infection.states, infectious.states, spread.to.focus.bay)
      
    }
    
    metrics.initial.ward <- ward.simulation.paired(2, infectious.states,
                           initial.ward.infection.rates, times, 
                           initial.ward.infection.status.policy, initial.ward.patient.attributes.policy, initial.ward.infection.times.policy,
                            TRUE,
                           restrictions=FALSE, quarantine.time=0, close.bay.coef=0, spread.to.focus.bay=FALSE, queue.departures=TRUE, 
                           initial.ward.new.patient.attributes.policy)
    
    
    metrics.destination.ward <- ward.simulation.paired(2, infectious.states,
                                                   destination.ward.infection.rates, times, 
                                                   destination.ward.infection.status.policy, destination.ward.patient.attributes.policy, destination.ward.infection.times.policy,
                                                   TRUE,
                                                   restrictions=FALSE, quarantine.time=0, close.bay.coef=0, spread.to.focus.bay=FALSE, queue.departures=TRUE, 
                                                  destination.ward.new.patient.attributes.policy)
   
    
    metrics.alternative.ward <-  ward.simulation.paired(1, infectious.states,
                                                        destination.ward.infection.rates, times, 
                                                        alternative.ward.infection.status.policy, alternative.ward.patient.attributes.policy, alternative.ward.infection.times.policy,
                                                        TRUE,
                                                        restrictions=FALSE, quarantine.time=0, close.bay.coef=0, spread.to.focus.bay=FALSE, queue.departures=TRUE, 
                                                        alternative.ward.new.patient.attributes.policy)
    
    metrics <- list(metrics.initial.ward, metrics.destination.ward, metrics.alternative.ward)
    Metrics <- c(Metrics, list(metrics))
  }
  

  
  
  
  return(Metrics)
  
  
  
  
}