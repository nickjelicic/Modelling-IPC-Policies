SR.allocation.metrics.UCLH.sensitivity.analysis <- function(parameter,h, initial.ward, destination.ward, disease, initial.bay.size, destination.bay.size,
                                       initial.ward.side.room.occupancy, initial.ward.focus.bay.occupants, 
                                       initial.ward.other.bay.occupants,
                                       alternative.ward.side.room.occupancy, destination.ward.focus.bay.occupants, 
                                       destination.ward.other.bay.occupants,
                                       side.room.infections, patient.infection.status,
                                       Ward.Data, Influenza.Data, COVID.Data, Norovirus.Data, RSV.Data, times,
                                       LOS.distribution="Exponential", spread.to.focus.bay=FALSE){
  
  initial.ward.configuration <- as.numeric(Ward.Data[c("Total Beds on the ward", "Number of side rooms on the ward"), initial.ward])
  initial.ward.configuration <- c(initial.ward.configuration[1], initial.bay.size, initial.ward.configuration[2])
  destination.ward.configuration <- as.numeric(Ward.Data[c("Total Beds on the ward", "Number of side rooms on the ward"), destination.ward])
  destination.ward.configuration <- c(destination.ward.configuration[1], destination.bay.size, destination.ward.configuration[2])
  
  
  initial.ward.arrival.rate <- as.numeric(Ward.Data["Actual admissions", initial.ward])
  initial.ward.departure.rates <- rep(1/as.numeric(Ward.Data["Length of Stay", initial.ward]), 2)
  initial.ward.discharge.rates <- rep(1/as.numeric(Ward.Data["Discharge Length of Stay", initial.ward]), 2)
  initial.ward.transfer.rates <- initial.ward.departure.rates - initial.ward.discharge.rates
  infection.rates <- infection.parameters(Influenza.Data, COVID.Data, Norovirus.Data, RSV.Data, disease, initial.ward)
  initial.ward.infection.rates <- matrix(c(unname(infection.rates[1:3, ]), unname(infection.rates[3, ])), ncol=1)
  recovery.rates <- 1/unname(infection.rates[4:5, ])
  
  
  destination.ward.arrival.rate <- as.numeric(Ward.Data["Actual admissions", destination.ward])
  destination.ward.departure.rates <- rep(1/as.numeric(Ward.Data["Length of Stay", destination.ward]), 2)
  destination.ward.discharge.rates <- rep(1/as.numeric(Ward.Data["Discharge Length of Stay", destination.ward]), 2)
  destination.ward.transfer.rates <- destination.ward.departure.rates - destination.ward.discharge.rates
  infection.rates <- infection.parameters(Influenza.Data, COVID.Data, Norovirus.Data, RSV.Data, disease, destination.ward)
  destination.ward.infection.rates <- matrix(c(unname(infection.rates[1:3, ]), unname(infection.rates[3, ])), ncol=1)
  
  infectious.states <- "Infectious"
  
  
  
  metrics <- readRDS("~/Documents/Simulation Chapter Analysis/Metric Estimates t=1_100.RData")
  
  initial.ward.arrival.rates <- rep(initial.ward.arrival.rate, 2)
  destination.ward.arrival.rates <- rep(destination.ward.arrival.rate, 2)
  latency.rates <- rep(recovery.rates[1], 2)
  gamma.rates <- rep(recovery.rates[2],2)
  if(parameter=="Initial Ward Arrival Rate"){
    initial.ward.arrival.rates[1] <- initial.ward.arrival.rate + h
    initial.ward.arrival.rates[2] <- initial.ward.arrival.rate - h
  }
  if(parameter=="Initial Ward Discharge Rate"){
    initial.ward.discharge.rates[1] <- initial.ward.discharge.rates[1] +h
    initial.ward.discharge.rates[2] <- initial.ward.discharge.rates[1] - h
  }
  if(parameter=="Initial Ward Transfer Rate"){
    initial.ward.transfer.rates[1] <- initial.ward.transfer.rates[1] + h
    initial.ward.transfer.rates[2] <- initial.ward.transfer.rates[1] - h
  }
  if(parameter=="Destination Ward Arrival Rate"){
    destination.ward.arrival.rates[1] <- destination.ward.arrival.rate + h
    destination.ward.arrival.rates[2] <- destination.ward.arrival.rate - h
  }
  if(parameter=="Destination Ward Discharge Rate"){
    destination.ward.discharge.rates[1] <- destination.ward.discharge.rates[1] + h
    destination.ward.discharge.rates[2] <- destination.ward.discharge.rates[1] - h
  }
  if(parameter=="Destination Ward Transfer Rate"){
    destination.ward.transfer.rates[1] <- destination.ward.transfer.rates[1] + h
    destination.ward.transfer.rates[2] <- destination.ward.transfer.rates[1] - h
  }
  if(parameter=="Latency Rate"){
    latency.rates[1] <- recovery.rates[1] + h
    latency.rates[2] <- recovery.rates[1] - h
  }
  if(parameter=="Recovery Rate"){
    gamma.rates[1] <- recovery.rates[2] + h
    gamma.rates[2] <- recovery.rates[2] - h
  }
  
  
  initial.ward.infection.rates <- cbind(initial.ward.infection.rates, initial.ward.infection.rates)
  destination.ward.infection.rates <- cbind(destination.ward.infection.rates, destination.ward.infection.rates)
  
  if(parameter=="Background Infection Rate"){
    initial.ward.infection.rates[1, ] <- initial.ward.infection.rates[1, ] + c(h, -h)
    destination.ward.infection.rates[1, ] <- destination.ward.infection.rates[1, ] + c(h, -h)
  }
  
  if(parameter=="Initial Side Room Infection Rate"){
    initial.ward.infection.rates[2, ] <- initial.ward.infection.rates[2, ] + c(h, -h)
  }
  if(parameter=="Destination Side Room Infection Rate"){
    destination.ward.infection.rates[2, ] <- destination.ward.infection.rates[2, ] + c(h, -h)
  }
  if(parameter=="Initial Open Bay Infection Rate"){
    initial.ward.infection.rates[3, ] <- initial.ward.infection.rates[3, ] + c(h, -h)
    initial.ward.infection.rates[4, ] <- initial.ward.infection.rates[4, ] + c(h, -h)
  }
  if(parameter=="Destination Open Bay Infection Rate"){
    destination.ward.infection.rates[3, ] <- destination.ward.infection.rates[3, ] + c(h, -h)
    destination.ward.infection.rates[4, ] <- destination.ward.infection.rates[4, ] + c(h, -h)
  }
    
    metrics.plus <- SR.allocation.metrics(initial.ward.configuration,
                                     destination.ward.configuration,
                                     initial.ward.side.room.occupancy, initial.ward.focus.bay.occupants, initial.ward.other.bay.occupants,
                                     alternative.ward.side.room.occupany, destination.ward.focus.bay.occupants, destination.ward.other.bay.occupants,
                                     side.room.infections, patient.infection.status,
                                     initial.ward.arrival.rates[1], rep(initial.ward.discharge.rates[1], 2), rep(initial.ward.transfer.rates[1], 2),
                                     initial.ward.infection.rates,
                                     destination.ward.arrival.rates[1], rep(destination.ward.discharge.rates[1], 2), rep(destination.ward.transfer.rates[1], 2),
                                     destination.ward.infection.rates,
                                     infectious.states, c(latency.rates[1], gamma.rates[1]), times,
                                     LOS.distribution, discharge.sd=c(1, 1), departure.sd=c(1, 1), spread.to.focus.bay)
    
    
    metrics.minus <- SR.allocation.metrics(initial.ward.configuration,
                                          destination.ward.configuration,
                                          initial.ward.side.room.occupancy, initial.ward.focus.bay.occupants, initial.ward.other.bay.occupants,
                                          alternative.ward.side.room.occupany, destination.ward.focus.bay.occupants, destination.ward.other.bay.occupants,
                                          side.room.infections, patient.infection.status,
                                          initial.ward.arrival.rates[2], rep(initial.ward.discharge.rates[2], 2), rep(initial.ward.transfer.rates[2], 2),
                                          initial.ward.infection.rates,
                                          destination.ward.arrival.rates[2], rep(destination.ward.discharge.rates[2], 2), rep(destination.ward.transfer.rates[2], 2),
                                          destination.ward.infection.rates,
                                          infectious.states, c(latency.rates[2], gamma.rates[2]), times,
                                          LOS.distribution, discharge.sd=c(1, 1), departure.sd=c(1, 1), spread.to.focus.bay)
    
    derivatives.plus <- NULL
    
    for(policy in 1:3){
      derivatives.policy.plus <- NULL
      for(ward in 1:3){
        derivatives.policy.ward.plus <- metrics.plus[[policy]][[ward]]-metrics[[policy]][[ward]]
        derivatives.policy.plus <- c(derivatives.policy.plus, list(derivatives.policy.ward.plus))
      }
      derivatives.plus <- c(derivatives.plus, list(derivatives.policy.plus))
    }
    
    derivatives.minus <- NULL
    
    for(policy in 1:3){
      derivatives.policy.minus <- NULL
      for(ward in 1:3){
        derivatives.policy.ward.minus <- metrics.minus[[policy]][[ward]]-metrics[[policy]][[ward]]
        derivatives.policy.minus <- c(derivatives.policy.minus, list(derivatives.policy.ward.minus))
      }
      derivatives.minus <- c(derivatives.minus, list(derivatives.policy.minus))
    }
    
  derivatives <- list(derivatives.plus, derivatives.minus)
  
  return(derivatives)
}