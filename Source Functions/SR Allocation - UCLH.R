SR.allocation.metrics.UCLH <- function(initial.ward, destination.ward, disease, initial.bay.size, destination.bay.size,
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
  
  
  
  metrics <- SR.allocation.metrics(initial.ward.configuration,
                         destination.ward.configuration,
                         initial.ward.side.room.occupancy, initial.ward.focus.bay.occupants, initial.ward.other.bay.occupants,
                         alternative.ward.side.room.occupany, destination.ward.focus.bay.occupants, destination.ward.other.bay.occupants,
                         side.room.infections, patient.infection.status,
                         initial.ward.arrival.rate, initial.ward.discharge.rates, initial.ward.transfer.rates,
                         initial.ward.infection.rates,
                         destination.ward.arrival.rate, destination.ward.discharge.rates, destination.ward.transfer.rates,
                         destination.ward.infection.rates,
                         
                         
                         infectious.states, recovery.rates, times,
                         LOS.distribution, discharge.sd=c(1, 1), departure.sd=c(1, 1), spread.to.focus.bay)
  
  return(metrics)
}