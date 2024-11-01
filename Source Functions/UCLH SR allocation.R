UCLH.SR.allocation.metrics <- function(initial.ward, destination.ward, disease, initial.bay.size, destination.bay.size,
                                       initial.ward.side.room.occupancy, initial.ward.focus.bay.occupants, initial.ward.other.bay.occupants,
                                       alternative.ward.side.room.occupancy, destination.ward.focus.bay.occupants, destination.ward.other.bay.occupants,
                                       side.room.infections, 
                                       time.since.positive.test,
                                       Ward.Data, Influenza.Data, COVID.Data, Norovirus.Data, RSV.Data, times,
                                       LOS.distribution, latent.distribution, spread.to.focus.bay){
  

  
  
  initial.ward.configuration <- c(as.numeric(Ward.Data["Total Beds on the ward", initial.ward]),
                                  initial.bay.size,
                                  as.numeric(Ward.Data["Number of side rooms on the ward", initial.ward]))
  
  
  destination.ward.configuration <- c(as.numeric(Ward.Data["Total Beds on the ward", destination.ward]),
                                      destination.bay.size,
                                      as.numeric(Ward.Data["Number of side rooms on the ward", destination.ward]))
  
  
  initial.ward.arrival.rate <- as.numeric(Ward.Data["Actual admissions", initial.ward])
  departure.rate <- 1/as.numeric(Ward.Data["Length of Stay", initial.ward])
  discharge.prop <- as.numeric(Ward.Data["Discharge proportion", initial.ward])
  initial.ward.discharge.rates <- rep(departure.rate*discharge.prop, 2)
  initial.ward.transfer.rates <- rep(departure.rate * (1-discharge.prop), 2)
  
  
  initial.ward.infection.rates <- infection.parameters(Influenza.Data, COVID.Data, Norovirus.Data, RSV.Data, 
                                                       disease, initial.ward)
  recovery.rates <- 1/unname(initial.ward.infection.rates[c(4, 5), ])
  initial.ward.infection.rates <- matrix(c(initial.ward.infection.rates[1:3, ], initial.ward.infection.rates[3, ]), ncol=1)
  
  
  
  destination.ward.arrival.rate <- as.numeric(Ward.Data["Actual admissions",destination.ward])
  departure.rate <- 1/as.numeric(Ward.Data["Length of Stay", destination.ward])
  discharge.prop <- as.numeric(Ward.Data["Discharge proportion", destination.ward])
  destination.ward.discharge.rates <- rep(departure.rate*discharge.prop, 2)
  destination.ward.transfer.rates <- rep(departure.rate * (1-discharge.prop), 2)
  
  destination.ward.infection.rates <-infection.parameters(Influenza.Data, COVID.Data, Norovirus.Data, RSV.Data, 
                                                          disease, destination.ward)[1:3, ]
  destination.ward.infection.rates <- matrix(c(destination.ward.infection.rates, destination.ward.infection.rates[3]), ncol=1)
  
  
  infectious.states <- "Infectious"
  
  init.patient.attributes <- initial.patient.attributes(matrix(c("Infectious", "No Bed", "No Bed"), ncol=1),infectious.states, 
                                                        c(0, 0), c(0,0), 
                                                        recovery.rates, discharge.sd=c(1, 1), 
                                                        transfer.sd=c(1, 1), latent.sd=1, LOS.distribution="Exponential", latent.distribution="Exponential")
  recovery.time <- init.patient.attributes[[2]][[4]][1, 1]
  if(recovery.time<=time.since.positive.test){
    patient.infection.status <- "Recovered"
  }else{
    patient.infection.status <- "Infectious"
  }
 
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
                        LOS.distribution,latent.distribution, discharge.sd=c(1, 1), departure.sd=c(1, 1), spread.to.focus.bay)
  
  return(metrics)
}