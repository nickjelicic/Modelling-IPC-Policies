close.bay.simulation.ward.metrics <- function(bay.size, ward.size, initial.ward.occupancy, infectious.states, infection.states,
                                              exposure.time, arrival.rate, transfer.rates, discharge.rates,
                                              infection.rates, recovery.rates, quarantine.times, spread.coef, close.bay.coef, time.seq, LOS.distribution,
                                              discharge.sd=rep(0.5, 2), transfer.sd=rep(1, 2), queue.departures, spread.to.focus.bay, 
                                              latent.distribution="Exponential", latent.sd=1, random.arrivals=TRUE){
  
  
  ## determine initial state of the bay
  if(nrow(infection.rates)<4){
    infection.rates <- rbind(infection.rates, 0)
  }
  infection.rates[4, ] <- infection.rates[3, ]*spread.coef
  
  infection.status <- matrix("No Bed", nrow=3, ncol=max(bay.size, ward.size-bay.size))
  infection.status[2, 1:bay.size] <- rep("Empty", bay.size)
  if(ward.size>bay.size){
    infection.status[3, 1:(ward.size-bay.size)] <- rep("Empty", ward.size-bay.size)
  }
  
  infection.status[2, 1] <- "Infectious"
  if(bay.size>1){
    infection.status[2, 2:bay.size] <- rep("Susceptible", bay.size - 1)
  }
  
  infection.status <- ward.simulation(2, ward.size, infectious.states, 0, c(0,0), c(0,0),
                                      infection.rates, c(0, 0), exposure.time, infection.status,
                                      LOS.distribution, c(1, 1), c(1, 1), NULL, FALSE)
  
  
  if(initial.ward.occupancy>0){
    infection.status[3, 1:initial.ward.occupancy] <- rep("Susceptible", initial.ward.occupancy)
  }
  infection.status[2, 1] <- "Empty"
  
  end.time <- max(time.seq)
  
  new.patient.attributes <- arrival.patient.attributes(end.time, arrival.rate, transfer.rates, discharge.rates, recovery.rates,
                                                       discharge.sd, transfer.sd, latent.sd, LOS.distribution, latent.distribution)
  
  patient.attributes <- initial.patient.attributes(infection.status, infectious.states, transfer.rates, discharge.rates,
                                                   recovery.rates, discharge.sd, transfer.sd, latent.sd, LOS.distribution, latent.distribution)
  
  infection.times <- infection.times.sample(0, infection.status, infection.rates, 2, infection.states, infectious.states, spread.to.focus.bay)
  
  
  Metrics <- NULL
  for(q in 1:length(quarantine.times)){
    metrics <- ward.simulation.paired(2, infectious.states,
                                      infection.rates, time.seq, 
                                      infection.status, patient.attributes, infection.times,TRUE,
                                      restrictions=TRUE, quarantine.times[q], close.bay.coef, spread.to.focus.bay, queue.departures, 
                                      new.patient.attributes, random.arrivals)
    Metrics <- c(Metrics, list(metrics))
  }
  
  
  return(Metrics)
  
  
}