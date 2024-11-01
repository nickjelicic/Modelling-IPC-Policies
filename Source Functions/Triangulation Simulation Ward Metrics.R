triangulation.simulation.ward.metrics <- function(bay.size, ward.size, initial.ward.occupancy, infectious.states, infection.states,
                                                  infection.probability, arrival.rate, transfer.rates, discharge.rates,
                                    infection.rates, recovery.rates, quarantine.time, spread.coef, close.bay.coef, time.seq, LOS.distribution,
                                    discharge.sd=rep(0.5, 2), transfer.sd=rep(1, 2), queue.departures, spread.to.focus.bay, 
                                    latent.distribution="Exponential", latent.sd=1){
  
  
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
  
  for(i in 1:bay.size){
    infection.status[2, i] <- infection.states[sample(1:length(infection.states),size=1, prob=infection.probability)]
  }
  if(initial.ward.occupancy>0){
    infection.status[3, 1:initial.ward.occupancy] <- rep("Susceptible", initial.ward.occupancy)
  }
  
  
  
  metrics.no.restrictions <- ward.simulation(2, ward.size, infectious.states,arrival.rate, discharge.rates, transfer.rates,
                                             infection.rates, recovery.rates, time.seq, 
                                             infection.status,
                                             LOS.distribution, discharge.sd, transfer.sd, NULL, TRUE, restrictions=FALSE,
                                             quarantine.time=0, close.bay.coef=0, spread.to.focus.bay, queue.departures, latent.distribution, latent.sd)
  
  metrics.restrictions <- ward.simulation(2, ward.size, infectious.states,arrival.rate, discharge.rates, transfer.rates,
                                          infection.rates, recovery.rates, time.seq, 
                                          infection.status,
                                          LOS.distribution, discharge.sd, transfer.sd, NULL, TRUE, restrictions=TRUE, 
                                          quarantine.time, close.bay.coef, spread.to.focus.bay, queue.departures, latent.distribution, latent.sd)

  
  
  return(list(metrics.no.restrictions, metrics.restrictions))
  
  
}