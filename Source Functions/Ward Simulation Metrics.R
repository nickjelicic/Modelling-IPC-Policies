simulation.ward.metrics <- function(bay.size, ward.size, initial.ward.occupancy, infectious.states, time.in.contact, arrival.rate, departure.rates, discharge.rates,
                               infection.rates, recovery.rates, quarantine.time, spread.coef, close.bay.coef, times, LOS.distribution,
                               discharge.sd=rep(0.5, 2), departure.sd=rep(1, 2), queue.departures, spread.to.focus.bay){
 
  
  
  
  ## determine initial state of the bay
  if(length(infection.rates)<4){
    infection.rates <- c(infection.rates, 0)
  }
  infection.rates[4] <- infection.rates[3]*spread.coef
  
  infection.status <- matrix("No Bed", nrow=3, ncol=max(bay.size, ward.size-bay.size))
  infection.status[2, 1:bay.size] <- rep("Empty", bay.size)
  if(ward.size>bay.size){
    infection.status[3, 1:(ward.size-bay.size)] <- rep("Empty", ward.size-bay.size)
  }
  
  infection.status[2, 1] <- "Infectious"
  initial.infection.rates <- infection.rates
  initial.infection.rates[c(2, 4)] <- 0
  
  sorted.times <- sort(time.in.contact, decreasing=TRUE)
  scheduled.arrivals <- sorted.times[1]-sorted.times
  
  infection.status <- ward.simulation(3, ward.size, infectious.states,0, c(0,0), c(0,0),
                                      initial.infection.rates,c(recovery.rates[1], rep(0, length(recovery.rates)-1)), max(time.in.contact), 
                                      infection.status,
                                      LOS.distribution, discharge.sd, departure.sd, scheduled.arrivals, FALSE,
                                      restrictions=FALSE, quarantine.time=0, close.bay.coef=0, spread.to.focus.bay=FALSE)
  infection.status[2, 1] <- "Empty"
  if(initial.ward.occupancy>0){
    infection.status[3, 1:initial.ward.occupancy] <- rep("Susceptible", initial.ward.occupancy)
  }
  

  
  metrics.no.restrictions <- ward.simulation(2, ward.size, infectious.states,arrival.rate, discharge.rates, departure.rates,
                              infection.rates, recovery.rates, times, 
                              infection.status,
                              LOS.distribution, discharge.sd, departure.sd, NULL, TRUE, restrictions=FALSE,
                              quarantine.time=0, close.bay.coef=0, spread.to.focus.bay, queue.departures)
  
  metrics.restrictions <- ward.simulation(2, ward.size, infectious.states,arrival.rate, discharge.rates, departure.rates,
                                             infection.rates, recovery.rates, times, 
                                             infection.status,
                                             LOS.distribution, discharge.sd, departure.sd, NULL, TRUE, restrictions=TRUE, 
                                          quarantine.time, close.bay.coef, spread.to.focus.bay, queue.departures)
  
  difference <- metrics.no.restrictions - metrics.restrictions
  
  parameters <- c(arrival.rate, departure.rates, recovery.rates, infection.rates, time.in.contact, times)
  
  output <- c(parameters, difference)
  
  return(rbind(metrics.no.restrictions, metrics.restrictions))

  
}