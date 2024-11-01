analytical.metrics <- function(bay.size, ward.size, ward.occupants, gen.matrices, gen.infected.matrices, D.I, D.arrivals,D.infectious.dep,D.departures,
                    infection.states, infectious.states, time.in.contact, arrival.rate, departure.rates, discharge.rates,
                    infection.rates, recovery.rates, quarantine.time, time.seq, difference, spread.coef, close.bay.coef, queue.departures=FALSE, 
                    keep.bay.open.metrics=TRUE){

  
  initial.dist <- analytical.initial.distribution(bay.size, ward.size, infection.states, infectious.states,gen.matrices, infection.rates,
                                                   time.in.contact, ward.occupants)
  
  infections <- NULL
  throughput <- NULL
  blocked.patients <- NULL
  unused_bed.days <- NULL
  unused_bed.days.ward <- NULL
  outbreak.prob <- NULL
  infectious.discharged <- NULL
  departures <- NULL
  Metrics <- NULL
  
  ## Generate necessary matrices
  
  Q <- transition.rate.matrix.ward(bay.size, ward.size, gen.matrices, arrival.rate, departure.rates, departure.rates, recovery.rates, infection.rates, spread.coef,
                                   arrival.rate)
  
  ## wrong!
  Q.restrictions <- transition.rate.matrix.ward(bay.size, ward.size, gen.matrices, arrival.rate, discharge.rates, departure.rates,
                                                recovery.rates, infection.rates, 
                                                spread.coef*close.bay.coef, 0)
  
  
  n.states <- nrow(Q)
  states.lab <- gen.ward.states(bay.size, ward.size, infection.states, TRUE, FALSE)
  initial.patients <- numeric(n.states)
  for(i in 1:n.states){
    initial.patients[i] <- length(which(states.lab[i, ]!="Empty")) - 2
  }
  outbreak.states <- which(states.lab[, bay.size+1]=="Outbreak")
  
  
  A <- NULL
  
  if(keep.bay.open.metrics==TRUE){
    ## no restriction (keep bay open)
    for(t in time.seq){
      infections <- c(infections, initial.dist %*% direct.transitions(Q, D.I, A, t))
      
      throughput <- c(throughput, initial.dist %*% (direct.transitions(Q, D.arrivals, A, t)))
      
      blocked.patients <- c(blocked.patients, initial.dist %*% blocked.ward.arrivals(bay.size, ward.size, infection.states, Q, A, t, arrival.rate, restrictions=FALSE))
      
      unused_bed.days <- c(unused_bed.days, initial.dist %*% unused.bed.days.bay(bay.size, ward.size, infection.states, Q, A, t))
      
      unused_bed.days.ward <- c(unused_bed.days.ward, initial.dist %*% unused.bed.days.ward(bay.size, ward.size, infection.states, Q, A, t))
      
      outbreak.prob <- c(outbreak.prob, initial.dist %*% outbreak.probability.ward(Q, A, t, outbreak.states))
      
      infectious.discharged <- c(infectious.discharged, initial.dist %*% direct.transitions(Q, D.infectious.dep, A, t))
      
      departures <- c(departures, initial.dist %*% direct.transitions(Q, D.departures, A, t))
      
    }
    
    Metrics <- cbind(infections, throughput, blocked.patients, unused_bed.days, unused_bed.days.ward, outbreak.prob, infectious.discharged,
                     departures)
  }
  

  
  restrictions <- TRUE
  infections <- NULL
  throughput <- NULL
  blocked.patients <- NULL
  unused_bed.days <- NULL
  unused_bed.days.ward <- NULL
  outbreak.prob <- NULL
  infectious.discharged <- NULL
  departures <- NULL
  
  
  ## bay closed for quarantine period
  transfer.rates <- departure.rates-discharge.rates
  p <- 1-exp(-transfer.rates[1]*quarantine.time)
  discharges.at.opening <- numeric(n.states)
  departures.at.opening <- numeric(n.states)
  if(queue.departures==TRUE){
    for(i in 1:n.states){
      discharges.at.opening[i] <- p*length(which(states.lab[i, ]%in%infectious.states))
      departures.at.opening[i] <- p*length(which(states.lab[i, 1:bay.size]!="Empty"))
    }
    
  }
  post.restriction.infectious.discharges <- post.restriction.distribution(states.lab, initial.dist, Q.restrictions, A, quarantine.time, 
                                                                          transfer.rates, bay.size, n.states, FALSE)%*%discharges.at.opening
  
  post.restriction.departures <- post.restriction.distribution(states.lab, initial.dist, Q.restrictions, A, quarantine.time, 
                                                                          transfer.rates, bay.size, n.states, FALSE)%*%departures.at.opening
  
  for(t in time.seq){
    
    if(t <= quarantine.time){
      infections <- c(infections, initial.dist%*%direct.transitions(Q.restrictions, D.I, A, t))
      
      throughput <- c(throughput, initial.dist%*%direct.transitions(Q.restrictions, D.arrivals, A, t))
      
      blocked.patients <- c(blocked.patients, 
                            initial.dist%*%blocked.ward.arrivals(bay.size, ward.size, infection.states, Q.restrictions, A, t, arrival.rate, TRUE))
      
      unused_bed.days <- c(unused_bed.days, initial.dist%*%unused.bed.days.bay(bay.size, ward.size, infection.states, Q.restrictions, A, t))
      
      unused_bed.days.ward <- c(unused_bed.days.ward, initial.dist%*%unused.bed.days.ward(bay.size, ward.size, infection.states, Q.restrictions, A, t))
      
      outbreak.prob <- c(outbreak.prob, initial.dist%*%outbreak.probability.ward(Q.restrictions, A, t, outbreak.states))
      
      infectious.discharged <- c(infectious.discharged, initial.dist%*%direct.transitions(Q.restrictions, D.infectious.dep, A, t))
      
      departures <- c(departures, initial.dist%*%direct.transitions(Q.restrictions, D.departures, A, t))
      
    }else{
      final.states <- post.restriction.distribution(states.lab, initial.dist, Q.restrictions, A, quarantine.time, transfer.rates, bay.size, n.states, queue.departures)
      
      infections <- c(infections, initial.dist%*%direct.transitions(Q.restrictions, D.I, A, quarantine.time) + 
                        final.states%*%direct.transitions(Q, D.I, A, t - quarantine.time))
      
      throughput <- c(throughput, initial.dist%*%direct.transitions(Q.restrictions, D.arrivals, A, quarantine.time) + 
                        final.states%*%direct.transitions(Q, D.arrivals, A, t-quarantine.time))
      
      blocked.patients <- c(blocked.patients, initial.dist%*%blocked.ward.arrivals(bay.size, ward.size, infection.states, Q.restrictions, A, quarantine.time, arrival.rate, TRUE) + 
                              final.states%*%blocked.ward.arrivals(bay.size, ward.size, infection.states, Q, A, t-quarantine.time, arrival.rate, FALSE))
      
      unused_bed.days <- c(unused_bed.days, initial.dist%*%unused.bed.days.bay(bay.size, ward.size, infection.states, Q.restrictions, A, quarantine.time) + 
                             final.states %*% unused.bed.days.bay(bay.size, ward.size, infection.states, Q, A, t-quarantine.time))
      
      unused_bed.days.ward <- c(unused_bed.days.ward, initial.dist%*%unused.bed.days.ward(bay.size, ward.size, infection.states, Q.restrictions, A, quarantine.time) + 
                             final.states %*% unused.bed.days.ward(bay.size, ward.size, infection.states, Q, A, t-quarantine.time))
      
      outbreak.prob <- c(outbreak.prob, final.states %*% outbreak.probability.ward(Q, A, t - quarantine.time, outbreak.states))
      
      
      infectious.discharged <- c(infectious.discharged, initial.dist%*%direct.transitions(Q.restrictions, D.infectious.dep, A, quarantine.time) + 
                                   post.restriction.infectious.discharges + final.states%*%direct.transitions(Q, D.infectious.dep, A, t-quarantine.time))
      
      departures <- c(departures, initial.dist%*%direct.transitions(Q.restrictions, D.departures, A, quarantine.time) + 
                        post.restriction.departures + final.states%*%direct.transitions(Q, D.departures, A, t-quarantine.time))
    }
  }
  
  Metrics.2 <- cbind(infections, throughput, blocked.patients, unused_bed.days, unused_bed.days.ward, outbreak.prob, infectious.discharged, departures)
  
  Difference <- Metrics - Metrics.2
  
  parameters <- c(arrival.rate, departure.rates, discharge.rates, recovery.rates, infection.rates, time.in.contact)
  Parameters <- cbind(t(matrix(parameters, nrow=length(parameters), ncol=length(time.seq))), unname(time.seq))
  
  if(difference==TRUE){
    output <- Difference
  }else{
    output <- rbind(Metrics, Metrics.2)
  }
  
  
  return(output)

}