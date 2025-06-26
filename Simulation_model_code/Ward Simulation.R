#' This function is the main body of the simulation and determines the key metrics for the ward
#'
#'INPUTS
#' focus_bay - a positive integer between 1 and nrow(infection_status) representing the focus bay
#' 
#' n_infectious_states - positive integer, either 1, 2 or 3. The number of infectious states.
#' 
#' infection_rates - a matrix of positive numbers representing the infection rates governing infection transmission within the ward
#' The matrix is of size 4 x m where m is the number of infectious states
#' The first row represents the background infection rate
#' The second row is the side room infection rate
#' The third row is the within-bay infection rate
#' The fourth row is the between open bay infection rate
#' The jth column represents the infection rate if the infectious patient is in the jth infectious state
#' i.e if there is only one infectious state, 
#' infection_rates <- rbind(background_infection_rate, SR_infection_rate, within_bay_infection_rate, between_OB_infection_rate)
#' 
#' time_seq - a vector of positive numbers, representing various stopping times for the simulation
#'   
#' infection_status - a matrix of strings representing the infection status of each bed in the ward
#' The matrix is of size n x m where n is the number of bays in the ward (including side rooms) and m is the largest size of any of these wards
#' A row of the matrix represents the infection status of beds in that bay. Row 1 always represents side rooms.
#' An element can be either "Empty", "Susceptible", "Latent", in an infectious state, "Recovered" or "No bed" if no such bed exists
#' e.g. infection_status <- rbind(c("Susceptible", "No bed"), c("Infectious", "Infectious"),c("Susceptible", "Susceptible"))
#' represents a ward with 1 side room with a susceptible patient, 1 ward with 2 infectious patients and another ward with 2 susceptible patients
#' 
#' init_patient_attributes - A list of matrices of the same diensions as infection_status representing patient attributes, in order:
#' transfer_period - the time until each patient is transferred from the ward
#' discharge_period -the time until each patient is discharge from the ward
#' latent_period - the duration of each patient's latent period if they are to catch the infection
#' incubation_period - the duration of each patient's time in the first infectious period if they are to catch the infection
#' note that if there is only one infectious period then this is the time for which they will be infectious.
#' early_symptomatic_period - The duration of each patient's time in the second infectious period if they are to catch the infection.
#' If there is only one infectious period, then this is set to infinity for all patients.
#' late_symptomatic_period - The duration of each patient's time in the third infectious period if they are to catch the infection.
#' If there are fewer than three infectious periods, then this is set to infinity for all patients.
#' 
#' infection_times - A matrix of positive numbers representing the time at which each patient will contract an infection
#' The matrix is of size n x m where n is the number of bays in the ward (including side rooms) and m is the largest size of any of these wards
#' A row of the matrix represents the infection status of beds in that bay. Row 1 always represents side rooms.
#' If the bed cannot become infected (either it is empty, does not exist, or the patient is already infected) then the time is set to infinity
#' 
#' restrictions - logical. If TRUE, then the focus is bay is closed until the quarantine time.
#' If FALSE, the focus bay is open
#' 
#' quarantine_time - positive number representing the time until the focus bay is reopened if it is closed at the start of the simulatino
#' If restrictions=FALSE, this need not be specified
#' 
#' close_bay_coef - positive number equal to the infection rate from a closed bay divided by the infection rate from an open bay
#' If restrictions=FALSE, this need not be specified
#' 
#' spread_to_focus_bay - logical. If FALSE, infections can only spread from the focus bay. 
#' If TRUE, infections can spread anywhere in the ward
#' 
#' queue_departures - logical. If TRUE, patients leave as soon as restrictions are lifted if they have exceeded their transfer period
#' If FALSE, their transfer period is resampled after restrictions are lifted
#' 
#' new_patient_attributes -  the attributes of the patients that will arrive at the ward as a list of vectors. 
#' A particular element refers to the same patient for each vector.
#' scheduled_arrival_times - the time at which each patient arrives at the ward
#' arrival_transfer_times - the time until each patient is transferred from the ward
#' arrival_discharge_times - the time until each patient is discharge from the ward
#' arrival_latent_period - the duration of each patient's latent period if they are to catch the infection
#' arrival_incubation_period - the duration of each patient's time in the first infectious period if they are to catch the infection
#' note that if there is only one infectious period then this is the time for which they will be infectious.
#' arrival_early_symptomatic_period - The duration of each patient's time in the second infectious period if they are to catch the infection.
#' If there is only one infectious period, then this is set to infinity for all patients.
#' arrival_late_symptomatic_period - The duration of each patient's time in the third infectious period if they are to catch the infection.
#' If there are fewer than three infectious periods, then this is set to infinity for all patients.
#' 
#'OUTPUT
#''A matrix, where the ith row represents the metrics for the ith stopping time. The metrics are, in order:
#'Number of infections in the focus bay
#'Nubmer of infections elsewhere in the ward
#'Arrivals admitted to the ward
#'Number of blocked arrivals
#'Unused bed days in the focus bay
#'Unused bed days elsewhere in the ward
#'Binary indicator of whether an oubreak occured
#'Number of patients discharged while infectious
#'Number of patients transferred while infectious
#'Number of patients discharged while infected
#'Number of patients transferred while infected
#'Number of days of delayed discharges
#'Number of discharges
#'Number of transfers

ward.simulation.paired <- function(focus_bay, n_infectious_states,
                            infection_rates, time_seq, 
                            infection_status, init_patient_attributes, infection_times,
                            restrictions=FALSE, quarantine_time=0, close_bay_coef=0, spread_to_focus_bay=FALSE, queue_departures=TRUE, 
                            new_patient_attributes){
  
  return_metrics <- TRUE
  random_arrivals <- TRUE
  
  if(n_infectious_states==1){
    infectious_states <- "Infectious"
  }else if(n_infectious_states==2){
    infectious_states <- c("Pre-symptomatic", "Symptomatic")
  }else if(n_infectious_states==3){
    infectious_states <- c("Pre-symptomatic", "Early symptomatic", "Late symptomatic")
  }
  
  infection_states <- c("Empty", "Susceptible", "Latent", infectious_states, "Recovered")
  
  ## Initial Conditions
  #####
  patient.attributes <- init_patient_attributes
  
  transfer.period <- patient.attributes[[1]]
  discharge.period <- patient.attributes[[2]]
  latent.period <- patient.attributes[[3]]
  incubation.period <- patient.attributes[[4]]
  early.symptomatic.period <- patient.attributes[[5]]
  late.symptomatic.period <- patient.attributes[[6]]
  
  transfer.times <- transfer.period
  discharge.times <- discharge.period
  
  latent.times <- latent.period
  latent.times[which(infection_status!="Latent")] <- Inf
  
  incubation.times <- incubation.period
  incubation.times[which(infection_status!=infectious_states[1])] <- Inf
  
  early.symptomatic.times <- early.symptomatic.period
  if(length(infectious_states)>1){
    early.sympatomatic.times[which(infection_status!=infectious_states[2])] <- Inf
  }
  
  late.symptomatic.times <- late.symptomatic.period
  if(length(infectious_states)>2){
    late.sympatomatic.times[which(infection_status!=infectious_states[3])] <- Inf
  }
  
  
  scheduled.arrival.times <- new_patient_attributes[[1]]
  arrival.transfer.times <- new_patient_attributes[[2]]
  arrival.discharge.times <- new_patient_attributes[[3]]
  arrival.latent.period <- new_patient_attributes[[4]]
  arrival.incubation.period <- new_patient_attributes[[5]]
  arrival.early.symptomatic.period <- new_patient_attributes[[6]]
  arrival.late.symptomatic.period <- new_patient_attributes[[7]]
  ## 
  if(restrictions==TRUE){
    end.restriction.time <- quarantine_time
    
    unrestricted.infection.rate <- infection_rates[4, ]
    infection_rates[4, ] <- infection_rates[4, ]*close_bay_coef
    
  }else{
    end.restriction.time <- Inf
  }
  
  T <- 0
  focus_bay.infections <- 0
  other.bay.infections <- 0
  new.arrivals.admitted <- 0
  blocked.arrivals <- 0
  unused_bed.days.focus_bay <- 0
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
  
  time_seq <- sort(time_seq, decreasing=FALSE)
  end.time <- time_seq[1]
  
  Metrics <- NULL
  
  bay.occupancy <- length(which(infection_status!="Empty"))
  
  Finish <- FALSE
  
  n.bay <- nrow(infection_status)

  
  #####
  ##simulate bay
  
  
  while(Finish==FALSE){
    
    if(length(scheduled.arrival.times)>0){
      first.arrival.time <- min(scheduled.arrival.times)
    }else{
      first.arrival.time <- Inf
    }
    
    first.infection.time <- min(infection_times)
    first.transfer.time <- min(transfer.times)
    first.discharge.time <- min(discharge.times)
    
    first.latent.time <- min(latent.times)
    first.incubation.time <- min(incubation.times)
    
    
    first.early.symptomatic.time <- min(early.symptomatic.times)
    first.late.symptomatic.time <- min(late.symptomatic.times)
    
    
    event.time <- min(first.arrival.time, first.infection.time, first.transfer.time, first.discharge.time,
                      first.latent.time, first.incubation.time, 
                      first.early.symptomatic.time, first.late.symptomatic.time, end.restriction.time)
    
    empty.beds.focus_bay <- length(which(infection_status[focus_bay, ]=="Empty"))
    empty.beds.other.bays <- length(which(infection_status[-focus_bay, ]=="Empty"))
    
    if(event.time > end.time){
      time_seq <- time_seq[-1]
      
      unused_bed.days.focus_bay <- unused_bed.days.focus_bay + (end.time-T) * empty.beds.focus_bay
      unused_bed.days.other.bays <- unused_bed.days.other.bays + (end.time-T) * empty.beds.other.bays
      
      T <- end.time
      
      if(length(time_seq)>0){
        end.time <- time_seq[1]
      }else{
        Finish <- TRUE
      }
      
      
      
      metrics <- c(focus_bay.infections, other.bay.infections, new.arrivals.admitted, blocked.arrivals, unused_bed.days.focus_bay, unused_bed.days.other.bays, outbreak, infectious.discharged,
                   infectious.transferred, infected.discharged, infected.transferred, days.discharges.delayed, discharges, transfers)
      
      Metrics <- rbind(Metrics, metrics)
      
    }else{
      
      unused_bed.days.focus_bay <- unused_bed.days.focus_bay + (event.time - T) * empty.beds.focus_bay
      unused_bed.days.other.bays <- unused_bed.days.other.bays + (event.time - T) * empty.beds.other.bays
      
      T <- event.time
      
      if(event.time==first.arrival.time){
        
        
        ## Arrivals should be placed in other beds on the ward if available
        empty.beds <- which(infection_status=="Empty")
        
        empty.beds.side.rooms <- empty.beds[which(empty.beds%%nrow(infection_status)==1)]
        empty.beds.focus_bay <- empty.beds[which(empty.beds%%nrow(infection_status)==2)]
        
        empty.beds.other.bays <- empty.beds[which(empty.beds%%nrow(infection_status)==0)]
        
        
        new.patient.location <- NULL
        
        if(random_arrivals==TRUE){
          
          if(restrictions==TRUE){
            
            available.beds <- c(empty.beds.side.rooms, empty.beds.other.bays)
          }else{
            available.beds <- c(empty.beds.side.rooms, empty.beds.focus_bay, empty.beds.other.bays)
          }
          if(length(available.beds)==0){
            blocked.arrivals <- blocked.arrivals + 1
          }else if(length(available.beds)==1){
            new.patient.location <- available.beds
          }else{
            new.patient.location <- sample(available.beds, 1)
          }
          
        }else{
          
          if(length(empty.beds.other.bays)>0){
            
            if(length(empty.beds.other.bays)==1){
              new.patient.location <- empty.beds.other.bays
            }else{
              new.patient.location <- sample(empty.beds.other.bays, 1)
            }
          }else if(length(empty.beds.focus_bay)>0 & restrictions==FALSE){
            if(length(empty.beds.focus_bay)==1){
              new.patient.location <- empty.beds.focus_bay
            }else{
              new.patient.location <- sample(empty.beds.focus_bay, 1)
            }
          }else if(length(empty.beds.side.rooms)>0){
            if(length(empty.beds.side.rooms)==1){
              new.patient.location <- empty.beds.side.rooms
            }else{
              new.patient.location <- sample(empty.beds.side.rooms, 1)
            }
          }else{
            blocked.arrivals <- blocked.arrivals + 1
          }
          
        }
        
        
        if(length(new.patient.location)>0){
          ward.occupancy <- ward.occupancy + 1
          new.arrivals.admitted <- new.arrivals.admitted + 1
          
          infection_status[new.patient.location] <- "Susceptible"
          
          
          transfer.times[new.patient.location] <- T + arrival.transfer.times[1]
          discharge.times[new.patient.location] <- T + arrival.discharge.times[1]
          
          transfer.period[new.patient.location] <- arrival.transfer.times[1] 
          discharge.period[new.patient.location] <- arrival.discharge.times[1]
          latent.period[new.patient.location] <- arrival.latent.period[1]
          incubation.period[new.patient.location] <- arrival.incubation.period[1]
          early.symptomatic.period[new.patient.location] <- arrival.early.symptomatic.period[1]
          late.symptomatic.period[new.patient.location] <- arrival.late.symptomatic.period[1]
        }
        
        ## Priority order of placing new patients (if occupied move to next one)
        # 1. Other open bay
        # 2. Focus bay
        # 3. Side Room

        scheduled.arrival.times <- scheduled.arrival.times[-1]
        arrival.transfer.times <- arrival.transfer.times[-1]
        arrival.discharge.times <- arrival.discharge.times[-1]
        arrival.latent.period <- arrival.latent.period[-1]
        arrival.incubation.period <- arrival.incubation.period[-1]
        arrival.early.symptomatic.period <- arrival.early.symptomatic.period[-1]
        arrival.late.symptomatic.period <- arrival.late.symptomatic.period[-1]
        
        infection_times <- infection.times.sample(T, infection_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
        
      }
      ## Infection
      if(event.time==first.infection.time){
        
        
        
        infection <- which.min(infection_times)
        
        ## record subsequent infection if in focus bay
        if(infection%%nrow(infection_status)!=focus_bay){
          if(outbreak==FALSE){
            outbreak <- TRUE
          }
          other.bay.infections <- other.bay.infections + 1
        }else{
          focus_bay.infections <- focus_bay.infections + 1
        }
        
        infection_status[infection] <- "Latent"
        infection_times[infection] <- Inf
        
        ## Additional Transfer + Discharge LOS HERE!!
        
        latent.times[infection] <- T + latent.period[infection]
        
      }
      ## Departure from the bay
      if(event.time==first.discharge.time){
        
        discharge <- which.min(discharge.times)
        
        if((infection_status[discharge]%in%infectious_states) & ((discharge%%n.bay)==focus_bay)){
          infectious.discharged <- infectious.discharged + 1
        }
        if((infection_status[discharge]%in%c("Latent",infectious_states)) & ((discharge%%n.bay)==focus_bay)){
          infected.discharged <- infected.discharged + 1
        }
        discharges <- discharges + 1
        
        infection_status[discharge] <- "Empty"
        infection_times[discharge] <- Inf
        
        infection_times <- infection.times.sample(T, infection_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
        
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
        
        if(transfer%%n.bay==focus_bay & restrictions==TRUE){
          if(queue_departures==TRUE){
            transfer.times[transfer] <- quarantine_time
            days.discharges.delayed <- days.discharges.delayed + (quarantine_time - T)
          }else{
            transfer.times[transfer] <- Inf
          }
          
        }else{
          if((infection_status[transfer]%in%infectious_states) & ((transfer%%n.bay)==focus_bay)){
            infectious.transferred <- infectious.transferred + 1
          }
          if((infection_status[transfer]%in%c("Latent", infectious_states)) & ((transfer%%n.bay)==focus_bay)){
            infected.transferred <- infected.transferred + 1
          }
          transfers <- transfers + 1
          
          infection_status[transfer] <- "Empty"
          
          infection_times <- infection.times.sample(T, infection_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
          
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
        
        infection_status[infectious] <- infectious_states[1]
        
        infection_times <- infection.times.sample(T, infection_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
        
        latent.times[infectious] <- Inf
        latent.period[infectious] <- Inf
        incubation.times[infectious] <- T + incubation.period[infectious]
        
        
      }
      
      
      if(event.time==first.incubation.time){
        
        incubation <- which.min(incubation.times)
        
        infection_status[incubation] <- infection_states[which(infection_states==infectious_states[1]) + 1] 
        
        infection_times <- infection.times.sample(T, infection_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
        
        incubation.times[incubation] <- Inf
        
        
        if(length(infection_states)>(which(infection_states==infectious_states[1]) + 1)){
          early.symptomatic.times[incubation] <- T + early.symptomatic.period[incubation]
        }else{
          early.symptomatic.times[incubation] <- Inf
        }
      }
      
      if(event.time==first.early.symptomatic.time){
        
        early.symptomatic <- which.min(early.symptomatic.times)
        
        infection_status[early.symptomatic] <- infection_states[which(infection_states==infectious_states[1]) + 2] 
        
        infection_times <- infection.times.sample(T, infection_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
        
        early.symptomatic.times[early.symptomatic] <- Inf
        if((length(infection_states)>which(infection_states==infectious_states[1]) + 2)){
          late.symptomatic.times[early.symptomatic] <- T + late.symptomatic.period[incubation]
        }else{
          late.symptomatic.times[early.symptomatic] <- Inf
        }
      }
      
      if(event.time==first.late.symptomatic.time){
        
        recovered <- which.min(late.symptomatic.times)
        
        infection_status[recovered] <- "Recovered"
        
        infection_times <- infection.times.sample(T, infection_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
        
        
        
        late.symptomatic.times[recovered] <- Inf
      }
      
      if(event.time==end.restriction.time){
        restrictions <- FALSE
        end.restriction.time <- Inf
        infection_rates[4, ] <- unrestricted.infection.rate
        
        if(queue_departures==FALSE){
          focus_bay_occupants <- which(!infection_status[focus_bay, ]%in%c("Empty", "No Bed"))
          transfer.times[focus_bay, focus_bay_occupants] <- T + transfer.period[focus_bay, focus_bay_occupants]
        }
        
        
        
        
        infection_times <- infection.times.sample(T, infection_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
      }
    }
    
  }
  
  colnames(Metrics) <- c("Focus Bay infections", "Other bay infections", "New arrivals", "Blocked arrivals", "Unused Bed Days in the focus bay", 
                         "Unused bed days elsewhere in the ward", "Outbreak occurred", "Infectious discharges", "Infectious transfers",
                         "Infected discharges", "Infected transfers", "Days of delayed discharges", "Discharges", "Transfers")
  rownames(Metrics) <- NULL
  
  
  if(return_metrics==TRUE){
    return(Metrics)
  }else{
    return(infection_status)
  }
  
}