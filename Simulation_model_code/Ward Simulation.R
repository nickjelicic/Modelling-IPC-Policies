#' This function is the main body of the simulation and determines the key metrics for the ward
#'
#'INPUTS
#' focus_bay - a positive integer between 1 and nrow(bed_status) representing the focus bay
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
#' bed_status - a matrix of strings representing the infection status of each bed in the ward
#' The matrix is of size n x m where n is the number of bays in the ward (including side rooms) and m is the largest size of any of these wards
#' A row of the matrix represents the infection status of beds in that bay. Row 1 always represents side rooms.
#' An element can be either "Empty", "Susceptible", "Latent", in an infectious state, "Recovered" or "No bed" if no such bed exists
#' e.g. bed_status <- rbind(c("Susceptible", "No bed"), c("Infectious", "Infectious"),c("Susceptible", "Susceptible"))
#' represents a ward with 1 side room with a susceptible patient, 1 ward with 2 infectious patients and another ward with 2 susceptible patients
#' 
#' init_patient_attributes - A list of matrices of the same diensions as bed_status representing patient attributes, in order:
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

ward_simulation_paired <- function(focus_bay, n_infectious_states,
                            infection_rates, time_seq, 
                            bed_status, init_patient_attributes, infection_times,
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
  
  all_states <- c("Empty", "Susceptible", "Latent", infectious_states, "Recovered")
  
  ## Initial Conditions
  #####
  patient_attributes <- init_patient_attributes
  
  transfer_period <- patient_attributes[[1]]
  discharge_period <- patient_attributes[[2]]
  latent_period <- patient_attributes[[3]]
  incubation_period <- patient_attributes[[4]]
  early_symptomatic_period <- patient_attributes[[5]]
  late_symptomatic_period <- patient_attributes[[6]]
  
  transfer_times <- transfer_period
  discharge_times <- discharge_period
  
  latent_times <- latent_period
  latent_times[which(bed_status!="Latent")] <- Inf
  
  incubation_times <- incubation_period
  incubation_times[which(bed_status!=infectious_states[1])] <- Inf
  
  early_symptomatic_times <- early_symptomatic_period
  if(length(infectious_states)>1){
    early_sympatomatic_times[which(bed_status!=infectious_states[2])] <- Inf
  }
  
  late_symptomatic_times <- late_symptomatic_period
  if(length(infectious_states)>2){
    late_sympatomatic_times[which(bed_status!=infectious_states[3])] <- Inf
  }
  
  
  scheduled_arrival_times <- new_patient_attributes[[1]]
  arrival_transfer_times <- new_patient_attributes[[2]]
  arrival_discharge_times <- new_patient_attributes[[3]]
  arrival_latent_period <- new_patient_attributes[[4]]
  arrival_incubation_period <- new_patient_attributes[[5]]
  arrival_early_symptomatic_period <- new_patient_attributes[[6]]
  arrival_late_symptomatic_period <- new_patient_attributes[[7]]
  ## 
  if(restrictions==TRUE){
    end_restriction_time <- quarantine_time
    
    unrestricted_infection_rate <- infection_rates[4, ]
    infection_rates[4, ] <- infection_rates[4, ]*close_bay_coef
    
  }else{
    end_restriction_time <- Inf
  }
  
  current_time <- 0
  focus_bay_infections <- 0
  other_bay_infections <- 0
  new_arrivals_admitted <- 0
  blocked_arrivals <- 0
  unused_bed_days_focus_bay <- 0
  unused_bed_days_other_bays <- 0
  infectious_discharged <- 0
  infectious_transferred <- 0
  infected_discharged <- 0
  infected_transferred <- 0
  days_discharges_delayed <- 0
  discharges <- 0
  transfers <- 0
  ward_occupancy <- 0
  outbreak <- FALSE
  
  time_seq <- sort(time_seq, decreasing=FALSE)
  end_time <- time_seq[1]
  
  metrics_table <- NULL
  
  bay_occupancy <- length(which(bed_status!="Empty"))
  
  Finish <- FALSE
  
  n_bay <- nrow(bed_status)

  
  #####
  ##simulate bay
  
  
  while(Finish==FALSE){
    
    if(length(scheduled_arrival_times)>0){
      first_arrival_time <- min(scheduled_arrival_times)
    }else{
      first_arrival_time <- Inf
    }
    
    first_infection_time <- min(infection_times)
    first_transfer_time <- min(transfer_times)
    first_discharge_time <- min(discharge_times)
    
    first_latent_time <- min(latent_times)
    first_incubation_time <- min(incubation_times)
    
    
    first_early_symptomatic_time <- min(early_symptomatic_times)
    first_late_symptomatic_time <- min(late_symptomatic_times)
    
    
    event_time <- min(first_arrival_time, first_infection_time, first_transfer_time, first_discharge_time,
                      first_latent_time, first_incubation_time, 
                      first_early_symptomatic_time, first_late_symptomatic_time, end_restriction_time)
    
    n_empty_beds_focus_bay <- length(which(bed_status[focus_bay, ]=="Empty"))
    n_empty_beds_other_bays <- length(which(bed_status[-focus_bay, ]=="Empty"))
    
    if(event_time > end_time){
      time_seq <- time_seq[-1]
      
      unused_bed_days_focus_bay <- unused_bed_days_focus_bay + (end_time-current_time) * n_empty_beds_focus_bay
      unused_bed_days_other_bays <- unused_bed_days_other_bays + (end_time-current_time) * n_empty_beds_other_bays
      
      current_time <- end_time
      
      if(length(time_seq)>0){
        end_time <- time_seq[1]
      }else{
        Finish <- TRUE
      }
      
      
      
      metrics_row <- c(focus_bay_infections, other_bay_infections, new_arrivals_admitted, blocked_arrivals, unused_bed_days_focus_bay, unused_bed_days_other_bays, outbreak, infectious_discharged,
                   infectious_transferred, infected_discharged, infected_transferred, days_discharges_delayed, discharges, transfers)
      
      metrics_table <- rbind(metrics_table, metrics_row)
      
    }else{
      
      unused_bed_days_focus_bay <- unused_bed_days_focus_bay + (event_time - current_time) * n_empty_beds_focus_bay
      unused_bed_days_other_bays <- unused_bed_days_other_bays + (event_time - current_time) * n_empty_beds_other_bays
      
      current_time <- event_time
      
      if(event_time==first_arrival_time){
        
        
        ## Arrivals should be placed in other beds on the ward if available
        empty_beds <- which(bed_status=="Empty")
        
        empty_beds_side_rooms <- empty_beds[which(empty_beds%%nrow(bed_status)==1)]
        empty_beds_focus_bay <- empty_beds[which(empty_beds%%nrow(bed_status)==2)]
        
        empty_beds_other_bays <- empty_beds[which(empty_beds%%nrow(bed_status)==0)]
        
        
        new_patient_location <- NULL
        
        if(random_arrivals==TRUE){
          
          if(restrictions==TRUE){
            
            available_beds <- c(empty_beds_side_rooms, empty_beds_other_bays)
          }else{
            available_beds <- c(empty_beds_side_rooms, empty_beds_focus_bay, empty_beds_other_bays)
          }
          if(length(available_beds)==0){
            blocked_arrivals <- blocked_arrivals + 1
          }else if(length(available_beds)==1){
            new_patient_location <- available_beds
          }else{
            new_patient_location <- sample(available_beds, 1)
          }
          
        }else{
          
          if(length(empty_beds_other_bays)>0){
            
            if(length(empty_beds_other_bays)==1){
              new_patient_location <- empty_beds_other_bays
            }else{
              new_patient_location <- sample(empty_beds_other_bays, 1)
            }
          }else if(length(empty_beds_focus_bay)>0 & restrictions==FALSE){
            if(length(empty_beds_focus_bay)==1){
              new_patient_location <- empty_beds_focus_bay
            }else{
              new_patient_location <- sample(empty_beds_focus_bay, 1)
            }
          }else if(length(empty_beds_side_rooms)>0){
            if(length(empty_beds_side_rooms)==1){
              new_patient_location <- empty_beds_side_rooms
            }else{
              new_patient_location <- sample(empty_beds_side_rooms, 1)
            }
          }else{
            blocked_arrivals <- blocked_arrivals + 1
          }
          
        }
        
        
        if(length(new_patient_location)>0){
          ward_occupancy <- ward_occupancy + 1
          new_arrivals_admitted <- new_arrivals_admitted + 1
          
          bed_status[new_patient_location] <- "Susceptible"
          
          
          transfer_times[new_patient_location] <- current_time + arrival_transfer_times[1]
          discharge_times[new_patient_location] <- current_time + arrival_discharge_times[1]
          
          transfer_period[new_patient_location] <- arrival_transfer_times[1] 
          discharge_period[new_patient_location] <- arrival_discharge_times[1]
          latent_period[new_patient_location] <- arrival_latent_period[1]
          incubation_period[new_patient_location] <- arrival_incubation_period[1]
          early_symptomatic_period[new_patient_location] <- arrival_early_symptomatic_period[1]
          late_symptomatic_period[new_patient_location] <- arrival_late_symptomatic_period[1]
        }
        
        ## Priority order of placing new patients (if occupied move to next one)
        # 1. Other open bay
        # 2. Focus bay
        # 3. Side Room

        scheduled_arrival_times <- scheduled_arrival_times[-1]
        arrival_transfer_times <- arrival_transfer_times[-1]
        arrival_discharge_times <- arrival_discharge_times[-1]
        arrival_latent_period <- arrival_latent_period[-1]
        arrival_incubation_period <- arrival_incubation_period[-1]
        arrival_early_symptomatic_period <- arrival_early_symptomatic_period[-1]
        arrival_late_symptomatic_period <- arrival_late_symptomatic_period[-1]
        
        infection_times <- infection_times_sample(current_time, bed_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
        
      }
      ## Infection
      if(event_time==first_infection_time){
        
        
        
        infection <- which.min(infection_times)
        
        ## record subsequent infection if in focus bay
        if(infection%%nrow(bed_status)!=focus_bay){
          if(outbreak==FALSE){
            outbreak <- TRUE
          }
          other_bay_infections <- other_bay_infections + 1
        }else{
          focus_bay_infections <- focus_bay_infections + 1
        }
        
        bed_status[infection] <- "Latent"
        infection_times[infection] <- Inf
        
        ## Additional Transfer + Discharge LOS HERE!!
        
        latent_times[infection] <- current_time + latent_period[infection]
        
      }
      ## Departure from the bay
      if(event_time==first_discharge_time){
        
        discharge <- which.min(discharge_times)
        
        if((bed_status[discharge]%in%infectious_states) & ((discharge%%n_bay)==focus_bay)){
          infectious_discharged <- infectious_discharged + 1
        }
        if((bed_status[discharge]%in%c("Latent",infectious_states)) & ((discharge%%n_bay)==focus_bay)){
          infected_discharged <- infected_discharged + 1
        }
        discharges <- discharges + 1
        
        bed_status[discharge] <- "Empty"
        infection_times[discharge] <- Inf
        
        infection_times <- infection_times_sample(current_time, bed_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
        
        discharge_times[discharge] <- Inf
        transfer_times[discharge] <- Inf
        latent_times[discharge] <- Inf
        incubation_times[discharge] <- Inf
        early_symptomatic_times[discharge] <- Inf
        late_symptomatic_times[discharge] <- Inf
        
        latent_period[discharge] <- Inf
        incubation_period[discharge] <- Inf
        early_symptomatic_period[discharge] <- Inf
        late_symptomatic_period[discharge] <- Inf
        
      }
      if(event_time==first_transfer_time){
        
        transfer <- which.min(transfer_times)
        
        if(transfer%%n_bay==focus_bay & restrictions==TRUE){
          if(queue_departures==TRUE){
            transfer_times[transfer] <- quarantine_time
            days_discharges_delayed <- days_discharges_delayed + (quarantine_time - current_time)
          }else{
            transfer_times[transfer] <- Inf
          }
          
        }else{
          if((bed_status[transfer]%in%infectious_states) & ((transfer%%n_bay)==focus_bay)){
            infectious_transferred <- infectious_transferred + 1
          }
          if((bed_status[transfer]%in%c("Latent", infectious_states)) & ((transfer%%n_bay)==focus_bay)){
            infected_transferred <- infected_transferred + 1
          }
          transfers <- transfers + 1
          
          bed_status[transfer] <- "Empty"
          
          infection_times <- infection_times_sample(current_time, bed_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
          
          discharge_times[transfer] <- Inf
          transfer_times[transfer] <- Inf
          latent_times[transfer] <- Inf
          incubation_times[transfer] <- Inf
          early_symptomatic_times[transfer] <- Inf
          late_symptomatic_times[transfer] <- Inf
          
          latent_period[transfer] <- Inf
          incubation_period[transfer] <- Inf
          early_symptomatic_period[transfer] <- Inf
          late_symptomatic_period[transfer] <- Inf
        }
        
        
        
        
      }
      if(event_time==first_latent_time){
        
        infectious <- which.min(latent_times)
        
        bed_status[infectious] <- infectious_states[1]
        
        infection_times <- infection_times_sample(current_time, bed_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
        
        latent_times[infectious] <- Inf
        latent_period[infectious] <- Inf
        incubation_times[infectious] <- current_time + incubation_period[infectious]
        
        
      }
      
      
      if(event_time==first_incubation_time){
        
        incubation <- which.min(incubation_times)
        
        bed_status[incubation] <- all_states[which(all_states==infectious_states[1]) + 1] 
        
        infection_times <- infection_times_sample(current_time, bed_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
        
        incubation_times[incubation] <- Inf
        
        
        if(length(all_states)>(which(all_states==infectious_states[1]) + 1)){
          early_symptomatic_times[incubation] <- current_time + early_symptomatic_period[incubation]
        }else{
          early_symptomatic_times[incubation] <- Inf
        }
      }
      
      if(event_time==first_early_symptomatic_time){
        
        early_symptomatic <- which.min(early_symptomatic_times)
        
        bed_status[early_symptomatic] <- all_states[which(all_states==infectious_states[1]) + 2] 
        
        infection_times <- infection_times_sample(current_time, bed_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
        
        early_symptomatic_times[early_symptomatic] <- Inf
        if((length(all_states)>which(all_states==infectious_states[1]) + 2)){
          late_symptomatic_times[early_symptomatic] <- current_time + late_symptomatic_period[incubation]
        }else{
          late_symptomatic_times[early_symptomatic] <- Inf
        }
      }
      
      if(event_time==first_late_symptomatic_time){
        
        recovered <- which.min(late_symptomatic_times)
        
        bed_status[recovered] <- "Recovered"
        
        infection_times <- infection_times_sample(current_time, bed_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
        
        
        
        late_symptomatic_times[recovered] <- Inf
      }
      
      if(event_time==end_restriction_time){
        restrictions <- FALSE
        end_restriction_time <- Inf
        infection_rates[4, ] <- unrestricted_infection_rate
        
        if(queue_departures==FALSE){
          focus_bay_occupants <- which(!bed_status[focus_bay, ]%in%c("Empty", "No Bed"))
          transfer_times[focus_bay, focus_bay_occupants] <- current_time + transfer_period[focus_bay, focus_bay_occupants]
        }
        
        
        
        
        infection_times <- infection_times_sample(current_time, bed_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay)
      }
    }
    
  }
  
  colnames(metrics_table) <- c("Focus Bay infections", "Other bay infections", "New arrivals", "Blocked arrivals", "Unused Bed Days in the focus bay", 
                         "Unused bed days elsewhere in the ward", "Outbreak occurred", "Infectious discharges", "Infectious transfers",
                         "Infected discharges", "Infected transfers", "Days of delayed discharges", "Discharges", "Transfers")
  rownames(metrics_table) <- NULL
  
  
  if(return_metrics==TRUE){
    return(metrics_table)
  }else{
    return(bed_status)
  }
  
}