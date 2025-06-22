#' This function samples the time at which patients in the ward contract an infection
#'
#'INPUTS
#' current_time - a positive number, the current time of the simulation
#' 
#' infection_status - a matrix of strings representing the infection status of each bed in the ward
#' The matrix is of size n x m where n is the number of bays in the ward (including side rooms) and m is the largest size of any of these wards
#' A row of the matrix represents the infection status of beds in that bay. Row 1 always represents side rooms.
#' An element can be either "Empty", "Susceptible", "Latent", in an infectious state, "Recovered" or "No bed" if no such bed exists
#' e.g. infection_status <- rbind(c("Susceptible", "No bed"), c("Infectious", "Infectious"),c("Susceptible", "Susceptible"))
#' represents a ward with 1 side room with a susceptible patient, 1 ward with 2 infectious patients and another ward with 2 susceptible patients
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
#' focus_bay - a positive integer between 1 and nrow(infection_status) representing the focus bay
#' 
#' n_infectious_states - positive integer, either 1, 2 or 3. The number of infectious states.
#' 
#' spread_to_focus_bay - logical. If FALSE, infections can only spread from the focus bay. 
#' If TRUE, infections can spread anywhere in the ward
#' 
#' OUTPUT
#' A matrix of positive numbers representing the time at which each patient will contract an infection
#' The matrix is of size n x m where n is the number of bays in the ward (including side rooms) and m is the largest size of any of these wards
#' A row of the matrix represents the infection status of beds in that bay. Row 1 always represents side rooms.
#' If the bed cannot become infected (either it is empty, does not exist, or the patient is already infected) then the time is set to infinity
#'
#'
infection.times.sample <- function(current_time, infection_status, infection_rates, focus_bay, n_infectious_states, spread_to_focus_bay){
  
  if(n_infectious_states==1){
    infectious_states <- "Infectious"
  }else if(n_infectious_states==2){
    infectious_states <- c("Pre-symptomatic", "Symptomatic")
  }else if(n_infectious_states==3){
    infectious_states <- c("Pre-symptomatic", "Early symptomatic", "Late symptomatic")
  }
  
  infection_states <- c("Empty", "Susceptible", "Latent", infectious_states, "Recovered")
  
  infection.times <- matrix(Inf, nrow=nrow(infection_status), ncol=ncol(infection_status))
  n.bay <- nrow(infection_status)
  ward.occupancy <- length(which(infection_status %in% infection_states[-1]))
  
  if(spread_to_focus_bay==FALSE){
    
    for(bay in 1:n.bay){
      infection.hazard <- infection_rates[1]
      for(k in 1:length(infectious_states)){
        
        if(bay==1){
          infection.hazard <- infection.hazard + infection_rates[2, k]*length(which(infection_status[focus_bay, ]==infectious_states[k]))
        }else if(bay==focus_bay){
          infection.hazard <- infection.hazard + infection_rates[3, k]*length(which(infection_status[focus_bay, ]==infectious_states[k]))
        }else{
          infection.hazard <- infection.hazard + infection_rates[4, k]*length(which(infection_status[focus_bay, ]==infectious_states[k]))
        }
      }
      
      susceptibles <- which(infection_status[bay, ]=="Susceptible")
      if(length(susceptibles)>0 & infection.hazard>0){
        infection.times[bay, susceptibles] <- current_time + rexp(length(susceptibles), infection.hazard)
      }
    }
   
  }else{
    
    for(bay in 1:n.bay){
      infection.hazard <- infection_rates[1]
      
      for(k in 1:length(infectious_states)){
        if(bay==1){
          infection.hazard <- infection.hazard + infection_rates[2, k]*length(which(infection_status[1, ]==infectious_states[k])) + 
            infection_rates[2, k]*length(which(infection_status[-1, ]==infectious_states[k]))
        }else{
          infection.hazard <- infection.hazard + infection_rates[2, k]*length(which(infection_status[1, ]==infectious_states[k])) + 
            infection_rates[3, k]*length(which(infection_status[bay, ]==infectious_states[k])) + 
            infection_rates[4, k]*length(which(infection_status[-c(1, bay), ]==infectious_states[k]))
        }
      }
      susceptibles <- which(infection_status[bay, ]=="Susceptible")
      if(length(susceptibles)>0 & infection.hazard>0){
        infection.times[bay, susceptibles] <- current_time + rexp(length(susceptibles), infection.hazard)
      }
    }
  }
 
  return(infection.times)
}