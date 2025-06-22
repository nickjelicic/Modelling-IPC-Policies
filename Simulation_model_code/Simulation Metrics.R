#' This is the main function that runs the simulation model to determine the infection and flow dynamics within the ward
#' 
#' INPUTS
#' 
#' ward_configuration - vector of positive integers of length equal to the number of bays in the ward
#' Represents the number of beds in each bay in the ward, 
#' where the first element represents the number of side rooms and the second represents the number of beds in the focus bay
#' 
#' bay_occupants - vector of positive integers of the same length as ward_configuration
#' represents the number of patients initially in each bay. 
#' 
#'
#' n_infectious_states - positive integer, either 1, 2 or 3. The number of infectious states.
#' 
#' infection_distribution - vector of probabilities of length equal to the 3 + n_infectious_states
#'  representing the infection state distribution of patients in the focus bay
#'  The kth component of the vector is equal to the probability that a given patient is in infection state k+1
#' 
#' #' arrival_rate - positive number, the daily arrival rate of patients to the ward
#' 
#' transfer_rates - vector of two positive numbers, representing the rate at which uninfected and infected patients are transferred from the ward
#' i.e. transfer_rates = c(uninfected.transfer.rate, infected.transfer.rate)
#' 
#' discharge_rates - vector of two positive numbers, representing the rate at which uninfected and infected patients are discharge from the ward
#' i.e. discharge_rates = c(uninfected.discharge.rate, infected.discharge.rate)
#' 
#' #' infection_rates - a matrix of positive numbers representing the infection rates governing infection transmission within the ward
#' The matrix is of size 4 x m where m is the number of infectious states
#' The first row represents the background infection rate
#' The second row is the side room infection rate
#' The third row is the within-bay infection rate
#' The fourth row is the between open bay infection rate
#' The jth column represents the infection rate if the infectious patient is in the jth infectious state
#' i.e if there is only one infectious state, 
#' infection_rates <- rbind(background_infection_rate, SR_infection_rate, within_bay_infection_rate, between_OB_infection_rate)
#' 
#' recovery_rates - vector of positive numbers, representing the latent rate and recovery rates from each infectious period.
#' The length of this vector is equal to 1 + number of infectious periods
#' i.e. if there is 1 infectious period, recovery_rates=c(latent.rate, recovery.from.infection.rate) 
#' 
#' restrictions - logical. If TRUE, then the focus is bay is closed until the quarantine time.
#' If FALSE, the focus bay is open
#' 
#' quarantine_time - positive number representing the time until the focus bay is reopened if it is closed at the start of the simulation
#' If restrictions=FALSE, this need not be specified
#' 
#' spread_coef - positive number equal to the infection rate between open bays divided by the within-bay infection rate
#' 
#' close_bay_coef - positive number equal to the infection rate from a closed bay divided by the infection rate from an open bay
#' If restrictions=FALSE, this need not be specified
#' 
#' time_seq - a vector of positive numbers, representing various stopping times for the simulation
#' 
#' LOS_distribution - a character representing the distribution of the transfer and discharge times of patients from the ward
#' This must be either "Exponential", "Gamma" or "Lognormal"
#' latent_distribution - character representing the distribution of the latent period duration.
#' This must be either "Exponential" or "Gamma"
#' 
#' discharge_SD - vector of two positive numbers, representing the standard deviation for the distribution of the time until uninfected and infection patients are discharged home from ward
#' i.e. discharge_SD = c(uninfected.discharge_SD, infected.discharge_SD)
#' #' IF LOS_distribution="Exponential", this does not need to be specified.
#' 
#' transfer_SD - vector of two positive numbers, representing the standard deviation for the distribution of the time until uninfected and infection patients are transferred to another ward
#' i.e. transfer_SD = c(uninfected.transfer_SD, infected.transfer_SD)
#' IF LOS_distribution="Exponential", this does not need to be specified.
#' 
#' latent_SD - positive number, representing the standard deviation for the distribution of the latent distribution if it is gamma distributed
#' IF latent_distribution="Exponential", this does not need to be specified.
#' 
#' spread_to_focus_bay - logical. If FALSE, infections can only spread from the focus bay. 
#' If TRUE, infections can spread anywhere in the ward
#' 
#' queue_departures - logical. If TRUE, patients leave as soon as restrictions are lifted if they have exceeded their transfer period
#' If FALSE, their transfer period is resampled after restrictions are lifted
#'
#' OUTPUT
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
simulation.ward.metrics <- function(ward_configuration, bay_occupants,
                                    n_infectious_states,
                                                  infection_distribution, arrival_rate, transfer_rates, discharge_rates,
                                    infection_rates, recovery_rates, quarantine_time=0, spread_coef, close_bay_coef, time_seq, LOS_distribution,
                                    discharge_SD=rep(0.5, 2), transfer_SD=rep(1, 2),restrictions=FALSE, queue_departures, spread_to_focus_bay, 
                                    latent_distribution="Exponential", latent_SD=1){
  
  
  if(n_infectious_states==1){
    infectious_states <- "Infectious"
  }else if(n_infectious_states==2){
    infectious_states <- c("Pre-symptomatic", "Symptomatic")
  }else if(n_infectious_states==3){
    infectious_states <- c("Pre-symptomatic", "Early symptomatic", "Late symptomatic")
  }
  
  infection_states <- c("Empty", "Susceptible", "Latent", infectious_states, "Recovered")
  
  ## determine initial state of the bay
  if(nrow(infection_rates)<4){
    infection_rates <- rbind(infection_rates, 0)
  }
  infection_rates[4, ] <- infection_rates[3, ]*spread_coef
  infection_rates.rest <- infection_rates
  infection_rates.rest[4, ] <- infection_rates[4, ]*close_bay_coef
  
  if(restrictions==TRUE){
    infection_rates <- infection_rates.rest
  }
  
  infection_status <- matrix("No Bed", nrow=length(ward_configuration), ncol=max(ward_configuration))
  for(bay in 1:length(ward_configuration)){
    infection_status[bay, 1:ward_configuration[bay]] <- "Empty"
    infection_status[bay, 1:bay_occupants[bay]] <- "Susceptible"
  }
  
  if(bay_occupants[2]>0){
    for(i in 1:bay_occupants[2]){
      infection_status[2, i] <- infection_states[sample(2:length(infection_states),size=1, prob=infection_distribution)]
    }
  }
  
  focus_bay <- 2
  ## initial patient attributes
  init.patient.attributes <- initial.patient.attributes(infection_status, n_infectious_states, transfer_rates, discharge_rates, recovery_rates,
                                                        discharge_SD, 
                                                        transfer_SD, latent_SD, LOS_distribution, latent_distribution)
  
  ## infection times
  infection_times <- infection.times.sample(0, infection_status, infection_rates, 2, n_infectious_states, spread_to_focus_bay)
  
  ## new patient attributes
  new.patient.attributes <- arrival.patient.attributes(max(time_seq), arrival_rate, transfer_rates, discharge_rates, recovery_rates, discharge_SD, 
                                                       transfer_SD, latent_SD, LOS_distribution, latent_distribution)
  
  metrics <- ward.simulation.paired(focus_bay, n_infectious_states, infection_rates, time_seq, infection_status,
                                      init.patient.attributes, infection_times, restrictions, quarantine_time, close_bay_coef, spread_to_focus_bay,
                                      queue_departures, new.patient.attributes)
  
  
  return(metrics)
  
  
  
}