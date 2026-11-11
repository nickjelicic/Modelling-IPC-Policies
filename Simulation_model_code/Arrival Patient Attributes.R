

#' This function determines the attributes of patients that will arrive at the ward during the simulation
#'
#'INPUTS
#' stopping_time - positive number, the stopping time for the simulation
#' 
#' arrival_rate - positive number, the daily arrival rate of patients to the ward
#' 
#' transfer_rates - vector of two positive numbers, representing the rate at which uninfected and infected patients are transferred from the ward
#' i.e. transfer_rates = c(uninfected.transfer.rate, infected.transfer.rate)
#' 
#' discharge_rates - vector of two positive numbers, representing the rate at which uninfected and infected patients are discharge from the ward
#' i.e. discharge_rates = c(uninfected.discharge.rate, infected.discharge.rate)
#' 
#' recovery_rates - vector of postiive numbers, representing the latent rate and recovery rates from each infectious period.
#' The length of this vector is equal to 1 + number of infectious periods
#' i.e. if there is 1 infectious period, recovery_rates=c(latent.rate, recovery.from.infection.rate)
#' 
#' discharge_SD - vector of two positive numbers, representing the standard deviation for the distribution of the time until uninfected and infection patients are discharged home from ward
#' i.e. discharge_SD = c(uninfected.discharge_SD, infected.discharge_SD)
#' IF LOS_distribution="Exponential", this does not need to be specified.
#' 
#' transfer_SD - vector of two positive numbers, representing the standard deviation for the distribution of the time until uninfected and infection patients are transferred to another ward
#' i.e. transfer_SD = c(uninfected.transfer_SD, infected.transfer_SD)
#' IF LOS_distribution="Exponential", this does not need to be specified.
#' 
#' latent_SD - positive number, representing the standard deviation for the distribution of the latent distribution if it is gamma distributed
#' IF LOS_distribution="Exponential", this does not need to be specified.
#' 
#' #' LOS_distribution - a character representing the distribution of the transfer and ischarge times of patients from the ward
#' This must be either "Exponential", "Gamma" or "Lognormal"
#' 
#' latent_distribution - character representing the distrubiton of the latent period duration.
#' This must be either "Exponential" or "Gamma"
#'
#' OUTPUTS -  the attributes of the patients that will arrive at the ward as a list of vectors of positive numbers.
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
#'
#' @examples
#' set.seed()
arrival_patient_attributes <- function(stopping_time, arrival_rate, transfer_rates, discharge_rates, recovery_rates, discharge_SD=c(1, 1), 
                                       transfer_SD=c(1, 1), latent_SD=1, LOS_distribution="Exponential", latent_distribution="Exponential"){

# -------------------------------------------------------------------------

  
  
  mu_ward <- log((1/transfer_rates[1]^2)/sqrt(transfer_SD[1]^2 + 1/transfer_rates[1]^2))
  sigma_ward <- sqrt(log((transfer_SD[1]*transfer_rates[1])^2 + 1))
  scale_ward <- transfer_SD[1]^2 * transfer_rates[1]
  shape_ward <- 1/(transfer_rates[1]^2 * transfer_SD[1]^2)
  
  mu_ward_infectious <- log((1/transfer_rates[2]^2)/sqrt(transfer_SD[2]^2 + 1/transfer_rates[2]^2))
  sigma_ward_infectious <- sqrt(log((transfer_SD[2]*transfer_rates[2])^2 + 1))
  scale_ward_infectious <- transfer_SD[2]^2 * transfer_rates[2]
  shape_ward_infectious <- 1/(transfer_rates[2]^2 * transfer_SD[2]^2)
  
  mu_ward_discharge <- log((1/discharge_rates[1]^2)/sqrt(discharge_SD[1]^2 + 1/discharge_rates[1]^2))
  sigma_ward_discharge <- sqrt(log((discharge_SD[1]*discharge_rates[1])^2 + 1))
  scale_ward_discharge <- discharge_SD[1]^2 * discharge_rates[1]
  shape_ward_discharge <- 1/(discharge_rates[1]^2 * discharge_SD[1]^2)
  
  mu_ward_discharge_infectious <- log((1/discharge_rates[2]^2)/sqrt(discharge_SD[2]^2 + 1/discharge_rates[2]^2))
  sigma_ward_discharge_infectious <- sqrt(log((discharge_SD[2]*discharge_rates[2])^2 + 1))
  scale_ward_discharge_infectious <- discharge_SD[2]^2 * discharge_rates[2]
  shape_ward_discharge_infectious <- 1/(discharge_rates[2]^2 * discharge_SD[2]^2)
  
  
  scale_latent <- latent_SD^2 * recovery_rates[1]
  shape_latent <- 1/(recovery_rates[1]^2 * latent_SD^2)
  
  current_time <- 0
  scheduled_arrival_times <- NULL
  
  while(current_time <= stopping_time){
    new_arrival_time <- current_time + rexp(1, arrival_rate)
    
    if(new_arrival_time<=stopping_time){
      scheduled_arrival_times <- c(scheduled_arrival_times, new_arrival_time)
    }
    current_time <- new_arrival_time
  }
  
  n_arrivals <- length(scheduled_arrival_times)
  
  if(n_arrivals>0){
    if(recovery_rates[1]>0){
      if(latent_distribution=="Exponential"){
        arrival_latent_period <- rexp(n_arrivals, recovery_rates[1])
      }else if(latent_distribution=="Gamma"){
        arrival_latent_period <- rgamma(n_arrivals, shape=shape_latent, scale=scale_latent)
      }
    }else{
      arrival_latent_period <- rep(Inf, n_arrivals)
    }
    
    
    if(recovery_rates[2]>0){
      arrival_incubation_period <- rexp(n_arrivals, recovery_rates[2])
    }else{
      arrival_incubation_period <- rep(Inf, n_arrivals)
    }
    
    if(length(recovery_rates)>2){
      arrival_early_symptomatic_period <- rexp(n_arrivals, recovery_rates[3])
    }else{
      arrival_early_symptomatic_period <- rep(Inf, n_arrivals)
    }
    
    if(length(recovery_rates)>3){
      arrival_late_symptomatic_period <- rexp(n_arrivals, recovery_rates[4])
    }else{
      arrival_late_symptomatic_period <- rep(Inf, n_arrivals)
    }
    
    
    if(transfer_rates[1]>0){
      if(LOS_distribution=="Exponential"){
        arrival_transfer_times <- rexp(n_arrivals, transfer_rates[1])
      }else if(LOS_distribution=="Lognormal"){
        arrival_transfer_times <- rlnorm(n_arrivals, mu_ward, sigma_ward)
      }else if(LOS_distribution=="Gamma"){
        arrival_transfer_times <- rgamma(n_arrivals, shape=shape_ward, scale=scale_ward)
      }
    }else{
      arrival_transfer_times <- rep(Inf, n_arrivals)
    }
    
    if(discharge_rates[1]>0){
      if(LOS_distribution=="Exponential"){
        arrival_discharge_times <- rexp(n_arrivals, discharge_rates[1])
      }else if(LOS_distribution=="Lognormal"){
        arrival_discharge_times <- rlnorm(n_arrivals, mu_ward_discharge, sigma_ward_discharge)
      }else if(LOS_distribution=="Gamma"){
        arrival_discharge_times <- rgamma(n_arrivals, shape=shape_ward_discharge, scale=scale_ward_discharge)
      }
    }else{
      arrival_discharge_times <- rep(Inf, n_arrivals)
    }
    
    
  }else{
    arrival_latent_period <- NULL
    arrival_incubation_period <- NULL
    arrival_early_symptomatic_period <- NULL
    arrival_late_symptomatic_period <- NULL
    arrival_transfer_times <- NULL
    arrival_discharge_times <- NULL
  }
  
  return(list(scheduled_arrival_times, arrival_transfer_times, arrival_discharge_times, arrival_latent_period, arrival_incubation_period,
              arrival_early_symptomatic_period, arrival_late_symptomatic_period))
}