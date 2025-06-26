#' This function determines the attributes of patients who are in the ward at the start of the simulation
#'
#'INPUTS
#' infection_status - a matrix of strings representing the infection status of each bed in the ward
#' The matrix is of size n x m where n is the number of bays in the ward (including side rooms) and m is the largest size of any of these wards
#' A row of the matrix represents the infection status of beds in that bay. Row 1 always represents side rooms.
#' An element can be either "Empty", "Susceptible", "Latent", in an infectious state, "Recovered" or "No bed" if no such bed exists
#' e.g. infection_status <- rbind(c("Susceptible", "No bed"), c("Infectious", "Infectious"),c("Susceptible", "Susceptible"))
#' represents a ward with 1 side room with a susceptible patient, 1 ward with 2 infectious patients and another ward with 2 susceptible patients
#' 
#' n_infectious_states - positive integer, either 1, 2 or 3. The number of infectious states.
#' 
#' #' transfer_rates - vector of two positive numbers, representing the rate at which uninfected and infected patients are transferred from the ward
#' i.e. transfer_rates = c(uninfected.transfer.rate, infected.transfer.rate)
#' 
#' #' discharge_rates - vector of two positive numbers, representing the rate at which uninfected and infected patients are discharge from the ward
#' i.e. discharge_rates = c(uninfected.discharge.rate, infected.discharge.rate)
#' 
#' recovery_rates - vector of positive numbers, representing the latent rate and recovery rates from each infectious period.
#' The length of this vector is equal to 1 + number of infectious periods
#' i.e. if there is 1 infectious period, recovery_rates=c(latent.rate, recovery.from.infection.rate)
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
#' IF LOS_distribution="Exponential", this does not need to be specified.
#' 
#' LOS_distribution - a character representing the distribution of the transfer and ischarge times of patients from the ward
#' This must be either "Exponential", "Gamma" or "Lognormal"
#' latent_distribution - character representing the distrubiton of the latent period duration.
#' This must be either "Exponential" or "Gamma"
#' 
#' OUTPUT
#' A list of matrices of the same diensions as infection_status representing patient attributes, in order:
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
initial.patient.attributes <- function(infection_status,n_infectious_states, transfer_rates, discharge_rates, recovery_rates,
                                        discharge_SD=c(1, 1), 
                                       transfer_SD=c(1, 1), latent_SD=1, LOS_distribution="Exponential", latent_distribution="Exponential"){
  
  if(n_infectious_states==1){
    infectious_states <- "Infectious"
  }else if(n_infectious_states==2){
    infectious_states <- c("Pre-symptomatic", "Symptomatic")
  }else if(n_infectious_states==3){
    infectious_states <- c("Pre-symptomatic", "Early symptomatic", "Late symptomatic")
  }
  
  infection_states <- c("Empty", "Susceptible", "Latent", infectious_states, "Recovered")
  
  mu.ward <- log((1/transfer_rates[1]^2)/sqrt(transfer_SD[1]^2 + 1/transfer_rates[1]^2))
  sigma.ward <- sqrt(log((transfer_SD[1]*transfer_rates[1])^2 + 1))
  scale.ward <- transfer_SD[1]^2 * transfer_rates[1]
  shape.ward <- 1/(transfer_rates[1]^2 * transfer_SD[1]^2)
  
  mu.ward.infectious <- log((1/transfer_rates[2]^2)/sqrt(transfer_SD[2]^2 + 1/transfer_rates[2]^2))
  sigma.ward.infectious <- sqrt(log((transfer_SD[2]*transfer_rates[2])^2 + 1))
  scale.ward.infectious <- transfer_SD[2]^2 * transfer_rates[2]
  shape.ward.infectious <- 1/(transfer_rates[2]^2 * transfer_SD[2]^2)
  
  mu.ward.discharge <- log((1/discharge_rates[1]^2)/sqrt(discharge_SD[1]^2 + 1/discharge_rates[1]^2))
  sigma.ward.discharge <- sqrt(log((discharge_SD[1]*discharge_rates[1])^2 + 1))
  scale.ward.discharge <- discharge_SD[1]^2 * discharge_rates[1]
  shape.ward.discharge <- 1/(discharge_rates[1]^2 * discharge_SD[1]^2)
  
  mu.ward.discharge.infectious <- log((1/discharge_rates[2]^2)/sqrt(discharge_SD[2]^2 + 1/discharge_rates[2]^2))
  sigma.ward.discharge.infectious <- sqrt(log((discharge_SD[2]*discharge_rates[2])^2 + 1))
  scale.ward.discharge.infectious <- discharge_SD[2]^2 * discharge_rates[2]
  shape.ward.discharge.infectious <- 1/(discharge_rates[2]^2 * discharge_SD[2]^2)
  
  
  scale.latent <- latent_SD^2 * recovery_rates[1]
  shape.latent <- 1/(recovery_rates[1]^2 * latent_SD^2)
  
  latent_period <- matrix(Inf, nrow=nrow(infection_status), ncol=ncol(infection_status))
  incubation_period <- matrix(Inf, nrow=nrow(infection_status), ncol=ncol(infection_status))
  early_symptomatic_period <- matrix(Inf, nrow=nrow(infection_status), ncol=ncol(infection_status))
  late_symptomatic_period <- matrix(Inf, nrow=nrow(infection_status), ncol=ncol(infection_status))
  transfer_period <- matrix(Inf, nrow=nrow(infection_status), ncol=ncol(infection_status))
  discharge_period <- matrix(Inf, nrow=nrow(infection_status), ncol=ncol(infection_status))
  
  
  occupied.beds <- which(!infection_status%in%c("No Bed", "Empty"))
  n.occupied.beds <- length(occupied.beds)
  
  if(n.occupied.beds>0){
    
    if(recovery_rates[1]>0){
      if(latent_distribution=="Exponential"){
        latent_period[occupied.beds] <- rexp(n.occupied.beds, recovery_rates[1])
      }else if(latent_distribution=="Gamma"){
        latent_period[occupied.beds] <- rgamma(n.occupied.beds, shape=shape.latent, scale=scale.latent)
      }
    }
    
    if(recovery_rates[2]>0){
      incubation_period[occupied.beds] <- rexp(n.occupied.beds, recovery_rates[2])
    }
    
    if(length(recovery_rates)>2){
      early_symptomatic_period[occupied.beds] <- rexp(n.occupied.beds, recovery_rates[3])
    }
    
    if(length(recovery_rates)>3){
      late_symptomatic_period[occupied.beds] <- rexp(n.occupied.beds, recovery_rates[4])
    }
    
    if(transfer_rates[1]>0){
      if(LOS_distribution=="Exponential"){
        transfer_period[occupied.beds] <- rexp(n.occupied.beds, transfer_rates[1])
      }else if(LOS_distribution=="Lognormal"){
        transfer_period[occupied.beds] <- rlnorm(n.occupied.beds, mu.ward, sigma.ward)
      }else if(LOS_distribution=="Gamma"){
        transfer_period[occupied.beds] <- rgamma(n.occupied.beds, shape=shape.ward, scale=scale.ward)
      }
    }
    
    if(discharge_rates[1]>0){
      if(LOS_distribution=="Exponential"){
        discharge_period[occupied.beds] <- rexp(n.occupied.beds, discharge_rates[1])
      }else if(LOS_distribution=="Lognormal"){
        discharge_period[occupied.beds] <- rlnorm(n.occupied.beds, mu.ward.discharge, sigma.ward.discharge)
      }else if(LOS_distribution=="Gamma"){
        discharge_period[occupied.beds] <- rgamma(n.occupied.beds, shape=shape.ward.discharge, scale=scale.ward.discharge)
      }
    }
    
  }

  initial.patient.attributes <- list(transfer_period, discharge_period, latent_period, incubation_period, early_symptomatic_period, late_symptomatic_period)
  
  return(initial.patient.attributes)
}