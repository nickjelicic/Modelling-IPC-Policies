
## Arrival patient attributes example
source("Arrival Patient Attributes.R")

set.seed(100)
arrival.patient.attributes(stopping_time=5, arrival_rate=1, transfer_rates=c(1/4, 1/4), discharge_rates=c(1/5, 1/5), recovery_rates=c(1/2, 1/3), 
                           discharge_SD=c(1, 1), transfer_SD=c(1, 1), latent_SD=1, LOS_distribution="Gamma", latent_distribution="Gamma")


#should return
#[[1]]
#[1] 0.9242116 1.6480488 1.7526937 4.8500560

#[[2]]
#[1] 4.441285 4.563849 2.997607 3.997388

#[[3]]
#[1] 4.871021 6.711854 6.186192 5.232374

#[[4]]
#[1] 1.454069 2.060742 1.248095 2.182870

#[[5]]
#[1] 3.3697398 3.3931429 1.1417432 0.2148669

#[[6]]
#[1] Inf Inf Inf Inf

#[[7]]
#[1] Inf Inf Inf Inf



## Infection Times example
source("Infection Times.R")



infection_status <- rbind(c("Susceptible", "Susceptible"), 
                          c("Infectious", "Infectious"),
                          c("Susceptible", "Susceptible"))
infection_rates <- rbind(0.05, 0.1, 0.5, 0.2)
set.seed(1000)
infection.times.sample(current_time=0, infection_status, infection_rates, focus_bay=2, n_infectious_states=1, spread_to_focus_bay=TRUE)


#should return
#       [,1]     [,2]
#[1,] 4.018648 2.070772
#[2,]      Inf      Inf
#[3,] 5.416410 4.807220



## Initial Patient Attributes example
source("Initial Patient Attributes.R")

infection_status <- rbind(c("Susceptible", "Susceptible"), 
                          c("Infectious", "Infectious"),
                          c("Susceptible", "Susceptible"))
set.seed(100)
initial.patient.attributes(infection_status,n_infectious_states=1, transfer_rates=c(1/4, 1/4), discharge_rates=c(1/5, 1/5), recovery_rates=c(1/2, 1/3),
  discharge_SD=c(1, 1), 
  transfer_SD=c(1, 1), latent_SD=1, LOS_distribution="Gamma", latent_distribution="Gamma")

# returns
#[[1]]
#[,1]     [,2]
#[1,] 4.441285 3.997388
#[2,] 4.563849 3.846199
#[3,] 2.997607 5.712800

#[[2]]
#[,1]     [,2]
#[1,] 6.186192 5.685571
#[2,] 5.232374 5.162760
#[3,] 3.849565 5.695539

#[[3]]
#[,1]     [,2]
#[1,] 1.3117667 1.861127
#[2,] 0.5809215 2.060742
#[3,] 2.6778096 1.248095

#[[4]]
#[,1]      [,2]
#[1,] 1.014130 3.3931429
#[2,] 6.069575 1.1417432
#[3,] 3.369740 0.2148669

#[[5]]
#[,1] [,2]
#[1,]  Inf  Inf
#[2,]  Inf  Inf
#[3,]  Inf  Inf

#[[6]]
#[,1] [,2]
#[1,]  Inf  Inf
#[2,]  Inf  Inf
#[3,]  Inf  Inf



## Paired ward simulation
source("Arrival Patient Attributes.R")
source("Infection Times.R")
source("Simulation Metrics.R")
source("Ward Simulation.R")
source("Initial Patient Attributes.R")
infection_status <- rbind(c("Susceptible", "Susceptible"), 
                          c("Infectious", "Infectious"),
                          c("Susceptible", "Susceptible"))
infection_rates <- rbind(0.05, 0.1, 0.5, 0.2)

set.seed(100)
init_patient_attributes <- initial.patient.attributes(infection_status,n_infectious_states=1, transfer_rates=c(1/4, 1/4), 
                                                      discharge_rates=c(1/5, 1/5), recovery_rates=c(1/2, 1/3), discharge_SD=c(1, 1), 
                                                      transfer_SD=c(1, 1), latent_SD=1, LOS_distribution="Gamma", latent_distribution="Gamma")
set.seed(100)
infection_times <- infection.times.sample(current_time=0, infection_status, infection_rates, focus_bay=2, n_infectious_states=1, spread_to_focus_bay=TRUE)

set.seed(100)
new_patient_attributes <- arrival.patient.attributes(stopping_time=5, arrival_rate=1, transfer_rates=c(1/4, 1/4), discharge_rates=c(1/5, 1/5), recovery_rates=c(1/2, 1/3), 
                           discharge_SD=c(1, 1), transfer_SD=c(1, 1), latent_SD=1, LOS_distribution="Gamma", latent_distribution="Gamma")

set.seed(100)
ward.simulation.paired(focus_bay=2, n_infectious_states=1,
  infection_rates, time_seq=5, 
  infection_status, init_patient_attributes, infection_times,
  restrictions=FALSE, quarantine_time=0, close_bay_coef=0, spread_to_focus_bay=FALSE, queue_departures=TRUE, 
  new_patient_attributes)

# returns
#    Focus Bay infections Other bay infections New arrivals Blocked arrivals Unused Bed Days in the focus bay Unused bed days elsewhere in the ward Outbreak occurred
#[1,]                    0                    2            1                3                         1.589952                              3.413776                1
#I      Infectious discharges Infectious transfers Infected discharges Infected transfers Days of delayed discharges Discharges Transfers
#[1,]                     0                    1                   0                  1                          0          0         5


## Simulation Ward Metrics
source("Arrival Patient Attributes.R")
source("Infection Times.R")
source("Simulation Metrics.R")
source("Ward Simulation.R")
source("Initial Patient Attributes.R")

ward_configuration <- c(4, 6, 6, 6, 6)
bay_occupants <- c(2, 4, 3, 4, 5)
infection_rates <- rbind(0.05, 0.1, 0.5, 0.2)
infection_distribution <- c(1/4, 1/4, 1/4, 1/4)

set.seed(100)
simulation.ward.metrics(ward_configuration, bay_occupants,  n_infectious_states=1,
                        infection_distribution, arrival_rate=1, transfer_rates=c(1/4, 1/4), discharge_rates=c(1/5, 1/5),
                        infection_rates, recovery_rates=c(1/2, 1/3), quarantine_time=3, spread_coef=infection_rates[4,]/infection_rates[3,], close_bay_coef=0.5, 
                        time_seq=c(7, 14), LOS_distribution="Gamma",
                        discharge_SD=c(1, 1), transfer_SD=c(1, 1), restrictions=TRUE, queue_departures=TRUE, spread_to_focus_bay=FALSE, 
                        latent_distribution="Gamma", latent_SD=1)

## returns
#      Focus Bay infections Other bay infections New arrivals Blocked arrivals Unused Bed Days in the focus bay Unused bed days elsewhere in the ward Outbreak occurred
#[1,]                    0                    8            7                0                         23.42484                              92.31605                1
#[2,]                    0                    8           13                0                         57.18392                             231.21831                1
#     Infectious discharges Infectious transfers Infected discharges Infected transfers Days of delayed discharges Discharges Transfers
#[1,]                     0                    1                   0                  1                          0          6        14
#[2,]                     0                    1                   0                  1                          0          6        20
