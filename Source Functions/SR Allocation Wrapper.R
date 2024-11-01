SR.allocation.wrapper <- function(
    initial.ward="T03",
    destination.ward="T10N",
    disease="Influenza A",
    
    initial.bay.size=5,
    destination.bay.size=4,
    
    initial.side.room.occupancy=8,
    initial.ward.focus.bay.occupants=5,
    initial.ward.other.bay.occupants=15,
    
    alternative.ward.side.room.occupancy=3,
    
    destination.ward.focus.bay.occupants=3,
    destination.ward.other.bay.occupants=10,
    
    side.room.infections=0,
    
    time.since.positive.test=1,
    
    iterations=100){

  
  stopping.time <- 14
  
  parallel <- NULL
  
  for(i in 1:iterations){
    metrics <- UCLH.SR.allocation.metrics(initial.ward, destination.ward, disease, initial.bay.size, destination.bay.size,
                                                           initial.ward.side.room.occupancy, initial.ward.focus.bay.occupants, 
                                                           initial.ward.other.bay.occupants,
                                                           alternative.ward.side.room.occupancy, destination.ward.focus.bay.occupants, 
                                                           destination.ward.other.bay.occupants,
                                                           side.room.infections, time.since.positive.test,
                                                           Ward.Data, Influenza.Data, COVID.Data, Norovirus.Data, RSV.Data, stopping.time,
                                                           "Exponential","Exponential", FALSE)
    
    parallel <- c(parallel, list(metrics))
  }
  
  
  Results <- NULL
  
  for(policy in 1:3){
    Results.policy <- NULL
    for(ward in 1:3){
      Results.policy.ward <- 0
      for(i in 1:iterations){
        Results.policy.ward <- ((i-1)*Results.policy.ward + parallel[[i]][[policy]][[ward]])/i
      }
      Results.policy <- c(Results.policy, list(Results.policy.ward))
    }
    Results <- c(Results, list(Results.policy))
  }
  
  Results.initial.ward <- rbind(Results[[1]][[1]], Results[[2]][[1]], Results[[3]][[1]])
  Results.alternative.ward <- rbind(Results[[1]][[2]], Results[[2]][[2]], Results[[3]][[3]])
  Results.destination.ward <- rbind(Results[[1]][[3]], Results[[2]][[3]], Results[[3]][[3]])
  
  column.names <- c("Focus Bay Infections", "Infections elsehwere on the ward", "New Arrivals admitted", "Blocked Arrivals", "Bed Days Lost",
                    "Unused Bed Days in Focus Bay", "Unused Bed Days in other bays", "Outbreak Probability", "Patients Discharged while Infected",
                    "Patients Transferred while Infected", "Days of delayed transfersf", "Total Discharges", "Total Transfers")
  policy.names <- c("Keep Patient ICU", "Transfer Patient to Open Bay on Destination Ward", "Transfer Patient to Side Room on Alternative Ward")
  infection.metrics <- c(1, 2, 8, 9, 10)
  Infection.Tables <- NULL
  Flow.Tables <- NULL
  for(ward in 1:3){
    Results.ward <-  rbind(Results[[1]][[ward]], Results[[2]][[ward]], Results[[3]][[ward]])
    Table <- as.data.frame(Results.ward)
    colnames(Table) <- column.names
    rownames(Table) <- policy.names
    
    Infection.Table <- Table[, infection.metrics]
    Flow.Table <- Table[, -infection.metrics]
    
    Infection.Tables <- c(Infection.Tables, list(Infection.Table))
    Flow.Tables <- c(Flow.Tables, list(Flow.Table))
  }
  
  return(list(Infection.Tables, Flow.Tables))
}