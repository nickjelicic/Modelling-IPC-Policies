infection.parameters <- function(Influenza.Data, COVID.Data, Norovirus.Data, RSV.Data, disease, ward){
  
  if(disease=="Influenza A"){
    mean.latent.period <- 1
    mean.infectious.period <- 9
    
    columns <- which(endsWith(colnames(Influenza.Data), ward))
    
    if(length(columns)==0){
      
      ward <- substr(ward, 1, nchar(ward)-1)
      columns <- which(endsWith(colnames(Influenza.Data), ward))
      parameters <- infection.parameters(Influenza.Data, COVID.Data, Norovirus.Data, RSV.Data, disease, ward)[1:3, ]
    }else if(length(columns)>6){
      parameters <- "Ward name too general"
    }else{
      Influenza.Ward.Data <- Influenza.Data[, c(1:2, columns)]
      parameters <- infection.estimates(Influenza.Ward.Data)
    }
  }
  
  if(disease=="COVID"){
    mean.latent.period <- 5
    mean.infectious.period <- 12
    
    columns <- which(endsWith(colnames(COVID.Data), ward))
    
    if(length(columns)==0){
      ward <- substr(ward, 1, nchar(ward)-1)
      columns <- which(endsWith(colnames(Influenza.Data), ward))
      parameters <- infection.parameters(Influenza.Data, COVID.Data, Norovirus.Data, RSV.Data, disease, ward)[1:3, ]
    }else if(length(columns)>6){
      parameters <- "Ward name too general"
    }else{
      COVID.Ward.Data <- COVID.Data[, c(1:2, columns)]
      parameters <- infection.estimates(COVID.Ward.Data)
    }
  }
  
  if(disease=="Norovirus"){
    mean.latent.period <- 2
    mean.infectious.period <- 3
    
    columns <- which(endsWith(colnames(Norovirus.Data), ward))
    
    if(length(columns)==0){
      ward <- substr(ward, 1, nchar(ward)-1)
      columns <- which(endsWith(colnames(Influenza.Data), ward))
      parameters <- infection.parameters(Influenza.Data, COVID.Data, Norovirus.Data, RSV.Data, disease, ward)[1:3, ]
    }else if(length(columns)>6){
      parameters <- "Ward name too general"
    }else{
      Norovirus.Ward.Data <- Norovirus.Data[, c(1:2, columns)]
      parameters <- infection.estimates(Norovirus.Ward.Data)
    }
  }
  
  if(disease=="RSV"){
    mean.latent.period <- 0
    mean.infectious.period <- 5
    
    columns <- which(endsWith(colnames(RSV.Data), ward))
    
    if(length(columns)==0){
      ward <- substr(ward, 1, nchar(ward)-1)
      columns <- which(endsWith(colnames(Influenza.Data), ward))
      parameters <- infection.parameters(Influenza.Data, COVID.Data, Norovirus.Data, RSV.Data, disease, ward)[1:3, ]
    }else if(length(columns)>6){
      parameters <- "Ward name too general"
    }else{
      RSV.Ward.Data <- RSV.Data[, c(1:2, columns)]
      parameters <- infection.estimates(RSV.Ward.Data)
    }
  }
  
  parameters <- c(parameters, mean.latent.period, mean.infectious.period)
  
  
  return(data.frame(parameters, row.names=c("Background Infection Rate", "Side Room Infection Rate", "Open Bay Infection Rate", "mean latent period", "mean infectious period")))
}