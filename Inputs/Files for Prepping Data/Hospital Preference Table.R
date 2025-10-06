#Load preference tables for SNF and community populations

setwd(paste("Inputs/Preference_Frames"))

if(file.exists(paste(paste("hospital_preference_frame", as.character(county), sep = "_"),".Rda", sep= "")) && file.exists(paste(paste("hospital_preference_frame_comm", as.character(county), sep = "_"),".Rda", sep= ""))){
  
  load(paste(paste("hospital_preference_frame", as.character(county), sep = "_"),".Rda", sep= ""))
  load(paste(paste("hospital_preference_frame_comm", as.character(county), sep = "_"),".Rda", sep= ""))
}else{
  #Otherwise run code
  
  #preference frame for snf:
  
  distanceTable <- as.matrix(raster::pointDistance( #The output table is the distance from each SNF (rows) to each hospital (columns)
    p1 = cbind(nursingfacility$longitude, nursingfacility$latitude), #SNF longitude and latitude
    p2 = cbind(hospitals$longitude, hospitals$latitude), #hospital longitude and latitude
    lonlat = TRUE,
    allpairs = TRUE
  ))
  
  distanceTable <- t(distanceTable) #transforms table so hospitals can be ranked by distance
  distanceTable <- apply(distanceTable,2,frank) #ranks by distance
  if(is.null(nrow(distanceTable)) == FALSE){ #checks if distanceTable is still a table or only a vector
    distanceTable <- as.data.table(t(distanceTable)) #if it is still a table it is transformed again so the hospitals are in separate columns and the each row is a household id
  }
  hospital_preference_frame <- data.table(cbind(nursingfacility$snf_id, distanceTable)) #binds ids of snfs and the distance ranks
  colnames(hospital_preference_frame) <- c("snf_id", paste("hosp_id_", hospitals$hosp_id, sep = "")) #gives column headings for the snf_id and the individual hospitals
  
  #preference frame for community:
  
  distanceTable <- as.matrix(raster::pointDistance( #The output table is the distance from each SNF (rows) to each hospital (columns)
    p1 = cbind(c(households$longitude,otherGroupQuarters$longitude), c(households$latitude, otherGroupQuarters$latitude)), #SNF longitude and latitude
    p2 = cbind(hospitals$longitude, hospitals$latitude), #hospital longitude and latitude
    lonlat = TRUE,
    allpairs = TRUE
  ))
  
  distanceTable <- t(distanceTable) #transforms table so hospitals can be ranked by distance
  distanceTable <- apply(distanceTable,2,frank) #ranks by distance
  if(is.null(nrow(distanceTable)) == FALSE){ #checks if distanceTable is still a table or only a vector
    distanceTable <- as.data.table(t(distanceTable)) #if it is still a table it is transformed again so the hospitals are in separate columns and the each row is a household id
  }
  hospital_preference_frame_comm <- data.table(cbind(c(households$sp_id, otherGroupQuarters$sp_id), distanceTable)) #binds ids of households and the distance ranks
  colnames(hospital_preference_frame_comm) <- c("sp_id", paste("hosp_id_", hospitals$hosp_id, sep = "")) #gives column headings for the sp_id and the individual hospitals
  
  save(hospital_preference_frame, file = paste(paste("hospital_preference_frame", as.character(county), sep = "_"),".Rda", sep= ""))
  save(hospital_preference_frame_comm, file = paste(paste("hospital_preference_frame_comm", as.character(county), sep = "_"),".Rda", sep= ""))
  
  
  rm(distanceTable, otherGroupQuarters) #removes unneeded variables
  
  gc() #clears memory
  
}