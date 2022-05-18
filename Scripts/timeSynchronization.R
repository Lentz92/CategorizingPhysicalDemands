timeSynchronization <- function(IMUdataframe = NULL, GPSdataframe, FMPdata, startTime, endTime = NULL){
  #Function to filter the data based on the synchronization times.
  #startTime and endTime has to be of class datetime format = "%Y-%m-%d%H:%M:%S"
  
  #If we want everything from the starttime to the end of the dataframe Then we do not need an end time,
  #there fore endTime is set to "NULL" as standard. This if-statement checks this. 
  #However, if we are interested in setting the endTime, the if-statement will do nothing
  
  if (is.null(endTime)){
    #if endtime is NULL then endTime would be set to the end time of the dataframe
    endTime <-  GPSdataframe$timestamp[nrow(GPSdataframe)]
  } else {
    #Do nothing
  }
  
  syncGPSData <- GPSdataframe %>%
    janitor::clean_names() %>% 
    filter(timestamp >= startTime,
           timestamp <= endTime)
  
  syncFMPData <- FMPdata %>% 
    filter(date_time >= startTime,
           date_time <= endTime)
  
  if (!is.null(IMUdataframe)){
    syncIMUData <- IMUdataframe %>% 
      janitor::clean_names() %>% 
      filter(date_time >= startTime,
             date_time <= endTime)
    
    dataframes <- list(syncIMUData, syncGPSData, syncFMPData)
  } else {
    dataframes <- list(syncGPSData, syncFMPData)
  }
  
  return(dataframes)  
}