if (abs(days.outdated) > 1) {
  
  # Download weather data & remove old data
  unlink("DWDdata10", recursive = TRUE)
  # Get the recent data (last 1.5 years or so)
  recentLink <- selectDWD(what.station, res = "10_minutes", var = "air_temperature", per = "recent")
  recentWeather <- dataDWD(recentLink, dir = "DWDdata10",format = "%Y%m%d%H%M")
  # Combine historic and recent data, but remove the overlap
  startRecent <- which(recentWeather$MESS_DATUM == oldWeather$MESS_DATUM[nrow(oldWeather)]) + 
    1
  weatherData <- rbind(oldWeather, recentWeather[startRecent:nrow(recentWeather), 
                                                 ])
  # Delete empty columns
  weatherData$STATIONS_ID <- NULL
  weatherData$eor <- NULL
  # Rename the variables
  colnames(weatherData) <- c("Date", "QN3", "AVGWindtempo", "MaxWindtempo", "QN4", 
                             "NiederschlagMM", "NiederschlagsForm", "SonnenscheinStunden", "Schneehoehe", 
                             "NM", "AVGDampfDruck", "AVGLuftdruck", "AVGTemp", "AVGFeuchte", "MaxTemp2m", 
                             "MinTemp2m", "MinTemp5cm")
  # Create the date variables
  weatherData$Month <- month(weatherData$Date)
  weatherData$Day <- mday(weatherData$Date)
  weatherData$Year <- year(weatherData$Date)
  weatherData$YDay <- yday(weatherData$Date)
  # Write the updated data set
  write.table(weatherData, file = "weatherLeipzig.csv", dec = ".", sep = ";", row.names = FALSE)
}