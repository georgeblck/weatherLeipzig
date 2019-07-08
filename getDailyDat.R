# Clear Workspace
rm(list = ls())

# load packages
library(lubridate)

# Get Old Weather
temp <- tempfile()
download.file("https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/tageswerte_KL_02932_19340101_20181231_hist.zip", 
    temp)
oldWeather <- read.csv(unz(temp, "produkt_klima_tag_19340101_20181231_02932.txt"), sep = ";", dec = ".", 
    na.strings = c("NA", -999), header = TRUE)
unlink(temp)

# Get new weather
temp <- tempfile()
download.file("https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/recent/tageswerte_KL_02932_akt.zip", 
    temp)
fileNames <- unzip(temp, list = TRUE)$Name
newWeather <- read.csv(unz(temp, grep("^produkt", fileNames, value = TRUE)), sep = ";", dec = ".", na.strings = c("NA", 
    -999), header = TRUE)
unlink(temp)

# Combine historic and recent data, but remove the overlap
startRecent <- which(newWeather$MESS_DATUM == oldWeather$MESS_DATUM[nrow(oldWeather)]) + 1
weatherData <- rbind(oldWeather, newWeather[startRecent:nrow(newWeather), ])

# Delete empty columns
weatherData$STATIONS_ID <- NULL
weatherData$eor <- NULL

# Rename the variables
colnames(weatherData) <- c("Date", "QN3", "AVGWindtempo", "MaxWindtempo", "QN4", "NiederschlagMM", "NiederschlagsForm", 
    "SonnenscheinStunden", "Schneehoehe", "NM", "AVGDampfDruck", "AVGLuftdruck", "AVGTemp", "AVGFeuchte", 
    "MaxTemp2m", "MinTemp2m", "MinTemp5cm")

# Create the date variables
weatherData$Date <- ymd(weatherData$Date)
weatherData$Month <- month(weatherData$Date)
weatherData$Day <- mday(weatherData$Date)
weatherData$Year <- year(weatherData$Date)
weatherData$YDay <- yday(weatherData$Date)

write.table(weatherData, file = "data/dailyData.csv", dec = ".", sep = ";", row.names = FALSE)
