# Clear workspace
rm(list = ls())

# load Packages
library(rdwd)
library(ggplot2)
library(formatR)
library(lubridate)
library(dplyr)
library(tidyr)
library(Cairo)
library(reshape2)
library(ggthemes)
library(patchwork)

# Pre-Init values
what.year <- 2018
since.year <- 1973
what.station <- "Leipzig/Halle"
save.plot <- TRUE

###################################################### Read the data Read the last (local) data
weatherData <- read.table("weatherLeipzig.csv", dec = ".", sep = ";", header = TRUE, 
    stringsAsFactors = FALSE)
# Format the Date
weatherData$Date <- ymd(weatherData$Date)

######################################### Get new data if the local one is outdated How outdated is the data
days.outdated <- as.numeric((date(now()) - 1) - date(weatherData[nrow(weatherData), 
    "Date"]))
print(days.outdated)
# If it is outdated --> get the recent data
if (abs(days.outdated) > 1) {
    
    # Download weather data & remove old data
    unlink("DWDdata", recursive = TRUE)
    # Get the recent data (last 1.5 years or so)
    recentLink <- selectDWD(what.station, res = "daily", var = "kl", per = "recent")
    # Get the historic data (back to 1934)
    oldLink <- selectDWD(what.station, res = "daily", var = "kl", per = "historical")
    recentWeather <- dataDWD(recentLink, dir = "DWDdata")
    oldWeather <- dataDWD(oldLink, dir = "DWDdata")
    
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

# Get regional data
regionalPaths <- list.files("regionalDat", full.names = TRUE)
regionalList <- lapply(regionalPaths, function(x){
  temp <- read.table(x, header = TRUE, skip = 1, sep = ";", dec = ".")
  return(temp[,-20])
})
names(regionalList) <- list.files("regionalDat", full.names = FALSE)
# Make Temp Data
regioTemp <- do.call("rbind", regionalList[grep("tm", names(regionalList))])
regioTemp <- regioTemp[order(regioTemp$Jahr, regioTemp$Monat),]
# Make Precip Data
regioPrecip <- do.call("rbind", regionalList[grep("rr", names(regionalList))])
regioPrecip <- regioPrecip[order(regioPrecip$Jahr, regioPrecip$Monat),]
# Make Sun Data
regioSun <- do.call("rbind", regionalList[grep("sd", names(regionalList))])
regioSun <- regioSun[order(regioSun$Jahr, regioSun$Monat),]
regio1 <- melt(regioTemp, id=c("Jahr","Monat"),factorsAsStrings = FALSE,
               value.name = "AvgTemp", variable.name = "Bundesland") 
regio2 <- melt(regioSun, id=c("Jahr","Monat"),factorsAsStrings = FALSE,
               value.name = "SunDuration", variable.name = "Bundesland") 
regio3 <- melt(regioPrecip, id=c("Jahr","Monat"),factorsAsStrings = FALSE,
               value.name = "PrecipMM", variable.name = "Bundesland") 
regioAll1 <- merge(regio1, regio3, by = c("Jahr", "Monat", "Bundesland"), sort = FALSE, all = TRUE)
regioAll <- merge(regioAll1, regio2, by = c("Jahr", "Monat", "Bundesland"), sort = FALSE, all = TRUE)
regioAll$Bundesland <- as.character(regioAll$Bundesland)
regioAll <- regioAll[order(regioAll$Jahr, regioAll$Monat),]
rm(regio1, regio2, regio3, regioAll1, regioTemp, regioSun, regioPrecip,regionalList, regionalPaths)





source("makeDailyPlot.R")
source("makeRegioPlot.R")

bigPlot <- (tempYear | tempPrecip) / dailyPlot
bigPlot <- tempYear | tempPrecip
if (save.plot) {
    # Save it cairo pdf
    ggsave(bigPlot, filename = paste0("plots/", gsub("[^[:alnum:]=\\.]", "", lubridate::now()), 
        ".pdf"), device = cairo_pdf, width = 270, height = 135, units = "mm", scale = 2)
    
}
