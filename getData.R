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


source("makeDailyPlot.R")

bigPlot <- dailyPlot

if (save.plot) {
    # Save it cairo pdf
    ggsave(bigPlot, filename = paste0("plots/", gsub("[^[:alnum:]=\\.]", "", lubridate::now()), 
        ".pdf"), device = cairo_pdf, width = 270, height = 135, units = "mm", scale = 1.5)
    
}
