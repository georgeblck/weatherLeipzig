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
weatherData <- read.table("weatherLeipzig.csv", dec = ".", sep = ";", header = TRUE, stringsAsFactors = FALSE)
# Format the Date
weatherData$Date <- ymd(weatherData$Date)

######################################### Get new data if the local one is outdated How outdated is the data
days.outdated <- as.numeric((date(now()) - 1) - date(weatherData[nrow(weatherData), "Date"]))
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
    startRecent <- which(recentWeather$MESS_DATUM == oldWeather$MESS_DATUM[nrow(oldWeather)]) + 1
    weatherData <- rbind(oldWeather, recentWeather[startRecent:nrow(recentWeather), ])
    # Delete empty columns
    weatherData$STATIONS_ID <- NULL
    weatherData$eor <- NULL
    # Rename the variables
    colnames(weatherData) <- c("Date", "QN3", "AVGWindtempo", "MaxWindtempo", "QN4", "NiederschlagMM", 
        "NiederschlagsForm", "SonnenscheinStunden", "Schneehoehe", "NM", "AVGDampfDruck", "AVGLuftdruck", 
        "AVGTemp", "AVGFeuchte", "MaxTemp2m", "MinTemp2m", "MinTemp5cm")
    # Create the date variables
    weatherData$Month <- month(weatherData$Date)
    weatherData$Day <- mday(weatherData$Date)
    weatherData$Year <- year(weatherData$Date)
    weatherData$YDay <- yday(weatherData$Date)
    # Write the updated data set
    write.table(weatherData, file = "weatherLeipzig.csv", dec = ".", sep = ";", row.names = FALSE)
}

regioAll <- read.table("https://raw.githubusercontent.com/georgeblck/weatherLeipzig/master/saxonyClimate.csv", 
    header = TRUE, dec = ".", sep = ";")

# Make Plots
source("makeDailyPlot.R")
source("checkLeipzig.R")
source("makeRegioPlot.R")

# Combine plots - patchwork style
bigPlot <- (tempYear | tempPrecip)/dailyPlot
bigPlot <- tempYear | tempPrecip
bigPlot <- tempYear/tempPrecip
bigPlot <- (tempYear/dailyPlot) | tempPrecip
bigPlot <- tempYear + tempPrecip + plot_layout(ncol = 2, widths = c(1.6, 1))
print(bigPlot)

# Save Plot
if (save.plot) {
    # Save it cairo pdf
    ggsave(bigPlot, filename = paste0("plots/538_", gsub("[^[:alnum:]=\\.]", "", lubridate::now()), ".pdf"), 
        device = cairo_pdf, width = 270, height = 125, units = "mm", scale = 1.7, limitsize = FALSE)
    ggsave(dailyPlot, filename = paste0("plots/daily_", gsub("[^[:alnum:]=\\.]", "", lubridate::now()), 
        ".png"), width = 270, height = 135, units = "mm", scale = 1.7, limitsize = FALSE)
}
