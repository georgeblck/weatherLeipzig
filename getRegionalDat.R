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
library(stringr)

tmString <- "ftp://ftp-cdc.dwd.de/pub/CDC/regional_averages_DE/monthly/air_temperature_mean/regional_averages_tm_"
rrString <- "ftp://ftp-cdc.dwd.de/pub/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_"
sunString <- "ftp://ftp-cdc.dwd.de/pub/CDC/regional_averages_DE/monthly/sunshine_duration/regional_averages_sd_"
allStrings <- c(tmString, rrString, sunString)
monthStrings <- str_pad(1:12, 2, pad = "0")
getStrings <- paste0(as.vector(outer(allStrings, monthStrings, paste0)), ".txt")


regionalList <- lapply(getStrings, function(x) {
  temp <- read.table(x, header = TRUE, skip = 1, sep = ";", dec = ".")
  return(temp[, -20])
})

str(regionalList)

names(regionalList) <- str_extract(getStrings, "[^/]+$")
# Make Temp Data
regioTemp <- do.call("rbind", regionalList[grep("tm", names(regionalList))])
regioTemp <- regioTemp[order(regioTemp$Jahr, regioTemp$Monat), ]
# Make Precip Data
regioPrecip <- do.call("rbind", regionalList[grep("rr", names(regionalList))])
regioPrecip <- regioPrecip[order(regioPrecip$Jahr, regioPrecip$Monat), ]
# Make Sun Data
regioSun <- do.call("rbind", regionalList[grep("sd", names(regionalList))])
regioSun <- regioSun[order(regioSun$Jahr, regioSun$Monat), ]
regio1 <- melt(regioTemp, id = c("Jahr", "Monat"), factorsAsStrings = FALSE, value.name = "AvgTemp", 
               variable.name = "Bundesland")
regio2 <- melt(regioSun, id = c("Jahr", "Monat"), factorsAsStrings = FALSE, value.name = "SunDuration", 
               variable.name = "Bundesland")
regio3 <- melt(regioPrecip, id = c("Jahr", "Monat"), factorsAsStrings = FALSE, value.name = "PrecipMM", 
               variable.name = "Bundesland")
regioAll1 <- merge(regio1, regio3, by = c("Jahr", "Monat", "Bundesland"), sort = FALSE, all = TRUE)
regioAll <- merge(regioAll1, regio2, by = c("Jahr", "Monat", "Bundesland"), sort = FALSE, all = TRUE)
regioAll$Bundesland <- as.character(regioAll$Bundesland)
regioAll <- regioAll[order(regioAll$Jahr, regioAll$Monat), ]
rm(regio1, regio2, regio3, regioAll1, regioTemp, regioSun, regioPrecip, regionalList)

write.table(regioAll, file = "saxonyClimate.csv", dec = ".", sep = ";", row.names = FALSE)