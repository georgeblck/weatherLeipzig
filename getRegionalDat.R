# Clear workspace
rm(list = ls())

# load Packages
library(reshape2)
library(stringr)

#### Load data ####

# Get the URLs of the Regional Monthly Averages
tmURL <- "ftp://ftp-cdc.dwd.de/pub/CDC/regional_averages_DE/monthly/air_temperature_mean/regional_averages_tm_"
rrURL <- "ftp://ftp-cdc.dwd.de/pub/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_"
sunURL <- "ftp://ftp-cdc.dwd.de/pub/CDC/regional_averages_DE/monthly/sunshine_duration/regional_averages_sd_"
allURL <- c(tmURL, rrURL, sunURL)
# Append the month numbers to ftp-URLs
monthStrings <- str_pad(1:12, 2, pad = "0")
getURLs <- paste0(as.vector(outer(allURL, monthStrings, paste0)), ".txt")

# Read tables from urls
listData <- lapply(getURLs, function(x) {
    temp <- read.table(x, header = TRUE, skip = 1, sep = ";", dec = ".")
    return(temp[, -20])
})

# Rename the list (without the URL-part)
names(listData) <- str_extract(getURLs, "[^/]+$")

#### Collapse the list into respective Variable-DF ####
varTypes <- c("tm", "rr", "sd")
# Make a Data-Frame for each Variable Type
dfData <- lapply(varTypes, function(x) {
    temp <- do.call("rbind", listData[grep(x, names(listData))])
    temp <- melt(temp, id = c("Jahr", "Monat"), factorsAsStrings = FALSE, value.name = x, variable.name = "Bundesland")
    return(temp)
})
# Merge the list of data frames
dfData <- Reduce(function(...) merge(..., all = TRUE, by = c("Jahr", "Monat", "Bundesland")), dfData)
dfData$Bundesland <- as.character(dfData$Bundesland)
# Sort Data
dfData <- dfData[order(dfData$Jahr, dfData$Monat), ]
colnames(dfData) <- c("Jahr", "Monat", "Bundesland", "Temperatur", "Niederschlag", "Sonnendauer")

# Daten abspeichern
write.table(dfData, file = "regionalAverages.csv", dec = ".", sep = ";", row.names = FALSE)

# Clean-Up
rm(listData, allURL, getURLs, monthStrings, rrURL, sunURL, tmURL, varTypes)
