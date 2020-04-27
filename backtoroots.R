# Clear Workspace
rm(list = ls())

# load packages
library(rdwd)
library(tidyverse)
library(lubridate)

weatherId <- "Leipzig-Holzhausen"
#weatherId <- "Hohn"
weatherId <- "Travemuende"
weatherId <- "Luebeck-Blankensee"
weatherId <- "Luebeck-Werft"


# Get data from ID
link <- selectDWD(weatherId, res="daily", var="kl", per = "historical", current = TRUE)
histDat <- dataDWD(link, read=TRUE)

link <- selectDWD(weatherId, res="daily", var="kl", per = "recent", current = TRUE)
recDat <- dataDWD(link, read=TRUE)

startRecent <- which(recDat$MESS_DATUM == histDat$MESS_DATUM[nrow(histDat)]) + 1
weatherData <- rbind(histDat, recDat[startRecent:nrow(recDat), ])

# Delete empty columns
weatherData$STATIONS_ID <- NULL
weatherData$eor <- NULL

# Rename the variables
colnames(weatherData) <- c("Date", "QN3", "AVGWindtempo", "MaxWindtempo", "QN4", "NiederschlagMM", "NiederschlagsForm", 
                           "SonnenscheinStunden", "Schneehoehe", "NM", "AVGDampfDruck", "AVGLuftdruck", "AVGTemp", "AVGFeuchte", 
                           "MaxTemp2m", "MinTemp2m", "MinTemp5cm")

# Create the date variables
weatherData <- weatherData %>% mutate(Date = ymd(Date), Month = month(Date),
                                      Day = mday(Date), Year = year(Date),
                                      YDay = yday(Date))


weatherData %>% filter(Year >= 1900) %>% 
  group_by(Year) %>% mutate(cumPrecip = cumsum(NiederschlagMM)) %>% 
  mutate(maxDay = factor(YDay == max(YDay))) %>%
  ungroup() %>% mutate(nowYear = Year == 2020) %>%
  mutate(decade = floor(Year/10)*10) %>%
  ggplot(aes(x=YDay, y = cumPrecip, group = Year,
             colour = nowYear))+
  #geom_text(data = , inherit.aes = FALSE,aes(label=Year, alpha = maxDay))+
  #scLuebeck-Werft	ale_alpha_discrete(range=c(0,1))+
  geom_line()+theme_minimal()+
  theme(legend.position = "none")+
  xlab("Tag im Jahr")+ylab("Kummulativer Niederschlag in mm")+
  scale_y_continuous(expand = expand_scale(mult=c(0,0)),limits = c(0,1000))+
  scale_colour_manual(values = c("grey", "red"))




weatherData %>% count(Year)
weatherData %>% filter(is.na(NiederschlagMM)) %>% count(Year)

sapply(file, function(x) sum(is.na(x)))


nearbyStations(51.3397,12.3731,radius=20, var = "kl", res = "daily")
nearbyStations(54.1905,9.6187,radius=40, var = "kl", res = "daily")
