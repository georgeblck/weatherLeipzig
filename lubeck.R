# Clear Workspace
rm(list = ls())
options(scipen=999)

# load packages
library(rdwd)
library(tidyverse)
library(lubridate)
library(gmodels)
library(ggpomological)

lubStations <- nearbyStations(53.9163,10.6854,radius=40, var = "kl", res = "daily")


# Get data from ID
link <- selectDWD("Luebeck-Werft", res="daily", var="kl", per = "historical", current = TRUE)
histDat <- dataDWD(link, read=TRUE)

link <- selectDWD("Luebeck", res="daily", var="kl", per = "historical", current = TRUE)
histDat2 <- dataDWD(link, read=TRUE)

link <- selectDWD("Luebeck-Blankensee", res="daily", var="kl", per = "historical", current = TRUE)
histDat3 <- dataDWD(link, read=TRUE)

link <- selectDWD("Luebeck-Blankensee", res="daily", var="kl", per = "recent", current = TRUE)
recent <- dataDWD(link, read=TRUE)

# Zusammensetzen
startHist2 <- which(histDat2$MESS_DATUM == histDat$MESS_DATUM[nrow(histDat)]) + 1
startRec <- which(recent$MESS_DATUM == histDat3$MESS_DATUM[nrow(histDat3)]) + 1
weatherData <- rbind(histDat, histDat2[startHist2:nrow(histDat2), ], histDat3, recent[startRec:nrow(recent),])


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

# FArbe dreistufig und x-achse in monaten
plotData <- weatherData %>% 
  group_by(Year) %>% mutate(cumPrecip = cumsum(replace_na(NiederschlagMM,0))) %>% 
  mutate(maxDay = factor(YDay == max(YDay))) %>%
  mutate(non_na_count = sum(!is.na(NiederschlagMM))) %>%
  ungroup() %>% filter((non_na_count > (365*0.5)) | (Year == 2020)) %>%
  mutate(nowYear = year(now())) %>%
  mutate(decade = floor(Year/10)*10, lastDec = decade == 2010) %>%
  mutate(decFactor = cut(Year,c(-Inf,2010,2020,Inf), dig.lab = 999, right = FALSE, labels = c("1890-2009", "2010-2019", "2020")))

# Get Data for hacked text annotation
textDat <- plotData %>% group_by(Year) %>%
  slice(which.max(cumPrecip)) %>%
  select(Year, cumPrecip, non_na_count) %>% mutate(xpos = 370) %>%
  filter(Year != year(now()))

# Get Confidence intervalls (doesnt look good, too many years)
sdDat <- plotData %>% group_by(YDay) %>%
  summarise(mean = ci(cumPrecip)[1], 
                   lowCI = ci(cumPrecip)[2],
                   hiCI = ci(cumPrecip)[3], 
                   sd = ci (cumPrecip)[4])
# Plot everything
beautyPlot <- plotData %>%
  ggplot(aes(x=YDay, y = cumPrecip, group = Year,
             colour = decFactor))+
  geom_text(data = textDat, inherit.aes = FALSE,aes(x=xpos, y=cumPrecip, label=Year))+
  geom_line()+
  xlab("Tag im Jahr")+ylab("Kummulativer Niederschlag in MM")+
  scale_y_continuous(expand = expansion(mult=c(0,0)),limits = c(0,1000))+
  scale_colour_manual("Jahr",values = c("grey", "black", "firebrick1"))+
  scale_x_continuous(expand = expansion(mult=c(0,0.01)), breaks = c(15, 45, 75, 105, 135, 165, 195, 228, 258, 288, 320, 350), 
                     labels = c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"))+
  #geom_linerange(sdDat, mapping = aes(x = YDay, ymin = lowCI, ymax = hiCI), inherit.aes = FALSE, colour = "wheat4", alpha = 0.7)+
  #geom_line(sdDat, mapping = aes(x=YDay, y = mean), inherit.aes = FALSE, colour = "gold", linetype = "dashed")+
  ggtitle("Niederschlag in Lübeck seit 1890")+
  theme_pomological_fancy()+
  theme(legend.position = c(0, 1), legend.justification = c(0,1),  
        legend.title = element_text(size = 12,hjust = 0.5))+
  theme(text = element_text(size = 11)) + theme(legend.text = element_text(size = 11),axis.title.x=element_blank())
beautyPlot
paint_pomological(beautyPlot, res = 100, width = 1500, height = 1500/1.8, outfile = "out.png",
                  pointsize = 1, antialias=TRUE,clip = TRUE)
