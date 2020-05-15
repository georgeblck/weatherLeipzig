# Clear Workspace
rm(list = ls())
options(scipen = 999)

# load packages
library(rdwd)
library(tidyverse)
library(lubridate)
library(gmodels)
library(ggpomological)
library(ggthemes)


lubStations <- nearbyStations(53.9163, 10.6854, radius = 40, var = "kl", res = "daily")

# Get data from ID
link <- selectDWD("Luebeck-Werft", res = "daily", var = "kl", per = "historical", 
  current = TRUE)
histDat <- dataDWD(link, read = TRUE)

link <- selectDWD("Luebeck", res = "daily", var = "kl", per = "historical", current = TRUE)
histDat2 <- dataDWD(link, read = TRUE)

link <- selectDWD("Luebeck-Blankensee", res = "daily", var = "kl", per = "historical", 
  current = TRUE)
histDat3 <- dataDWD(link, read = TRUE)

link <- selectDWD("Luebeck-Blankensee", res = "daily", var = "kl", per = "recent")
recent <- dataDWD(link, overwrite = TRUE, force = TRUE, read = TRUE)

# Zusammensetzen
startHist2 <- which(histDat2$MESS_DATUM == histDat$MESS_DATUM[nrow(histDat)]) + 1
startRec <- which(recent$MESS_DATUM == histDat3$MESS_DATUM[nrow(histDat3)]) + 1
weatherData <- rbind(histDat, histDat2[startHist2:nrow(histDat2), ], histDat3, recent[startRec:nrow(recent), 
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
weatherData <- weatherData %>% mutate(Date = ymd(Date), Month = month(Date), Day = mday(Date), 
  Year = year(Date), YDay = yday(Date))

# FArbe dreistufig und x-achse in monaten
plotData <- weatherData %>% group_by(Year) %>% mutate(cumPrecip = cumsum(replace_na(NiederschlagMM, 
  0))) %>% mutate(maxDay = factor(YDay == max(YDay))) %>% mutate(non_na_count = sum(!is.na(NiederschlagMM))) %>% 
  ungroup() %>% filter((non_na_count > (365 * 0.5)) | (Year == 2020)) %>% mutate(nowYear = year(now())) %>% 
  mutate(decade = floor(Year/10) * 10, lastDec = decade == 2010) %>% mutate(decFactor = cut(Year, 
  c(-Inf, 2010, 2020, Inf), dig.lab = 999, right = FALSE, labels = c("1890-2009", 
    "2010-2019", "2020")))

# Get Data for hacked text annotation
textDat <- plotData %>% group_by(Year) %>% slice(which.max(cumPrecip)) %>% select(Year, 
  cumPrecip, non_na_count) %>% mutate(xpos = 370) %>% filter(Year != year(now()))

# Get Confidence intervalls (doesnt look good, too many years)
sdDat <- plotData %>% group_by(YDay) %>% summarise(mean = ci(cumPrecip)[1], lowCI = ci(cumPrecip)[2], 
  hiCI = ci(cumPrecip)[3], sd = ci(cumPrecip)[4])
# Plot everything
beautyPlot <- plotData %>% ggplot(aes(x = YDay, y = cumPrecip, group = Year, colour = decFactor)) + 
  geom_text(data = textDat, inherit.aes = FALSE, aes(x = xpos, y = cumPrecip, label = Year)) + 
  geom_line() + xlab("Tag im Jahr") + ylab("Kummulativer Niederschlag in MM") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 1000)) + 
  scale_colour_manual("Jahr", values = c("grey", "black", "firebrick1")) + scale_x_continuous(expand = expansion(mult = c(0, 
  0.02)), breaks = c(15, 45, 75, 105, 135, 165, 195, 228, 258, 288, 320, 350), 
  labels = c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", 
    "September", "Oktober", "November", "Dezember")) + # geom_linerange(sdDat, mapping = aes(x = YDay, ymin = lowCI, ymax = hiCI),
# inherit.aes = FALSE, colour = 'wheat4', alpha = 0.7)+ geom_line(sdDat, mapping
# = aes(x=YDay, y = mean), inherit.aes = FALSE, colour = 'gold', linetype =
# 'dashed')+
ggtitle("Niederschlag in Lübeck seit 1890") + theme_pomological_fancy(plot.background.color = "white") + 
  theme(legend.position = c(0, 1), legend.justification = c(0, 1), legend.title = element_text(size = 12, 
    hjust = 0.5)) + theme(text = element_text(size = 11)) + theme(legend.text = element_text(size = 11), 
  axis.title.x = element_blank(), axis.text.x = element_text(size = 10))
beautyPlot
ggsave("out.pdf", device = cairo_pdf, width = 29.7, height = 21, dpi = 320, units = "cm")
paint_pomological(beautyPlot, res = 100, width = 1500, height = 1500/1.414, outfile = "out.png", 
  pointsize = 1, antialias = TRUE, clip = TRUE)

# Filter festlegen
jahr <- 2020
lastday <- ifelse((year(today()) - jahr) != 0, yday(ymd(paste0(jahr, "1231"))), yday(today()) - 
  5)
beginjahr <- 1890

# Daten einlesen
dailyData <- weatherData %>% 
  select(c("Date", "YDay", "Year", "AVGTemp", "NiederschlagMM"))
dailyData <- weatherData %>% select(c("Date", "YDay", "Year", "AVGTemp", "NiederschlagMM"))
colnames(dailyData)[3:5] <- c("Jahr", "Temperatur", "Niederschlag")

# Daten filtern und aggregieren
weatherDat <- dailyData %>% filter(Jahr <= jahr) %>% filter(YDay <= lastday) %>% 
  group_by(Jahr) %>% summarise_at(c("Temperatur", "Niederschlag"), list(mw = mean, 
  summe = sum), na.rm = TRUE) %>% ungroup() %>% mutate(typeYear = (Jahr >= 2000) + 
  (Jahr == jahr)) %>% filter(Jahr >= beginjahr)

# Make the empty plot with the geom_segments Get the borders and round down or up
tempRange <- weatherDat %>% summarise_at(c("Temperatur_mw"), list(min = min, max = max), 
  na.rm = TRUE) %>% mutate(min = floor(min), max = ceiling(max)) %>% unlist()
tempTicks <- tempRange[1]:tempRange[2]
tempsegDat <- data.frame(x = rep(beginjahr, length(tempTicks)), xend = rep(jahr + 
  0.5, length(tempTicks)), y = tempTicks, yend = tempTicks)
ggplot(data = weatherDat, aes(x = Jahr, y = Temperatur_mw)) + geom_segment(data = tempsegDat, 
  aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE, linetype = "dashed", 
  alpha = 0.3, col = "black") + geom_rangeframe(col = "black") + geom_line(alpha = 0.7, 
  col = "red", size = 0.7) + geom_point(size = 1.5, alpha = 0.7, col = "red") + 
  xlab("Jahr") + ylab("Durchschnittstemperatur (°C)") + theme_tufte(base_size = 11) + 
  theme(legend.position = "none") + scale_x_continuous(breaks = c(beginjahr, seq(1890, 
  jahr, by = 10), jahr)) + scale_y_continuous(limits = c(tempRange[1], tempRange[2])) + 
  theme(text = element_text(size = 11, family = "sans-serif"))

#### Plot Precip vs Temp ####

# Round Precip to nearest 50
precipRange <- weatherDat %>% summarise_at(c("Niederschlag_summe"), list(min = min, 
  max = max)) %>% mutate(min = floor(min/50) * 50, max = ceiling(max/50) * 50) %>% 
  unlist()
# Get Average Values
avgValues <- weatherDat %>% summarise_at(c("Temperatur_mw", "Niederschlag_summe"), 
  mean) %>% unlist()
ggplot(data = weatherDat, aes(y = Temperatur_mw, x = Niederschlag_summe, 
  color = factor(typeYear), alpha = factor(typeYear))) + geom_segment(aes(y = avgValues[1], 
  yend = avgValues[1], x = precipRange[1] * 1.1, xend = precipRange[2] * 0.9), 
  inherit.aes = FALSE, linetype = "dashed", alpha = 0.5, col = "black", data = data.frame()) + 
  geom_segment(aes(y = tempRange[1] * ifelse(tempRange[1] > 0, 1.1, 0.9), yend = tempRange[2] * 
    0.9, x = avgValues[2], xend = avgValues[2]), inherit.aes = FALSE, linetype = "dashed", 
    alpha = 0.5, col = "black", data = data.frame()) + geom_point(size = 2) + 
  geom_rangeframe(col = "black", sides = "br") + theme_tufte(base_size = 15) + 
  ylab("Durchschnittstemperatur (°C)") + xlab("Niederschlag (mm)") + scale_color_manual(values = c("blue", 
  "black", "red"), breaks = c(0, 1), name = "Jahr", labels = c("1890-1999", paste0("2000-", 
  jahr - 1))) + scale_y_continuous(limits = c(tempRange[1], tempRange[2]), position = "right") + 
  scale_x_continuous(limits = precipRange) + theme(legend.position = c(0, 0), legend.justification = c(0, 
  0), legend.title = element_text(size = 7, face = "bold", hjust = 0.5)) + annotate("text", 
  y = tempRange[1] * ifelse(tempRange[1] > 0, 1.1, 0.9), x = avgValues[2] + 0.01 * 
    avgValues[2], label = "Durchschnittswerte", size = 2, angle = 90) + scale_alpha_manual(values = c(0.4, 
  1, 1), guide = FALSE) + guides(colour = guide_legend(override.aes = list(alpha = c(0.4, 
  0.9)))) + theme(text = element_text(size = 11, family = "sans-serif")) + theme(legend.text = element_text(size = 6))

