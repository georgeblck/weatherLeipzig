library(lubridate)
library(tidyverse)
library(ggthemes)


# Filter festlegen
jahr <- 2018
bundesland <- "Sachsen"
lastmonat <- ifelse((year(today()) - jahr) > 0, 12, month(today()))

# Daten einlesen
regionalAverages <- read.table("regionalAverages.csv", header = TRUE, dec = ".", sep = ";")
# Daten filtern und aggregieren
bundeslandDat <- regionalAverages %>% filter(Bundesland == bundesland, Jahr <= jahr) %>% filter(Monat <= 
    lastmonat) %>% group_by(Jahr) %>% summarise_at(c("Temperatur", "Niederschlag"), list(mw = mean, summe = sum), 
    na.rm = TRUE) %>% ungroup() %>% mutate(typeYear = (Jahr >= 2000) + (Jahr == jahr))


# Make the empty plot with the geom_segments Get the borders and round down or up
tempRange <- bundeslandDat %>% summarise_at(c("Temperatur_mw"), list(min = min, max = max)) %>% mutate(min = floor(min), 
    max = ceiling(max)) %>% unlist()
tempTicks <- (tempRange[1] - 1):(tempRange[2] + 1)
tempsegDat <- data.frame(x = rep(1881, length(tempTicks)), xend = rep(jahr + 0.5, length(tempTicks)), 
    y = tempTicks, yend = tempTicks)
ggplot(data = bundeslandDat, aes(x = Jahr, y = Temperatur_mw)) + geom_segment(data = tempsegDat, aes(x = x, 
    y = y, xend = xend, yend = yend), inherit.aes = FALSE, linetype = "dashed", alpha = 0.3, col = "black") + 
    geom_rangeframe(col = "black") + geom_line(alpha = 0.7, col = "red", size = 0.7) + geom_point(size = 1.5, 
    alpha = 0.7, col = "red") + xlab("Jahr") + ylab("Durchschnittstemperatur (°C)") + theme_tufte(base_size = 11) + 
    theme(legend.position = "none") + scale_x_continuous(breaks = c(1881, seq(1900, jahr, by = 20), jahr)) + 
    scale_y_continuous(limits = c(tempRange[1], tempRange[2])) + theme(text = element_text(size = 11, 
    family = "sans-serif"))

# Plot precip against Temp
precipRange <- bundeslandDat %>% summarise_at(c("Niederschlag_summe"), list(min = min, max = max)) %>% 
    mutate(min = floor(min/50) * 50, max = ceiling(max/50) * 50) %>% unlist()
avgValues <- bundeslandDat %>% summarise_at(c("Temperatur_mw", "Niederschlag_summe"), mean) %>% unlist()
ggplot(data = bundeslandDat, aes(y = Temperatur_mw, x = Niederschlag_summe, color = factor(typeYear), 
    alpha = factor(typeYear))) + geom_segment(aes(y = avgValues[1], yend = avgValues[1], x = avgValues[2] * 
    (2/3), xend = avgValues * (4/3)), inherit.aes = FALSE, linetype = "dashed", alpha = 0.5, col = "black", 
    data = data.frame()) + geom_segment(aes(y = avgValues[1] * (3/4), yend = avgValues[1] * (5/4), x = avgValues[2], 
    xend = avgValues[2]), inherit.aes = FALSE, linetype = "dashed", alpha = 0.5, col = "black", data = data.frame()) + 
    geom_point(size = 1.5) + geom_rangeframe(col = "black", sides = "br") + theme_tufte(base_size = 15) + 
    ylab("Durchschnittstemperatur (°C)") + xlab("Niederschlag (mm)") + scale_color_manual(values = c("blue", 
    "black", "red"), breaks = c(0, 1), name = "Jahr", labels = c("1881-1999", paste0("2000-", jahr - 
    1))) + scale_y_continuous(limits = c(tempRange[1], tempRange[2]), position = "right") + scale_x_continuous(limits = precipRange, 
    breaks = seq(500, 1000, by = 100)) + theme(legend.position = c(0, 0), legend.justification = c(0, 
    0), legend.title = element_text(size = 5, face = "bold", hjust = 0.5)) + annotate("text", y = 6.2, 
    x = avgValues[2] + 0.01 * avgValues[2], label = "Durchschnittswerte", size = 2, angle = 90) + scale_alpha_manual(values = c(0.4, 
    1, 1), guide = FALSE) + guides(colour = guide_legend(override.aes = list(alpha = c(0.4, 0.9)))) + 
    theme(text = element_text(size = 11, family = "sans-serif")) + theme(legend.text = element_text(size = 4))



