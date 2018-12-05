# Subset Saxony
saxonyDat <- regioAll[regioAll$Bundesland == "Sachsen", ]

# Make Averages of all years for Months 1-11
saxonyMeanTemp1_11 <- tapply(saxonyDat[saxonyDat$Monat %in% 1:11, "AvgTemp"], saxonyDat[saxonyDat$Monat %in% 
    1:11, "Jahr"], FUN = mean)
saxonyMeanPrecip1_11 <- tapply(saxonyDat[saxonyDat$Monat %in% 1:11, "PrecipMM"], 
    saxonyDat[saxonyDat$Monat %in% 1:11, "Jahr"], FUN = sum)
saxonyMeanSunDur1_11 <- tapply(saxonyDat[saxonyDat$Monat %in% 1:11, "SunDuration"], 
    saxonyDat[saxonyDat$Monat %in% 1:11, "Jahr"], FUN = sum)
saxonyMeans <- data.frame(Jahr = as.numeric(names(saxonyMeanPrecip1_11)), Temp = saxonyMeanTemp1_11, 
    Precip = saxonyMeanPrecip1_11, sunHours = saxonyMeanSunDur1_11)
saxonyMeans$yearRegul <- saxonyMeans$Jahr >= 2000
saxonyMeans$yearRegul[saxonyMeans$Jahr == 2018] <- 2

# Plot average Temp over the Years
tempYear <- ggplot(data = saxonyMeans, aes(x = Jahr, y = Temp)) + geom_segment(aes(x = 1881, 
    xend = 2018.5, y = 11, yend = 11), inherit.aes = FALSE, linetype = "dashed", 
    alpha = 0.3, col = "black", data = data.frame()) + geom_segment(aes(x = 1881, 
    xend = 2018.5, y = 10, yend = 10), inherit.aes = FALSE, linetype = "dashed", 
    alpha = 0.3, col = "black", data = data.frame()) + geom_segment(aes(x = 1881, 
    xend = 2018.5, y = 9, yend = 9), inherit.aes = FALSE, linetype = "dashed", alpha = 0.3, 
    col = "black", data = data.frame()) + geom_segment(aes(x = 1881, xend = 2018.5, 
    y = 8, yend = 8), inherit.aes = FALSE, linetype = "dashed", alpha = 0.3, col = "black", 
    data = data.frame()) + geom_segment(aes(x = 1881, xend = 2018.5, y = 7, yend = 7), 
    inherit.aes = FALSE, linetype = "dashed", alpha = 0.3, col = "black", data = data.frame()) + 
    geom_rangeframe(col = "black") + geom_line(alpha = 0.7, col = "red", size = 1) + 
    xlab("Jahr") + ylab("Durchschnittstemperatur (°C)") + geom_point(size = 3, alpha = 0.7, 
    col = "red") + theme_tufte(base_size = 15) + theme(legend.position = "none") + 
    scale_x_continuous(breaks = c(1881, seq(1900, 2018, by = 20), 2018)) + scale_y_continuous(limits = c(6.5, 
    11.5)) + theme(text = element_text(size = 16, family = "sans-serif"))

# Plot Sunhour data
sunYear <- ggplot(data = na.omit(saxonyMeans), aes(x = Jahr, y = sunHours/365)) + 
    geom_line(alpha = 0.7, col = "darkorange", size = 1) + xlab("Jahr") + ylab("Sonnenstunden") + 
    geom_point(size = 3, alpha = 0.7, col = "darkorange") + theme_tufte(base_size = 15) + 
    theme(legend.position = "none") + scale_x_continuous(limits = c(1951, 2018), 
    breaks = c(1951, seq(1970, 2018, by = 20), 2018)) + geom_rangeframe(col = "black") + 
    geom_smooth(se = FALSE)
print(sunYear)


# Plot precip against Temp
tempPrecip <- ggplot(data = saxonyMeans, aes(y = Temp, x = Precip, color = factor(yearRegul), 
    alpha = factor(yearRegul))) + geom_segment(aes(y = mean(saxonyMeans$Temp[saxonyMeans$yearRegul == 
    0]), yend = mean(saxonyMeans$Temp[saxonyMeans$yearRegul == 0]), x = 350, xend = 980), 
    inherit.aes = FALSE, linetype = "dashed", alpha = 0.5, col = "black", data = data.frame()) + 
    geom_segment(aes(y = 6.5, yend = 11.2, x = mean(saxonyMeans$Precip[saxonyMeans$yearRegul == 
        0]), xend = mean(saxonyMeans$Precip[saxonyMeans$yearRegul == 0])), inherit.aes = FALSE, 
        linetype = "dashed", alpha = 0.5, col = "black", data = data.frame()) + geom_point(size = 3) + 
    geom_rangeframe(col = "black", sides = "br") + theme_tufte() + ylab("Durchschnittstemperatur (°C)") + 
    xlab("Niederschlag (mm)") + scale_color_manual(values = c("blue", "black", "red"), 
    breaks = c(0, 1), name = "Jahr", labels = c("1881-1999", "2000-2017")) + theme_tufte(base_size = 15) + 
    scale_y_continuous(limits = c(6.5, 11.5), position = "right") + scale_x_continuous(limits = c(350, 
    1020), breaks = c(400, 600, 800, 1000)) + theme(legend.position = c(0, 0), legend.justification = c(0, 
    0), legend.background = element_rect(fill = "grey90", size = 0.5, linetype = "solid", 
    colour = "black"), legend.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
    annotate("text", y = 7, x = mean(saxonyMeans$Precip[saxonyMeans$yearRegul == 
        0]) + 7, label = "Durchschnittswerte", size = 3, angle = 90) + scale_alpha_manual(values = c(0.4, 
    0.9, 0.9), guide = FALSE) + guides(colour = guide_legend(override.aes = list(alpha = c(0.4, 
    0.9)))) + theme(text = element_text(size = 16, family = "sans-serif"))

