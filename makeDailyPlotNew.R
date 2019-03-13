source("getDailyDat.R")

# Subset Saxony
finalYear <- 2018

subWeather <- weatherData[weatherData$Year >= 1973 & weatherData$Year <= 2018, ]
# Make Averages of all years for Months 1-11
meanTemp18 <- aggregate(AVGTemp ~ Year, data = subWeather, FUN = mean)
sumPrecip18 <- aggregate(NiederschlagMM ~ Year, data = subWeather, FUN = sum)
sumSundur18 <- aggregate(SonnenscheinStunden ~ Year, data = subWeather, FUN = sum)

leipzigDat <- meanTemp18 %>% left_join(sumPrecip18) %>% left_join(sumSundur18)
colnames(leipzigDat) <- c("Year", "Temp", "Precip", "Sun")
leipzigDat$yearRegul <- leipzigDat$Year >= 2000
leipzigDat$yearRegul[leipzigDat$Year == finalYear] <- 2

# Make the empty plot with the geom_segments Get the borders and round down or up
tempRange <- round(range(leipzigDat$Temp))
tempTicks <- (tempRange[1] - 2):(tempRange[2] + 2)
tempsegDat <- data.frame(x = rep(1973, length(tempTicks)), xend = rep(finalYear + 0.5, length(tempTicks)), 
    y = tempTicks, yend = tempTicks)

tempYear <- ggplot(data = leipzigDat, aes(x = Year, y = Temp)) + geom_segment(data = tempsegDat, aes(x = x, 
    y = y, xend = xend, yend = yend), inherit.aes = FALSE, linetype = "dashed", alpha = 0.3, col = "black") + 
    geom_rangeframe(col = "black") + geom_line(alpha = 0.7, col = "red", size = 0.7) + geom_point(size = 2, 
    alpha = 0.7, col = "red") + xlab("Jahr") + ylab("Durchschnittstemperatur (°C)") + theme_tufte(base_size = 11) + 
    theme(legend.position = "none") + scale_x_continuous(breaks = c(1973, seq(1980, 2018, by = 10), finalYear)) + 
    scale_y_continuous(limits = c(tempRange[1], tempRange[2] + 0.5)) + theme(text = element_text(size = 11, 
    family = "sans-serif"))


PrecipRange <- round(range(leipzigDat$Precip))

tempPrecip <- ggplot(data = leipzigDat, aes(y = Temp, x = Precip, color = factor(yearRegul), alpha = factor(yearRegul))) + 
    geom_segment(aes(y = mean(leipzigDat$Temp[leipzigDat$yearRegul == 0]), yend = mean(leipzigDat$Temp[leipzigDat$yearRegul == 
        0]), x = 320, xend = 720), inherit.aes = FALSE, linetype = "dashed", alpha = 0.5, col = "black", 
        data = data.frame()) + geom_segment(aes(y = 7.3, yend = 11, x = mean(leipzigDat$Precip[leipzigDat$yearRegul == 
    0]), xend = mean(leipzigDat$Precip[leipzigDat$yearRegul == 0])), inherit.aes = FALSE, linetype = "dashed", 
    alpha = 0.5, col = "black", data = data.frame()) + geom_point(size = 2) + geom_rangeframe(col = "black", 
    sides = "br") + theme_tufte() + ylab("Durchschnittstemperatur (°C)") + xlab("Niederschlag (mm)") + 
    scale_color_manual(values = c("blue", "black", "red"), breaks = c(0, 1), name = "Jahr", labels = c("1881-1999", 
        "2000-2017")) + theme_tufte(base_size = 15) + scale_y_continuous(limits = c(tempRange[1], tempRange[2] + 
    0.5), position = "right") + scale_x_continuous(limits = c(300, 800), breaks = c(400, 600, 800)) + 
    theme(legend.position = c(0, 0), legend.justification = c(0, 0), legend.title = element_text(size = 5, 
        face = "bold", hjust = 0.5)) + annotate("text", y = 7.5, x = mean(leipzigDat$Precip[leipzigDat$yearRegul == 
    0]) + 10, label = "Durchschnittswerte", size = 2, angle = 90) + scale_alpha_manual(values = c(0.4, 
    1, 1), guide = FALSE) + guides(colour = guide_legend(override.aes = list(alpha = c(0.4, 0.9)))) + 
    theme(text = element_text(size = 11, family = "sans-serif")) + theme(legend.text = element_text(size = 4))

bigPlot <- tempYear + tempPrecip + plot_layout(ncol = 2, widths = c(1.6, 1))
print(bigPlot)



# Make plots for 2019
finalYear <- 2019
lastDay <- weatherData[nrow(weatherData),"YDay"]

subWeather <- weatherData[weatherData$Year >= 1973 & weatherData$Year <= finalYear, ]
subWeather <- subWeather[subWeather$YDay <= lastDay, ]
# Make Averages of all years for Months 1-11
meanTemp19 <- aggregate(AVGTemp ~ Year, data = subWeather, FUN = mean)
sumPrecip19 <- aggregate(NiederschlagMM ~ Year, data = subWeather, FUN = sum)
sumSundur19 <- aggregate(SonnenscheinStunden ~ Year, data = subWeather, FUN = sum)

leipzigDat <- meanTemp19 %>% left_join(sumPrecip19) %>% left_join(sumSundur19)
colnames(leipzigDat) <- c("Year", "Temp", "Precip", "Sun")
leipzigDat$yearRegul <- leipzigDat$Year >= 2000
leipzigDat$yearRegul[leipzigDat$Year == finalYear] <- 2

tempRange <- round(range(leipzigDat$Temp))
tempTicks <- seq(tempRange[1] - 2, tempRange[2] + 2, by = 2)
tempsegDat <- data.frame(x = rep(1973, length(tempTicks)), xend = rep(finalYear + 0.5, length(tempTicks)), 
    y = tempTicks, yend = tempTicks)


ggplot(data = leipzigDat, aes(x = Year, y = Temp)) + geom_segment(data = tempsegDat, aes(x = x, y = y, 
    xend = xend, yend = yend), inherit.aes = FALSE, linetype = "dashed", alpha = 0.3, col = "black") + 
    geom_rangeframe(col = "black") + geom_line(alpha = 0.7, col = "red", size = 0.7) + geom_point(size = 2, 
    alpha = 0.7, col = "red") + xlab("Jahr") + ylab("Durchschnittstemperatur (°C)") + theme_tufte(base_size = 11) + 
    theme(legend.position = "none") + scale_x_continuous(breaks = c(1973, seq(1980, 2018, by = 10), finalYear)) + 
    scale_y_continuous(limits = c(tempRange[1] - 0.5, tempRange[2] + 0.5), breaks = c(-5, -3, -1, 1, 
        3, 5)) + theme(text = element_text(size = 11, family = "sans-serif"))

ggplot(data = leipzigDat, aes(y = Temp, x = Precip, color = factor(yearRegul), alpha = factor(yearRegul))) + 
    geom_segment(aes(y = mean(leipzigDat$Temp[leipzigDat$yearRegul == 0]), yend = mean(leipzigDat$Temp[leipzigDat$yearRegul == 
        0]), x = 20, xend = 120), inherit.aes = FALSE, linetype = "dashed", alpha = 0.5, col = "black", 
        data = data.frame()) + geom_segment(aes(y = -5, yend = 5, x = mean(leipzigDat$Precip[leipzigDat$yearRegul == 
    0]), xend = mean(leipzigDat$Precip[leipzigDat$yearRegul == 0])), inherit.aes = FALSE, linetype = "dashed", 
    alpha = 0.5, col = "black", data = data.frame()) + geom_point(size = 2) + geom_rangeframe(col = "black", 
    sides = "br") + theme_tufte() + ylab("Durchschnittstemperatur (°C)") + xlab("Niederschlag (mm)") + 
    scale_color_manual(values = c("blue", "black", "red"), breaks = c(0, 1), name = "Jahr", labels = c("1881-1999", 
        "2000-2018")) + theme_tufte(base_size = 15) + scale_y_continuous(limits = c(tempRange[1], tempRange[2] + 
    0.5), breaks = c(-5, -3, -1, 1, 3, 5), position = "right") + scale_x_continuous(limits = c(10, 130)) + 
    theme(legend.position = c(0, 0), legend.justification = c(0, 0), legend.title = element_text(size = 6, 
        face = "bold", hjust = 0.5)) + annotate("text", y = -4, x = mean(leipzigDat$Precip[leipzigDat$yearRegul == 
    0]) + 1, label = "Durchschnittswerte", size = 2, angle = 90) + scale_alpha_manual(values = c(0.4, 
    1, 1), guide = FALSE) + guides(colour = guide_legend(override.aes = list(alpha = c(0.4, 0.9)))) + 
    theme(text = element_text(size = 11, family = "sans-serif")) + theme(legend.text = element_text(size = 6))

