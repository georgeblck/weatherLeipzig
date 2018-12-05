leipzigDat <- subWeather

# Make Averages of all years for Months 1-11
leipzigMeanTemp1_11 <- tapply(leipzigDat[leipzigDat$yearDay %in% 1:337, "Temp"], 
    leipzigDat[leipzigDat$yearDay %in% 1:337, "Year"], FUN = mean)
leipzigMeanPrecip1_11 <- tapply(leipzigDat[leipzigDat$yearDay %in% 1:337, "PrecipinMM"], 
    leipzigDat[leipzigDat$yearDay %in% 1:337, "Year"], FUN = sum)
leipzigMeanSunDur1_11 <- tapply(leipzigDat[leipzigDat$yearDay %in% 1:337, "SunHours"], 
    leipzigDat[leipzigDat$yearDay %in% 1:337, "Year"], FUN = sum)
leipzigMeans <- data.frame(Jahr = as.numeric(names(leipzigMeanPrecip1_11)), Temp = leipzigMeanTemp1_11, 
    Precip = leipzigMeanPrecip1_11, sunHours = leipzigMeanSunDur1_11)
leipzigMeans$yearRegul <- leipzigMeans$Jahr >= 2000
leipzigMeans$yearRegul[leipzigMeans$Jahr == 2018] <- 2

# Plot average Temp over the Years
tempYear <- ggplot(data = leipzigMeans, aes(x = Jahr, y = Temp)) + geom_segment(aes(x = 1973, 
    xend = 2018.5, y = 11, yend = 11), inherit.aes = FALSE, linetype = "dashed", 
    alpha = 0.3, col = "black", data = data.frame()) + geom_segment(aes(x = 1973, 
    xend = 2018.5, y = 10, yend = 10), inherit.aes = FALSE, linetype = "dashed", 
    alpha = 0.3, col = "black", data = data.frame()) + geom_segment(aes(x = 1973, 
    xend = 2018.5, y = 9, yend = 9), inherit.aes = FALSE, linetype = "dashed", alpha = 0.3, 
    col = "black", data = data.frame()) + geom_segment(aes(x = 1973, xend = 2018.5, 
    y = 8, yend = 8), inherit.aes = FALSE, linetype = "dashed", alpha = 0.3, col = "black", 
    data = data.frame()) + geom_segment(aes(x = 1973, xend = 2018.5, y = 7, yend = 7), 
    inherit.aes = FALSE, linetype = "dashed", alpha = 0.3, col = "black", data = data.frame()) + 
    geom_segment(aes(x = 1973, xend = 2018.5, y = 12, yend = 12), inherit.aes = FALSE, 
        linetype = "dashed", alpha = 0.3, col = "black", data = data.frame()) + geom_rangeframe(col = "black") + 
    geom_line(alpha = 0.7, col = "red", size = 1) + xlab("Jahr") + ylab("Durchschnittstemperatur (°C)") + 
    geom_point(size = 3, alpha = 0.7, col = "red") + theme_tufte(base_size = 15) + 
    theme(legend.position = "none") + scale_x_continuous(breaks = c(1973, seq(1980, 
    2018, by = 10), 2018)) + scale_y_continuous(limits = c(8, 12)) + theme(text = element_text(size = 16, 
    family = "sans-serif"))

# Plot Sunhour data
ggplot(data = na.omit(leipzigMeans), aes(x = Jahr, y = sunHours/365)) + geom_line(alpha = 0.7, 
    col = "darkorange", size = 1) + xlab("Jahr") + ylab("Sonnenstunden") + geom_point(size = 3, 
    alpha = 0.7, col = "darkorange") + theme_tufte(base_size = 15) + theme(legend.position = "none") + 
    scale_x_continuous(limits = c(1973, 2017), breaks = c(1973, seq(1980, 2018, by = 10), 
        2017)) + geom_rangeframe(col = "black") + geom_smooth(se = FALSE)
print(sunYear)


# Plot precip against Temp
tempPrecip <- ggplot(data = leipzigMeans, aes(y = Temp, x = Precip, color = factor(yearRegul), 
    alpha = factor(yearRegul))) + geom_segment(aes(y = mean(leipzigMeans$Temp[leipzigMeans$yearRegul == 
    0]), yend = mean(leipzigMeans$Temp[leipzigMeans$yearRegul == 0]), x = 280, xend = 740), 
    inherit.aes = FALSE, linetype = "dashed", alpha = 0.5, col = "black", data = data.frame()) + 
    geom_segment(aes(y = 8, yend = 11.5, x = mean(leipzigMeans$Precip[leipzigMeans$yearRegul == 
        0]), xend = mean(leipzigMeans$Precip[leipzigMeans$yearRegul == 0])), inherit.aes = FALSE, 
        linetype = "dashed", alpha = 0.5, col = "black", data = data.frame()) + geom_point(size = 3) + 
    geom_rangeframe(col = "black", sides = "br") + theme_tufte() + ylab("Durchschnittstemperatur (°C)") + 
    xlab("Niederschlag (mm)") + scale_color_manual(values = c("blue", "black", "red"), 
    breaks = c(0, 1), name = "Jahr", labels = c("1973-1999", "2000-2017")) + theme_tufte(base_size = 15) + 
    scale_y_continuous(limits = c(8, 12), position = "right") + scale_x_continuous(limits = c(250, 
    750)) + theme(legend.position = c(0, 0), legend.justification = c(0, 0), legend.background = element_rect(fill = "grey90", 
    size = 0.5, linetype = "solid", colour = "black"), legend.title = element_text(size = 16, 
    face = "bold", hjust = 0.5)) + annotate("text", y = 8.2, x = mean(leipzigMeans$Precip[leipzigMeans$yearRegul == 
    0]) - 4, label = "Durchschnittswerte", size = 3, angle = 90) + scale_alpha_manual(values = c(0.4, 
    0.9, 0.9), guide = FALSE) + guides(colour = guide_legend(override.aes = list(alpha = c(0.4, 
    0.9)))) + theme(text = element_text(size = 16, family = "sans-serif"))

tempYear + tempPrecip + plot_layout(ncol = 2, widths = c(1.6, 1))
