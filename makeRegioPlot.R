# Subset Saxony
saxonyDat <- regioAll[regioAll$Bundesland == "Sachsen", ]

# Make Averages of all years for Months 1-11
saxonyMeanTemp1_11 <- tapply(saxonyDat[saxonyDat$Monat %in% 1:11, "AvgTemp"], saxonyDat[saxonyDat$Monat %in% 
    1:11, "Jahr"], FUN = mean)
saxonyMeanPrecip1_11 <- tapply(saxonyDat[saxonyDat$Monat %in% 1:11, "PrecipMM"], 
    saxonyDat[saxonyDat$Monat %in% 1:11, "Jahr"], FUN = sum)
saxonyMeans <- data.frame(Jahr = as.numeric(names(saxonyMeanPrecip1_11)), Temp = saxonyMeanTemp1_11, 
    Precip = saxonyMeanPrecip1_11)
saxonyMeans$yearRegul <- saxonyMeans$Jahr >= 2000
saxonyMeans$yearRegul[saxonyMeans$Jahr == 2018] <- 2

# Plot average Temp over the Years
tempYear <- ggplot(data = saxonyMeans, aes(x = Jahr, y = Temp, color = factor(yearRegul))) + 
    geom_rangeframe(col = "black") +  geom_line(alpha = 0.7, col ="grey") +xlab("Jahr")+
  ylab("Durchschnittstemperatur")+
geom_point(size = 3, alpha = 0.65) + theme_tufte(base_size = 15) + theme(legend.position = "none") + scale_color_manual(values = c("blue", "black", "red")) + 
    scale_x_continuous(breaks = c(1881,seq(1900, 2018, by = 20), 2018))+scale_y_continuous(expand = expand_scale(mult=.15))

# Plot precip against Temp
tempPrecip <- ggplot(data = saxonyMeans, aes(x = Temp, y = Precip, color = factor(yearRegul))) + 
    geom_point(size = 3.5, alpha = 0.7) + geom_rangeframe(col = "black") + theme_tufte() + 
    xlab("Durchschnittstemperatur") + ylab("Niederschlag in mm") + scale_color_manual(values = c("blue", 
    "black", "red"), name = "Jahr", labels = c("1881-1999", "2000-2017", "2018")) + theme_tufte(base_size = 15) +# theme(legend.position = "none") + 
  geom_hline(yintercept = mean(saxonyMeans$Precip[saxonyMeans$yearRegul == 
    0]), linetype = "dashed", alpha = 0.5) + geom_vline(xintercept = mean(saxonyMeans$Temp[saxonyMeans$yearRegul == 
    0]), linetype = "dashed", alpha = 0.5)+scale_x_continuous(expand = expand_scale(mult=.15))+
  scale_y_continuous(expand = expand_scale(mult=.15))+theme(legend.position = c(0.9,0.9),
                                                            legend.background = element_rect(fill="grey90",
                                                                                             size=0.5, linetype="solid", 
                                                                                             colour ="black"),
                                                            legend.title = element_text(size=16, face="bold", hjust = 0.5))
  

