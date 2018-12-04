# Subset Saxony
saxonyDat <- regioAll[regioAll$Bundesland == "Sachsen",]

# Make Averages of all years for Months 1-11
saxonyMeanTemp1_11 <- tapply(saxonyDat[saxonyDat$Monat %in% 1:11, "AvgTemp"], 
                         saxonyDat[saxonyDat$Monat %in% 1:11,  "Jahr"], FUN = mean)
saxonyMeanPrecip1_11 <- tapply(saxonyDat[saxonyDat$Monat %in% 1:11, "PrecipMM"], 
                             saxonyDat[saxonyDat$Monat %in% 1:11,  "Jahr"], FUN = mean)
saxonyMeans <- data.frame(Jahr = as.numeric(names(saxonyMeanPrecip1_11)), 
                          Temp = saxonyMeanTemp1_11, Precip = saxonyMeanPrecip1_11)
saxonyMeans$yearRegul <- saxonyMeans$Jahr >= 2000
saxonyMeans$yearRegul[saxonyMeans$Jahr == 2018] <- 2

# Plot average Temp over the Years
tempYear <- ggplot(data = saxonyMeans, aes(x = Jahr, y = Temp, color = factor(yearRegul))) + geom_rangeframe(col="black")+
  #geom_line(alpha = 0.5) + 
  geom_point(size=3, alpha = 0.5) +
  theme_tufte(base_size = 15) +
  theme(axis.title=element_blank(),legend.position = "none")+scale_color_manual(values = c("blue","black", "red"))+
  scale_x_continuous(breaks = c(seq(1880,2018,by=20),2018))

# Plot precip against Temp
tempPrecip <- ggplot(data = saxonyMeans, aes(x = Temp, y = Precip, color = factor(yearRegul))) + geom_point(size = 3, alpha = 0.5) + geom_rangeframe(col="black") + theme_tufte() +
  xlab("Durchschnittstemperatur") + ylab("Niederschlag") + scale_color_manual(values = c("blue", "black", "red"))+
  theme(axis.title.x = element_text(vjust=-0.5), axis.title.y = element_text(vjust=1.5),legend.position = "none")+
  geom_hline(yintercept = mean(saxonyMeans$Precip[saxonyMeans$yearRegul==0]), linetype = "dashed", alpha = 0.5)+
  geom_vline(xintercept = mean(saxonyMeans$Temp[saxonyMeans$yearRegul==0]), linetype = "dashed", alpha = 0.5)

