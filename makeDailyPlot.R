# function to turn y-axis labels into degree formatted values
dgr_fmt <- function(x, ...) {
    parse(text = paste(x, "^o", sep = ""))
}

# Subset the Temp-Data by Year (1973) & by Variable
subWeather <- weatherData[weatherData$Year >= since.year, c("Month", "Day", "Year", 
    "YDay", "AVGTemp", "MaxTemp2m", "MinTemp2m", "SonnenscheinStunden", "NiederschlagMM")]
colnames(subWeather) <- c("Month", "Day", "Year", "yearDay", "Temp", "MaxTemp", "MinTemp", 
    "SunHours", "PrecipinMM")

# Get y-Axis Limits
ylim.plot <- c(floor(min(subWeather$MinTemp)/5) * 5, ceiling(max(subWeather$MaxTemp)/5) * 
    5)

# Make Indices for the chosen year
notYearIndex <- !is.na(subWeather$Temp) & (subWeather$Year != what.year)
YearIndex <- !is.na(subWeather$Temp) & (subWeather$Year == what.year)

# Get the daily records per year-day
recordHigh <- tapply(subWeather[notYearIndex, "MaxTemp"], subWeather[notYearIndex, 
    "yearDay"], FUN = max)
recordLow <- tapply(subWeather[notYearIndex, "MinTemp"], subWeather[notYearIndex, 
    "yearDay"], FUN = min)
recordHighLowPast <- data.frame(high = recordHigh, low = recordLow, yearDay = 1:366)

####### Get the average low & highs per year-day
avgHigh <- tapply(subWeather[notYearIndex, "MaxTemp"], subWeather[notYearIndex, "yearDay"], 
    FUN = mean)
avgLow <- tapply(subWeather[notYearIndex, "MinTemp"], subWeather[notYearIndex, "yearDay"], 
    FUN = mean)
avgHighLowPast <- data.frame(high = avgHigh, low = avgLow, yearDay = 1:366)

####### Get the present high lows
highLowPresent <- subWeather[YearIndex, c("yearDay", "MaxTemp", "MinTemp")]
colnames(highLowPresent) <- c("yearDay", "high", "low")

####### Get the normal range (without the year)
tempSummary <- tapply(subWeather[notYearIndex, "Temp"], subWeather[notYearIndex, 
    "yearDay"], FUN = function(x) {
    upper <- max(x, na.rm = TRUE)
    lower <- min(x, na.rm = TRUE)
    avg <- mean(x, na.rm = TRUE)
    se <- sd(x, na.rm = TRUE)/sqrt(length(x))  #obacht bei NAs
    avg_upper <- avg + (2.101 * se)
    avg_lower = avg - (2.101 * se)
    return(c(upper, lower, avg, se, avg_upper, avg_lower))
})
pastTempRange <- as.data.frame(do.call("rbind", tempSummary))
pastTempRange$yearDay <- as.numeric(names(tempSummary))
colnames(pastTempRange) <- c("upper", "lower", "avg", "se", "avg_upper", "avg_lower", 
    "yearDay")

# Check if the high&low records were broken in the chosen
mergedRecords <- merge(x = highLowPresent, y = recordHighLowPast, by = "yearDay", 
    all.x = TRUE)
brokenHigh <- mergedRecords[mergedRecords$high.x > mergedRecords$high.y, c("yearDay", 
    "high.x")]
brokenLow <- mergedRecords[mergedRecords$low.x < mergedRecords$low.y, c("yearDay", 
    "low.x")]

################################ Create the plot Create the min-max Year Day Temperatures

# Plot the base layout
dailyPlot <- ggplot(recordHighLowPast, aes(yearDay)) + theme(plot.background = element_blank(), 
    panel.grid.minor = element_blank(), panel.grid.major = element_blank(), panel.border = element_blank(), 
    panel.background = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
# Plot the daily low/high temp records
dailyPlot <- dailyPlot + geom_linerange(recordHighLowPast, mapping = aes(x = yearDay, 
    ymin = low, ymax = high), colour = "wheat2", alpha = 0.9)

# Plot the daily low/high temp confidence interval
dailyPlot <- dailyPlot + geom_linerange(avgHighLowPast, mapping = aes(x = yearDay, 
    ymin = low, ymax = high), colour = "wheat4", alpha = 0.7)
# and as a line for better visibility
dailyPlot <- dailyPlot + geom_line(data = avgHighLowPast, aes(x = yearDay, y = low), 
    colour = "wheat4", alpha = 0.5)
dailyPlot <- dailyPlot + geom_line(data = avgHighLowPast, aes(x = yearDay, y = high), 
    colour = "wheat4", alpha = 0.5)

# Draw the max-min from the chosen year
dailyPlot <- dailyPlot + geom_linerange(highLowPresent, mapping = aes(x = yearDay, 
    ymin = low, ymax = high), colour = "sienna4", alpha = 0.8, size = 1)
# Plot the y-axis
dailyPlot <- dailyPlot + geom_vline(xintercept = 0, colour = "wheat4", linetype = 1.5, 
    size = 1)

# Horizontal grid lines (for Temperature)
dailyPlot <- dailyPlot + geom_hline(yintercept = -25, colour = "white", linetype = 1) + 
    geom_hline(yintercept = -20, colour = "white", linetype = 1) + geom_hline(yintercept = -15, 
    colour = "white", linetype = 1) + geom_hline(yintercept = -10, colour = "white", 
    linetype = 1) + geom_hline(yintercept = -5, colour = "white", linetype = 1) + 
    geom_hline(yintercept = 0, colour = "grey85", linetype = 2, alpha = 0.5) + geom_hline(yintercept = 5, 
    colour = "white", linetype = 1) + geom_hline(yintercept = 10, colour = "white", 
    linetype = 1) + geom_hline(yintercept = 15, colour = "white", linetype = 1) + 
    geom_hline(yintercept = 20, colour = "white", linetype = 1) + geom_hline(yintercept = 25, 
    colour = "white", linetype = 1) + geom_hline(yintercept = 30, colour = "white", 
    linetype = 1) + geom_hline(yintercept = 35, colour = "white", linetype = 1)
# plot the month seperator
dailyPlot <- dailyPlot + geom_vline(xintercept = 31, colour = "wheat4", linetype = 3, 
    size = 0.5) + geom_vline(xintercept = 59, colour = "wheat4", linetype = 3, size = 0.5) + 
    geom_vline(xintercept = 90, colour = "wheat4", linetype = 3, size = 0.5) + geom_vline(xintercept = 120, 
    colour = "wheat4", linetype = 3, size = 0.5) + geom_vline(xintercept = 151, colour = "wheat4", 
    linetype = 3, size = 0.5) + geom_vline(xintercept = 181, colour = "wheat4", linetype = 3, 
    size = 0.5) + geom_vline(xintercept = 212, colour = "wheat4", linetype = 3, size = 0.5) + 
    geom_vline(xintercept = 243, colour = "wheat4", linetype = 3, size = 0.5) + geom_vline(xintercept = 273, 
    colour = "wheat4", linetype = 3, size = 0.5) + geom_vline(xintercept = 304, colour = "wheat4", 
    linetype = 3, size = 0.5) + geom_vline(xintercept = 334, colour = "wheat4", linetype = 3, 
    size = 0.5) + geom_vline(xintercept = 365, colour = "wheat4", linetype = 3, size = 0.5)

ylim.ticks <- seq(ylim.plot[1], ylim.plot[2], by = 10)
a <- dgr_fmt(ylim.ticks)

# Draw the x-axis month labels
dailyPlot <- dailyPlot + coord_cartesian(ylim = ylim.plot) + scale_y_continuous(breaks = ylim.ticks, 
    labels = a,expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0), breaks = c(15, 45, 75, 105, 
    135, 165, 195, 228, 258, 288, 320, 350), labels = c("Januar", "Februar", "März", 
    "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", 
    "Dezember"))

# Draw the current broken high/low temp records
dailyPlot <- dailyPlot + geom_point(data = brokenLow, aes(x = yearDay, y = low.x), 
    colour = "dodgerblue1", shape = 21, fill = "dodgerblue1", size = 1.6) + geom_point(data = brokenHigh, 
    aes(x = yearDay, y = high.x), colour = "firebrick1", shape = 21, fill = "firebrick1", 
    size = 1.6)

print(dailyPlot)
