rm(list=ls())

library(lubridate)
library(tidyverse)
library(ggthemes)
library(formatR)
library(corrplot)
library(factoextra)
library(parallelDist)
library(heatmap3)

# Make own dist-plot function
plotDistus <- function(dist, gradient, order = FALSE){
  library(ggplot2)
  if(order){
    res.hc <- stats::hclust(dist, method = "ward.D2")
    dist.obj <- as.matrix(dist)[res.hc$order, res.hc$order]
  } else {
    dist.obj <- as.matrix(dist)
  }
  rownames(dist.obj) <- colnames(dist.obj) <- paste0(rownames(dist.obj), 
                                                     "-")
  d <- reshape2::melt(dist.obj)
  p <- ggplot(d, aes_string(x = "Var1", y = "Var2")) + ggplot2::geom_tile(aes_string(fill = "value"))+
    ggplot2::scale_fill_gradient2(midpoint = mean(dist.obj), 
                                  low = gradient$low, mid = gradient$mid, high = gradient$high, 
                                  space = "Lab")+theme(axis.text = element_blank(), axis.ticks = element_blank(), 
                                                       axis.title.x = element_blank(), axis.title.y = element_blank())+theme(legend.position="none")
  return(p)
}
#fviz_dist(dailyDist, show_labels = FALSE, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), order = TRUE)

library(rdwd)
library(lubridate)
library(tidyverse)
library(ggthemes)
link <- selectDWD("Leipzig/Halle", res="10_minutes", var="solar", per="historic")
file <- dataDWD(link, read=FALSE, dir="DWDdata", quiet=TRUE, force=NA)
clim <- readDWD(file, varnames=TRUE)
clim[[2]] %>% mutate(year = year(MESS_DATUM), hour = hour(MESS_DATUM),
                yday = yday(MESS_DATUM)) %>% 
  select(c(yday,hour,SD_10.Sonnenscheindauer))%>%
  gather(variable,temp, -c(yday,hour)) %>% group_by(yday, hour) %>% summarise_at("temp", mean, na.rm=TRUE)%>%
  mutate(temp = ifelse(temp!=0, temp, NA))%>%
  ggplot(aes(x=yday,y=hour,fill=ntile(temp,100))) + geom_raster()+
  scale_fill_viridis_c(option = "A", direction = -1, na.value = "white")+
  theme_tufte(base_size = 15) + theme(axis.title = element_blank())+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank())+theme(legend.position="none")

p <- ggplot(subset(britain, age < 101), aes(x = year, y = age, fill = ntile(male, 100)))
p_out <- p + geom_raster() +
  scale_fill_viridis_c(option = "A", direction = -1) +
  scale_x_continuous(breaks = seq(1845, 2015, by = 15)) +
  ylim(c(0, 100)) +
  guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
  labs(x = "Year", y = "Age", fill = "Male Death Rate Percentile",
       title = "Male Mortality Rates in England and Wales, 1841-2016",
       subtitle = "Binned by percentile",
       caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
  theme(legend.position = "top",
        legend.title = element_text(size = 8))

p_out




# Daten einlesen
dailyData <- read.table("data/dailyData.csv", header = TRUE, dec = ".", sep = ";") %>% filter(Year >= 2002, Year < 2010) %>%
  select(-c(1,2,5,7,18,19,20,21)) 
sapply(dailyData, function(y) sum(length(which(is.na(y)))))
dailyData <- na.omit(dailyData)

# Make Distance
dailyDist <- parallelDist(as.matrix(dailyData), threads = 4)

# Make Plots
distplot <- plotDistus(dailyDist, gradient = list(low = "#07d3fc", mid = "black", high = "#FC4E07"), order = FALSE)
#print(distplot)
ggsave("distplot.pdf", plot = distplot, device = cairo_pdf, width = 1250, height = 1250, units = "mm")
