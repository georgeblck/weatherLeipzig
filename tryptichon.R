rm(list=ls())

library(rdwd)
library(lubridate)
library(tidyverse)
library(ggthemes)
library(naniar)

# Load data
link <- selectDWD("Leipzig/Halle", res="hourly", var="air_temperature", per="hr", outvec = TRUE)
file <- dataDWD(link, read=FALSE, dir="DWDdata", quiet=TRUE, force=NA)
clim <- readDWD(file, varnames=TRUE)
clim <- as.data.frame(data.table::rbindlist(clim)) 

# Work on data
climDat <- clim %>% distinct(MESS_DATUM, .keep_all = TRUE) %>% arrange(MESS_DATUM) 
colnames(climDat) <- c("ID", "Datum", "QN", "Temp", "Hum", "eor")
climDat <- climDat %>% mutate(year = year(Datum), hour = hour(Datum),
                          yday = yday(Datum))
# look at missing values
gg_miss_span(climDat, Temp, span_every = 8760)

# Do it
tempPlot <- climDat %>% filter(year <= 2017, year > 1972) %>% group_by(yday, hour) %>% 
  summarise_at("Temp", mean, na.rm=TRUE) %>%
  #mutate(temp = ifelse(temp!=0, temp, NA))%>%
  ggplot(aes(x=yday,y=hour,fill=Temp)) + geom_raster()+
  scale_fill_viridis_c(option = "A", direction = 1, na.value = "white")+
  theme_tufte(base_size = 15) + theme(axis.title = element_blank())+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank())+theme(legend.position="none")
print(tempPlot)

climDat %>% filter(year <= 2018, year > 1972) %>% mutate(c2018 = year == 2018) %>% 
  group_by(c2018,yday, hour) %>% 
  summarise_at("Temp", mean, na.rm=TRUE) %>% ungroup() %>% filter(yday <= 365) %>% group_by(yday,hour) %>%
  summarise_at("Temp", diff) %>%
  ggplot(aes(x=yday,y=hour,fill=Temp)) + geom_raster()+
  scale_fill_viridis_c(option = "A", direction = 1, na.value = "white")+
  theme_tufte(base_size = 15) + theme(axis.title = element_blank())+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank())#+theme(legend.position="none")

climDat %>% filter(year < 2018, year > 1972)%>% 
  group_by(yday, hour) %>% 
  summarise_at("Temp", list(mean=mean, sd = sd), na.rm=TRUE) %>% ungroup() %>% filter(yday <= 365) %>% 
  mutate(Temp = mean+1*sd) %>% select(-c(sd, mean))%>% rbind(climDat[climDat$year==2018, c("yday", "hour", "Temp")])%>%
  group_by(yday,hour) %>%
  summarise_at("Temp", diff) %>% ungroup()%>%
  ggplot(aes(x=yday,y=hour,fill=Temp>0)) + geom_raster()+
  theme_tufte(base_size = 15) + theme(axis.title = element_blank())+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank())+theme(legend.position="none")

