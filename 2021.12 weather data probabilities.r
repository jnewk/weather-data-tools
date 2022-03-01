library(tidyverse)
library(rnoaa)
library(ggplot2)
library(lubridate)
library(geosphere)
source("C:\\Users\\jnewk\\OneDrive\\Documents\\R\\precip_ptable.r")
source("C:\\Users\\jnewk\\OneDrive\\Documents\\R\\temp_ptable.r")

# Email:	jnewkirk458@gmail.com
noaakey <- "AGtBhHxpFIgWoVoEedhKganvyJTjZExZ"

# find right station
all_stations <- ghcnd_stations()
area_stations  <- all_stations %>% filter(between(latitude, 35.73469,46.26097) 
                                          & between(longitude, -114.49934, -105.00715) 
                                          & last_year - first_year > 30 
                                          & element == "PRCP")
lat_lon <- as.matrix(area_stations[c(3,2)], rownames = "name")


36.82139, -111.62630 # Lees Ferry
37.52429, -109.89564 # Kane Gulch RS
38.47367, -110.20052 # Horseshoe Canyon TH

station_dist <- distm(lat_lon, rev(c(38.47367, -110.20052)))*0.000621371
area_stations <- cbind(area_stations,station_dist)

# Needles station = USCsou00421168, Bryce Canyon = USC00421008, Kodachrome = USC00424755, Capitol Reef = USC00421171
# Blanding = USC00420738, Natural Bridges = USC00426053, BULLFROG BASIN = USC00421020
# LEES FERRY = USC00024849, BRYCE CANYON NP HQRS = USC00421008, KANE GULCH - BLANDING 23WSW = USR0000KANE
# WEBER BASIN PUMP PLT 3 = USC00429346, Emigrant Summit = USS0011G06S
station <- "USC00426053"
station_name <- all_stations$name[all_stations$id == station][1]

raw_dat <- ghcnd(stationid = station, refresh = T) %>% filter(year >= 1965)
metrics <- unique(raw_dat$element)
# args: temp_ptable <- function(weather_dat, period, metric, t_hold, comp_dir )
# metric: "TMAX" "TMIN" "TOBS" "PRCP" "SNOW" "SNWD"
# comp_dir:  "greater than and equal" "geq", "greater than" "gt", "less than and equal" "leq", "less than" "lt" the value in val.

tmax <- temp_ptable(raw_dat,"WOY","TMAX", 85,"gt")
tmin <- temp_ptable(raw_dat,"WOY","TMIN", 25,"lt")
precip <- precip_ptable(raw_dat,"WOY","PRCP", .1,"gt")
snow <- precip_ptable(raw_dat,"WOY","SNOW", 1,"gt")
snowd <- precip_ptable(raw_dat,"WOY","SNWD", 1,"gt")

all_tbl <- list(tmax, tmin, precip, snow, snowd) %>% reduce(left_join, by = "WOY") 
write_csv(all_tbl, paste0("C:\\Users\\jnewk\\Downloads\\",station_name," Weather Probabilities by Week of Year.csv"))

library(ggplot2)
all_tbl_tall <- gather(all_tbl,Metric, Probability, -WOY)

p1 <- ggplot(all_tbl_tall, aes(x = WOY, y = Probability, group = Metric, color = Metric)) + geom_line(size = 1) +
  labs(x = "Week of Year", y = "Probability of metric out of limit", title = paste(station_name,"Weather Probabilities by Week of Year")) +
  geom_hline(yintercept = 25, colour = "yellow", size = 2) + geom_hline(yintercept = 50, colour = "red", size = 2)

ggsave(paste0("C:\\Users\\jnewk\\Downloads\\",station_name," Weather Probabilities by Week of Year.png"), p1, width = 11.5, height = 4.76, units = "in")

library(plotly)
ggplotly(p1)

p2 <- ggplot(all_tbl_tall, aes(x = WOY,y =  Metric, fill = Probability))+geom_tile() +
  scale_fill_gradient(low = "green", high = "red")
ggplotly(p2)
