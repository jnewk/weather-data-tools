library(tidyverse)
library(rnoaa)
library(ggplot2)
library(lubridate)
library(geosphere)
source("C:\\Users\\jnewk\\OneDrive\\Documents\\R\\precip_ptable.r")
source("C:\\Users\\jnewk\\OneDrive\\Documents\\R\\temp_ptable.r")
source("C:\\Users\\jnewk\\OneDrive\\Documents\\R\\closest_stations.r")

 
# find closest stations to area of interest
near_stations <- closest_stations(41.13639, -111.92196) # input target latitude, longitude
View(near_stations) # look at list, pick a station
target_station <- 2 # row# from near_stations choice
  station <- near_stations$id[target_station]
  station_name <- near_stations$name[target_station]

raw_dat <- ghcnd(stationid = station, refresh = T) 
metrics <- unique(raw_dat$element)
# args: temp_ptable <- function(weather_dat, period, metric, t_hold, comp_dir )
# metric: "TMAX" "TMIN" "TOBS" "PRCP" "SNOW" "SNWD"
# comp_dir:  "greater than and equal" "geq", "greater than" "gt", "less than and equal" "leq", "less than" "lt" the value in val.

tmax <- temp_ptable(raw_dat,"WOY","TMAX", 85,"gt")
tmin <- temp_ptable(raw_dat,"WOY","TMIN", 30,"lt")
precip <- precip_ptable(raw_dat,"WOY","PRCP", .1,"gt")
snow <- precip_ptable(raw_dat,"WOY","SNOW", 1,"gt")
snowd <- precip_ptable(raw_dat,"WOY","SNWD", 1,"gt")

all_tbl <- list(tmax, tmin, precip, snow, snowd) %>% reduce(left_join, by = "WOY") 
write_csv(all_tbl, paste0("C:\\Users\\jnewk\\Downloads\\",station_name," Weather Probabilities by Week of Year.csv"))

## Plot results
library(ggplot2)
library(plotly)
library(htmlwidgets)
all_tbl_tall <- gather(all_tbl,Metric, Probability, -WOY) 
all_tbl_tall2 <- pivot_longer(all_tbl, cols = c("TMAX_gt_85","TMIN_lt_30", "PRCP_gt_0.1", "SNOW_gt_1", "SNWD_gt_1"),
                              names_to = "Metric", values_to = "Probability")

# overlay line plot
p1 <- ggplot(all_tbl_tall, aes(x = WOY, y = Probability, group = Metric, color = Metric)) + geom_line(size = 1) +
  labs(x = "Week of Year", y = "Probability of metric out of limit", title = paste(station_name,"Weather Probabilities by Week of Year"),
       subtitle = paste("Date range =", min(raw_dat$year),"to", max(raw_dat$year))) +
  geom_hline(yintercept = 25, colour = "yellow", size = 2) + geom_hline(yintercept = 50, colour = "red", size = 2)
  

ggsave(paste0("C:\\Users\\jnewk\\Downloads\\",station_name," Weather Probabilities by Week of Year.png"), p1, width = 11.5, height = 4.76, units = "in")
saveWidget(ggplotly(p1), file = paste0("C:\\Users\\jnewk\\Downloads\\",station_name," Weather Probabilities by Week of Year.html"))

# heat map
p2 <- ggplot(all_tbl_tall, aes(x = WOY,y =  Metric, fill = Probability))+geom_tile() +
  scale_fill_gradient(low = "green", high = "red") +
  labs(x = "Week of Year", y = "Metric", title = paste(station_name,"Weather Probabilities by Week of Year"),
       subtitle = paste("Date range =", min(raw_dat$year),"to", max(raw_dat$year)))

saveWidget(ggplotly(p2),file = paste0("C:\\Users\\jnewk\\Downloads\\",station_name," Weather Probabilities by Week of Year - heat map.html"))

p3 <- ggplot(all_tbl_tall, aes(x = WOY, y = Probability, group = Metric, color = Metric)) + geom_line(size = 1) +
  labs(x = "Week of Year", y = "Probability of metric out of limit", title = paste(station_name,"Weather Probabilities by Week of Year"),
       subtitle = paste("Date range =", min(raw_dat$year),"to", max(raw_dat$year))) +
  geom_hline(yintercept = 25, colour = "yellow", size = 2) + geom_hline(yintercept = 50, colour = "red", size = 2)
