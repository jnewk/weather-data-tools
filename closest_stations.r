closest_stations <- function(lat, lon){
  library(tidyverse)
  library(geosphere)
  library(rnoaa)
  
  all_stations <- ghcnd_stations() # pull list of all stations
  # lat = latitude in decimal degrees
  # lon = longitude in decimal degrees
  
  #filter down to intermountain west
  area_stations  <- all_stations %>% filter(between(latitude, 35.09, 45.45) 
                                            & between(longitude, -117.12, -105.80)
                                            & last_year - first_year >= 30
                                            & element == "TMAX")
  # compile aggregate metric list for each station
  station_metrics <- all_stations %>% filter(id %in% area_stations$id)  %>% 
    group_by(id) %>% summarise(Metrics = toString(unique(element)))
  # use station lat/lon to calculate distance from target, add column
  lat_lon <- as.matrix(area_stations[c(3,2)], rownames = "name")
  station_dist <- distm(lat_lon, c(lon, lat))*0.000621371
  area_stations <- cbind(area_stations,station_dist)
  # sort to 10 closest stations
  top_10 <- area_stations %>% slice_min(order_by = station_dist, n = 10)
  top_10 <- left_join(top_10, station_metrics, by = "id") %>% select(-(gsn_flag:element))
  top_10$elevation <- top_10$elevation * 3.28084 # convert elevation from m to ft
  return(top_10)
}
