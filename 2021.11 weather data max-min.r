library(tidyverse)
library(rnoaa)
library(ggplot2)
library(lubridate)
library(geosphere)


# Email:	jnewkirk458@gmail.com
noaakey <- "AGtBhHxpFIgWoVoEedhKganvyJTjZExZ"

all_stations <- ghcnd_stations()
area_stations  <- all_stations %>% filter(between(latitude, 36.9,37.7) 
                                        & between(longitude, -112.1, -109.9) 
                                        & last_year - first_year > 30 
                                        & element == "PRCP")
lat_lon <- as.matrix(area_stations[c(3,2)], rownames = "name")

station_dist <- distm(lat_lon, c(-110.98595,37.42558))*0.000621371
area_stations <- cbind(area_stations,station_dist)

# Needles station = USC00421168, Bryce Canyon = USC00421008, Kodachrome = USC00424755, Capitol Reef = USC00421171
# Blanding = USC00420738, Natural Bridges = USC00426053
station <- "USC00426053"
station_name <- area_stations$name[area_stations$id == station]
metrics_list <- ncdc_datacats(stationid = paste0("GHCND:",station), token = noaakey, limit = 1000)
metrics_table <- as.data.frame(metrics_list$data)

raw_dat <- ghcnd(stationid = station, refresh = T) %>% filter(year >= 1965)
metrics <- unique(raw_dat$element)

## Temperature Tmax
dat_gathered_tmax <- raw_dat %>% filter(element == "TMAX") %>% 
  gather(key = "value_day", value = "Tmax_C",contains("VALUE")) %>% #make long table of temp values
  filter(!is.na(Tmax_C)) %>% 
  mutate(day = as.integer(gsub("VALUE", "",value_day))) %>% #extract day of month from value labels
  mutate(Tmax_F = as.numeric((9/50)* Tmax_C + 32)) %>% # make F units column
  mutate(Date = as.POSIXlt(paste(year,month,day,sep = "-"),format = "%Y-%m-%d")) %>% # make date col
  mutate(WOY = week(Date)) %>% # make week of year column
  select(!contains("FLAG")) %>% # remove FLAG columns
  mutate(across(c("year", "month", "WOY"),factor)) # make some cols as factor for plotting
  
dat_gathered_tmin <- raw_dat %>% filter(element == "TMIN") %>% 
  gather(key = "value_day", value = "Tmin_C",contains("VALUE")) %>% #make long table of temp values
  filter(!is.na(Tmin_C)) %>% 
  mutate(day = as.integer(gsub("VALUE", "",value_day))) %>% #extract day of month from value labels
  mutate(Tmin_F = as.numeric((9/50)* Tmin_C + 32)) %>% # make F units column
  mutate(Date = as.POSIXlt(paste(year,month,day,sep = "-"),format = "%Y-%m-%d")) %>% # make date col
  mutate(WOY = week(Date)) %>% # make week of year column
  select(!contains("FLAG")) %>% # remove FLAG columns
  mutate(across(c("year", "month", "WOY"),factor)) # make some cols as factor for plotting

dat_range <- full_join(dat_gathered_tmax,dat_gathered_tmin, by = c("Date", "WOY", "month", "id", "day", "year")) %>% 
  mutate(Temp_Range = Tmax_F - Tmin_F) %>% 
  filter(!is.na(Temp_Range)) %>% 
  select(!contains(c("element", "value_day", "_C"))) %>% 
  filter(Temp_Range > 0) # eliminate cases min > max
 
min_temp_by_week <- lm(Tmin_F ~ WOY, dat_range)
temp_range_by_week <- lm(Temp_Range ~ WOY, dat_range)
temp_range_by_month <- lm(Temp_Range ~ month, dat_range)

library(broom)
test2 <- broom::tidy.lm(temp_range_by_week)
dat_range[which.max(abs(test2)),]

dat_range <- dat_range[-which.max(abs(test2)),]

range_by_month <- dat_range %>% group_by(month) %>% dplyr::summarise(Mean_range = mean(Temp_Range))
range_by_week <- dat_range %>% group_by(WOY) %>% dplyr::summarise(Mean_range = mean(Temp_Range))

ggplot(range_by_week, aes(WOY, Mean_range)) + geom_point()

ggplot(range_by_month, aes(x = month, y = Mean_range)) + geom_line() + geom_point()

ggplotly()

Q <- quantile(temp_range_by_week$residuals, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(temp_range_by_week$residuals)
outliers <- which(temp_range_by_week$residuals > Q[2]+1.5*iqr | temp_range_by_week$residuals < Q[1]-1.5*iqr) # Lower Range???
  
test2 <- anova(min_temp_by_week)

library(ggstatsplot)
ggbetweenstats(dat_range,month, Temp_Range, outlier.tagging = TRUE)

