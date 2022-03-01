precip_ptable <- function(weather_dat, period, metric, t_hold, comp_dir ){
  library(tidyverse)
  library(lubridate)
# metric: "TMAX" "TMIN" "TOBS" "PRCP" "SNOW" "SNWD"
# comp_dir:  "greater than and equal" "geq", "greater than" "gt", "less than and equal" "leq", "less than" "lt" the value in val.
  
  fac_cols <- c("year", "month", "day","DOY","WOY")
  conv_inch <-  switch(metric, "PRCP" = 254, "SNOW" = 25.4, "SNWD" = 25.4)
  
  dat <- weather_dat %>% filter(element == metric) %>% 
    gather(key = "value_day", value = "Precip",all_of(grep("VALUE",names(weather_dat)))) %>% 
    mutate(day = as.integer(gsub("VALUE", "",value_day))) %>% 
    mutate(Precip_in = as.numeric(Precip/conv_inch)) %>% 
    filter(!is.na(Precip)) %>% 
    mutate(DOY = yday(ymd(paste(year,month,day,sep = "-")))) %>% 
    mutate(WOY = isoweek(ymd(paste(year,month,day,sep = "-"))))
  dat <- dat[, grep("FLAG", names(dat), invert = T)]
  dat[, fac_cols] <- lapply(dat[, fac_cols], factor) # change y, m, d columns to factor
  
  p_out <- dat %>% group_by(get(period)) %>% 
    summarise(pcnt_out = FSA::perc(Precip_in, t_hold, dir = comp_dir))
  names(p_out) <- c(period,paste0(metric,"_",comp_dir,"_",t_hold))
  
  return(p_out)
  
} #end function