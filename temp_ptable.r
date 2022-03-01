temp_ptable <- function(weather_dat, period, metric, t_hold, comp_dir ){
  library(tidyverse)
  library(lubridate)
  
  #dir  "greater than and equal" "geq", "greater than" "gt", "less than and equal" "leq", "less than" "lt" the value in val.
  
  fac_cols <- c("year", "month", "day","DOY","WOY")
  
  dat <- weather_dat %>% filter(element == metric) %>% 
    gather(key = "value_day", value = "Temp_C",all_of(grep("VALUE",names(weather_dat)))) %>% 
    mutate(day = as.integer(gsub("VALUE", "",value_day))) %>% 
    mutate(Temp_F = as.numeric((9/50)* Temp_C + 32)) %>% 
    filter(!is.na(Temp_F)) %>% 
    mutate(DOY = yday(paste(year,month,day,sep = "-"))) %>% 
    mutate(WOY = week(paste(year,month,day,sep = "-")))
  dat <- dat[, grep("FLAG", names(dat), invert = T)]
  dat[, fac_cols] <- lapply(dat[, fac_cols], factor) # change y, m, d columns to factor
  p_out <- dat %>% group_by(get(period)) %>% 
    summarise(pcnt_out = FSA::perc(Temp_F, t_hold, dir = comp_dir), avg_temp = mean(Temp_F))
  names(p_out) <- c(period,paste0(metric,"_",comp_dir,"_",t_hold),paste0(metric,"_","avg"))
  
  return(p_out)
  
} #end function