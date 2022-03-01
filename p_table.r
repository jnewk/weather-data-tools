dat_gathered_precip$[which.max(dat_gathered_precip$Precip_in),c("year", "month","day")]

dat_gathered_precip[which.max(dat_gathered_precip$Precip_in),]


test <- dat_gathered_tmax %>% summarise()

wk22_dat <- dat_gathered_tmax[dat_gathered_tmax$WOY == "22",]

above_85 <- sum(wk22_dat$Temp_F >= 85)
p_85 <- above_85/nrow(wk22_dat)


p_table <- function(weather_dat, t_upper, t_lower, precip_thresh, snow_thresh, swowd_thresh){
  fac_cols <- c("year", "month", "day","DOY","WOY")
  ## Temperature Tmax
  dat_tmax <- raw_dat %>% filter(element == "TMAX")
  value_cols <- grep("VALUE",names(dat_tmax))
  
  tmax_dat <- weather_dat %>% filter(element == "TMAX") %>% 
    gather(key = "value_day", value = "Temp_C",all_of(grep("VALUE",names(weather_dat)))) %>% 
    mutate(day = as.integer(gsub("VALUE", "",value_day))) %>% 
    mutate(Temp_F = as.numeric((9/50)* Temp_C + 32)) %>% 
    filter(!is.na(Temp_F)) %>% 
    mutate(DOY = yday(paste(year,month,day,sep = "-"))) %>% 
    mutate(WOY = week(paste(year,month,day,sep = "-")))
  tmax_dat <- tmax_dat[, grep("FLAG", names(tmax_dat), invert = T)]
  tmax_dat[, fac_cols] <- lapply(tmax_dat[, fac_cols], factor) # change y, m, d columns to factor
  p_tmax <- tmax_dat %>% group_by(WOY) %>% 
    summarise(p_over = sum())
  
} #end function