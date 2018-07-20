GetShiftDay <- function(des.day){
  library(jsonlite)
  library(httr)
  library(data.table)
  
  source("../utils/get_fleets_id.R", local = T)
  source("../utils/utils.R", local = T)
  source("../utils/constants/credentials.R", local = T)
  
  fleets <- get_fleets_id()
  shifts.all <- data.table()
  
  rename.from <- c("shiftDefinition.toHourOfDay", "shiftDefinition.fromHourOfDay")
  rename.to <- c("hour.to", "hour.from")
  final.feats <- c("duration", "fleet", "hour.from", "hour.to")
  return.feats <- c("date", "shift", "fleet", "duration")
  
  for (i in 1:nrow(fleets)){
    shifts <- GET(paste(alyx_url, fleets$city[i], "/fleets/", 
                     fleets$id[i], "/reach/", des.day, "/instant", sep = ""))
    shifts <- fromJSON(content(shifts, as = "text"))
    shifts <- shifts$data$fleetPlan$assignments
    
    if ( !is.null(unlist(shifts) )){
      shifts <- data.table(fleet = fleets$fleet[i], flatten(shifts))
      shifts <- shifts[!is.na(shiftDefinition.finishAtPlace.name)]
      shifts <- shifts[shiftDefinition.finishAtPlace.name != "Corporate"]
      
      setnames(shifts, rename.from, rename.to)
      shifts[, duration := hour.to - hour.from]
      shifts <- shifts[, final.feats, with = F]
      shifts.all <- rbind(shifts.all, shifts, use.names = T, fill = T)
    }
  }
  shifts.all[, date := des.day]
  if(nrow(shifts.all)!=0){shifts.all <- update_fleets(shifts.all)}
  shifts.all[, time := paste(floor(hour.from), (hour.from - floor(hour.from)) * 60,
                             sep = ":")]
  shifts.all[, date := paste(date, time, sep = ":")]
  shifts.all[, date := strptime(date, format = "%Y-%m-%d:%H:%M")]
  
  shifts.all$city <- get_city_by_fleet(shifts.all)
  shifts.all$shift <- get_shift(shifts.all$date, shifts.all$city)
  
  shifts.all[, date := as.Date(date)]
  
  return(unique(shifts.all[, return.feats, with = F]))
}