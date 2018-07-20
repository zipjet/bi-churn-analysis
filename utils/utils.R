GetMongoTable <- function(collection_name, query, fields){
  library(data.table)
  library(jsonlite)
  
  
  source("utils/utils.R", local = T)

  
  mongo.handle <- get_mongo_handle(collection_name)
  mongo.table <- mongo.handle$find(
    query = query,
    fields = get_fields_query(fields)
  )

  return(data.table(flatten(mongo.table)))
}


get_mongo_handle <- function(collection_name){
  library(mongolite)
  source("utils/constants/credentials.R", local = T)

  for (i in 1:length(db_credentials$dbs)) {
    if (collection_name %in% db_credentials$collections[[i]]){
      return ( mongo(collection=collection_name, db = db_credentials$dbs[i],
                     url = db_credentials$urls[[i]], verbose=T) )
    }
  }
  stop("Collection not found")
}


factor_to_char <- function(data){
  factor_colnames <- colnames(data)[data[,lapply(.SD, class)=="factor"]]
  data[,(factor_colnames) := lapply(.SD, as.character),
       .SDcols = factor_colnames]

  return(data)
}


set_timezone <- function(tasks){
  library(lubridate)
  source("utils/constants/fleets.R", local=T)

  for (city_name in names(city_tz)){
    tz = city_tz[[city_name]]
    idxs <- tasks[, .I[city==city_name]]
    tasks$date[idxs] <- as.character(with_tz(tasks[idxs, date], tzone=tz))
  }

  return (tasks)
}


get_shift <- function(time_posix, city){
  source("utils/constants/fleets.R", local=T)

  get_shift_helper <- function(x, trans){
    return (ifelse(strptime(x, format="%H:%M") < trans, morning_shift, evening_shift))
  }

  shft_trans <- strptime("14:00", format="%H:%M")
  shft_trans_paris_mnd <- strptime("13:00", format="%H:%M")

  dt <- data.table(
    "time" = sapply(time_posix, function(x) strftime(x, format="%H:%M")),
    "city" = city,
    "day_of_week" = weekdays(time_posix)
  )

  dt[, shift := lapply(time, get_shift_helper, trans=shft_trans)]
  dt[city == "Paris" & day_of_week %in% c("Monday", "Thursday"),
     shift := lapply(time, get_shift_helper, trans=shft_trans_paris_mnd)]

  return(as.character(dt$shift))
}


get_time_range_query <- function(time_field, start_date = NA,
                                 end_date = Sys.Date()){
  
  library(methods)
  date_to_bit64 <- function(date){
    library(bit64)
    return (as.integer64(as.integer(as.POSIXct(date,"%Y-%m-%d")) * 1000))
  }


  if (!is(start_date, "Date")) start_date <- as.Date(start_date)
  if (!is(start_date, "Date")) end_date <- as.Date(end_date)

  if (!is.na(start_date)){
    query <- paste0(
      '{"',
      time_field,
      '": {"$gte": { "$date" : { "$numberLong" : "',
      date_to_bit64(start_date),
      '"}}, "$lt": { "$date" : { "$numberLong" : "',
      date_to_bit64(end_date),
      '"}} }}')

    return(query)
  }

  return ('{}')
}


get_fields_query <- function(my_fields){
  fields <- paste(paste0('"', my_fields, '" : 1,'), collapse = "")
  fields <- substr(fields, 1, nchar(fields) - 1)
  fields <- paste0("{", fields, "}")

  return(fields)
}


get_city <- function(data, id.col = "id"){
  source("utils/constants/fleets.R", local = T)

  for(i in 1:length(city_codes)){
    data[grepl(city_codes[i], data[[id.col]]), city := names(city_codes)[i]]
  }

  return(data)
}


get_city_by_fleet <- function(dt){
  source("utils/constants/fleets.R", local=T)

  for (city_name in names(active_fleets)){
    dt[fleet %in% active_fleets[[city_name]], city := city_name]
  }

  return(dt$city)
}


set_daysoff <- function(tasks, value = NA){
  source("utils/constants/fleets.R", local = T)
  source("utils/constants/days_off.R", local = T)

  set_daysoff_city <- function(tasks, city, fleets){
    days_off <- days_off[[city]]
    for (day_off in days_off){
      day_off_vec <- unlist(strsplit(day_off, " "))
      tasks <- tasks[ day_of_week ==  day_off_vec[1] & shift == day_off_vec[2],
                      (fleets) := lapply(.SD, function(x) value),
                      .SDcols=fleets]
    }

    holidays <- holidays[[city]]
    for (holiday in holidays){
      holiday_vec <- unlist(strsplit(holiday, " "))
      if (is.na(holiday_vec[2])){
        tasks <- tasks[ date == holiday_vec[1],
                        (fleets) := lapply(.SD, function(x) value),
                        .SDcols=fleets]
      }else{
        tasks <- tasks[ date == holiday_vec[1] & shift == holiday_vec[2],
                        (fleets) := lapply(.SD, function(x) value),
                        .SDcols=fleets]
      }
    }
    return(tasks)
  }

  tasks[, day_of_week := weekdays(date)]

  for (i in 1:length(active_fleets)){
    tasks <- set_daysoff_city(tasks, names(active_fleets)[i], active_fleets[[i]])
  }

  return(tasks[, -c("day_of_week")])
}


FormatDate <- function(x, start.date, end.date){
  dates <- seq(start.date, end.date, by = "day")
  dates.dt <- data.table(
    "date" = rep(dates, each = 2),
    "shift" = rep(c(morning_shift, evening_shift), times = length(dates))
  )

  x[, date := as.Date(date)]
  x <- merge(dates.dt, x, all.x = T, by = c("date", "shift"))

  return(x)
}


FormatDateMonthYear <- function(x, start.date, end.date){
  library(zoo)

  dates <- seq(start.date, end.date, by = "day")
  dates.dt <- data.table(
    "date" = rep(dates, each = 2),
    "shift" = rep(c(morning_shift, evening_shift), times = length(dates))
  )

  x[, date := as.Date(date)]

  dates.dt[, month_year := as.yearmon(date)]
  x[, month_year := as.yearmon(date)]

  x <- merge(dates.dt, x[, -c("date"), with = F],
             all.x = T, by = c("month_year"))
  x <- x[, -c("month_year"), with = F]

  return(x)
}


CalcGrowthReport <- function(start.date = Sys.Date() - 7,
                             end.date = Sys.Date() - 1){
  library(data.table)

  fi <- "/home/bi_user/powerbi-share/R_outputs/intwash_orders_marketing.csv"
  non.market.channels <- c("CC", "Refer a Friend", "Website", "Direct", "Internal")

  if (is.character(start.date)){
    start.date <- as.Date(start.date)
    end.date <- as.Date(end.date)
  }

  data <- fread(fi)

  data <- data[dropOffTimeslot_from >= start.date & dropOffTimeslot_from <= end.date]
  data <- data[!duplicated(reference)]

  print(paste("external revenue", sum(data$grossBeforeVoucher_eur, na.rm = T)))
  print(paste("voucher value", sum(data$vouchervalue, na.rm = T)))
  print(paste("all orders", nrow(data)))
  print(paste("new orders", sum(data$newCustomer)))
  print(paste("orders marketing",
              nrow(data[!final_lc_channels %in% non.market.channels])))
}


GetStartOfWeek <- function(date){
  week.start = date - ( setNames(c(6, 0:5), 0:6) [strftime(date,'%w')] )
  names(week.start) = NULL

  return(week.start)
}
