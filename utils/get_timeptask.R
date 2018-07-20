get_timeptask_actual <- function(start_date, end_date = start_date + 1){
  library(mongolite)
  library(jsonlite)
  library(data.table)
  
  source("../utils/utils.R", local = T)
  source("../utils/assign_clusters.R", local = T)
  
  
  my_fields <- c("_id", "action._", "action.taskId", "action.date.date",
                 "ack.task.address.latLng.lat", "ack.task.address.latLng.lng")
  
  actions <- c("DriverApi.Action.OnTheWay", "DriverApi.Action.ArrivedAt",
               "DriverApi.Action.CompleteInteraction",
               "DriverApi.Action.FailedOnSite")
  
  rename_from <- c("date.DriverApi.Action.CompleteInteraction",
                   "date.DriverApi.Action.ArrivedAt",
                   "date.DriverApi.Action.OnTheWay",
                   "date.DriverApi.Action.FailedOnSite")
  rename_to <- c("complete_inter", "arrived_at", "on_the_way", "failed_inter")

  task_type_abbr <- c("PU", "DO", "COR", "FAC")
  task_type_full <- c("PICKUP", "DROPOFF", "CORPORATE", "FACILITY")
  
  final_feats <- c("id", "date", "travel_time", "inter_time", "task_type", "shift",
                   "fleet")
  
  mongo_handle <- get_mongo_handle("da_action_results")
  tasks <- mongo_handle$find(
    query = get_time_range_query("createdAt", start_date, end_date),
    fields = get_fields_query(my_fields)
  )
  tasks <- data.table(flatten(tasks))
  

  if (nrow(tasks) == 0){
    return(tasks)
  }
  
  tasks <- tasks[`action._` %in% actions]
  tasks <- tasks[, -c("_id")]
  setnames(tasks, names(tasks), c("id", "type", "date", "x", "y")) 
  tasks <- get_city(tasks)
  tasks <- assign_cluster(tasks)
  tasks <- set_timezone(tasks)
  
  tasks <- reshape(tasks, v.names = c("date"),
                   idvar = c("id", "city", "fleet"),
                   timevar ="type", direction = "wide")
  setnames(tasks, rename_from, rename_to)
  
  tasks[is.na(complete_inter), complete_inter := failed_inter]
  tasks[, travel_time := as.numeric(difftime(arrived_at, on_the_way,
                                             units = "mins"))]
  tasks[, inter_time := as.numeric(difftime(complete_inter, arrived_at,
                                            units = "mins"))]
  
  message(paste("Number of tasks with NA travel and inter time:", 
                nrow(tasks[(is.na(travel_time) & is.na(inter_time))])))
  tasks <- tasks[! (is.na(travel_time) & is.na(inter_time))]
  
  tasks[, date := on_the_way]
  tasks[is.na(date), date := arrived_at]

  tasks$shift <- get_shift(tasks$date, tasks$city)
  
  for (i in 1:length(task_type_abbr)){
    tasks[grepl(task_type_abbr[i], id), task_type := task_type_full[i]]
  }
  
  tasks <- tasks[order(date)]
  tasks <- tasks[!duplicated(id)]
  
  return (tasks[, final_feats, with = F])
}


get_timeptask_pred <- function(start_date, end_date = start_date + 1){
  library(mongolite)
  library(jsonlite)
  library(data.table)
  
  source("../utils/utils.R", local = T)
  source("../utils/assign_clusters.R", local = T)
  
  my_fields <- c("_id", "destination.externalId",
                 "destination.serviceSlot.from.date", "destination.latLng.lat",
                 "destination.latLng.lng", "destination.type",
                 "forecast.travelingTime", 
                 "destination.serviceSlot.serviceDurationInSecs")
  
  rename_from <- c("forecast.travelingTime",
                   "destination.serviceSlot.serviceDurationInSecs",
                   "destination.type", "destination.latLng.lat",
                   "destination.latLng.lng", "destination.externalId",
                   "destination.serviceSlot.from.date")
  rename_to <- c("travel_time", "inter_time", "task_type", "x", "y", "id",
                 "date")
  
  final_feats <- c("id", "date", "travel_time", "inter_time", "task_type",
                   "shift", "fleet")
  
  mongo_handle <- get_mongo_handle("bi_task_predictions")
  tasks <- mongo_handle$find(
    query = get_time_range_query("start.serviceSlot.from.date",
                                 start_date, end_date),
    fields = get_fields_query(my_fields)
  )
  
  if (nrow(tasks) == 0) {
    return (tasks)
  }
  
  tasks <- data.table(flatten(tasks))
  tasks <- tasks[order(-`_id`)]
  tasks <- tasks[!duplicated(destination.externalId)]
  tasks <- tasks[, -c("_id")]
  setnames(tasks, rename_from, rename_to)
  
  tasks <- get_city(tasks)
  tasks <- assign_cluster(tasks)
  
  tasks <- set_timezone(tasks)
  tasks$shift <- get_shift(tasks$date, tasks$city)
  
  tasks[, inter_time := inter_time /60]
  tasks[, travel_time := travel_time /60]

  return(tasks[, final_feats, with = F])
}


get_timeptask <- function(start_date, end_date = start_date + 1){
  tasks_actual <- get_timeptask_actual(start_date, end_date)
  tasks_pred <- get_timeptask_pred(start_date, end_date)
  
  if (nrow(tasks_actual) != 0 | nrow(tasks_pred) != 0){
    join_ids <- intersect(tasks_actual$id, tasks_pred$id)
    tasks_actual_join <- tasks_actual[id %in% join_ids]
    tasks_pred_join <- tasks_pred[id %in% join_ids]
    
    tasks_actual_join <- tasks_actual_join[order(id)]
    tasks_pred_join <- tasks_pred_join[order(id)]
    
    if (!identical(tasks_actual_join$fleet, tasks_pred_join$fleet)){
      warning("Driver App and Alyx tasks have differing fleet assignments!")
    }  
  }
  
  return(list("tasks_pred" = tasks_pred, "tasks_actual" = tasks_actual))
}


get_timepshift <- function(start_date, end_date = start_date + 1, q = 0.5, in.hours = F){
  kFacTask <- "FACILITY"
  time.col.names <- c("timeptask", "timeptask", "time_to_fac")
  
  tasks <- get_timeptask(start_date, end_date)
  
  if (nrow(tasks$tasks_actual) == 0 | nrow(tasks$tasks_pred) == 0){
    timepshift <- list(
      "timeptask_pred" = data.table(),
      "timeptask_actual" = data.table(),
      "time_to_fac_pred" = data.table()
    )
  }else{
    tasks <- lapply(tasks, function(x) x[, total_time := travel_time + inter_time])
    
    timepshift <- list(
      "timeptask_pred" = tasks$tasks_pred[task_type != kFacTask, 
                                          quantile(total_time, prob = q,
                                                   na.rm = T),
                                          by = .(shift, fleet)],
      "timeptask_actual" = tasks$tasks_actual[ , quantile(total_time, prob = q,
                                                          na.rm = T),
                                               by = .(shift, fleet)],
      "time_to_fac_pred" = tasks$tasks_pred[task_type == kFacTask, 
                                            quantile(travel_time, prob = q,
                                                     na.rm = T),
                                            by = .(shift, fleet)]
    )
    
    if (in.hours){
      timepshift <- lapply(timepshift, function(x) x[, V1 := V1 / 60])
    }
    timepshift <- Map(function(x, rename.to) setnames(x, "V1", rename.to),
                      timepshift, time.col.names)  
  }
  
  return(timepshift)
}