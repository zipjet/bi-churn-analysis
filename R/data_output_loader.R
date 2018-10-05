GetRatings <- function(churn.data){
  ratings <- fread(file="~/powerbi-share/R_outputs/ratings.csv")
  ratings.cols.old <- names(ratings)[grepl("topics", names(ratings))]
  ratings.cols.new <- unlist(lapply(ratings.cols.old, 
                                    function(x) gsub("topics", "rating", x)))
  setnames(ratings, ratings.cols.old, ratings.cols.new)
  ratings.cols <- append(ratings.cols.new, c("order_ref", "rating"), 0)

  churn.data <- merge(churn.data, ratings[, ..ratings.cols], 
                     all.x = TRUE, by.x = "order_id", by.y = "order_ref")
  
  return(churn.data)
}

GetFacility <- function(churn.data){
  
  recleans <- fread("data/input/orders.csv")
  recleans.cols <- c("order_id", "fac_name", "reclean_order")
  recleans <- recleans[, reclean_order := as.logical(max(as.numeric(
    reclean_order))), by = order_id]
  recleans <- recleans[!duplicated(order_id)]
  churn.data <- merge(churn.data, recleans[, recleans.cols, with = F],
                     all.x = TRUE, by = "order_id")
  
  tuxedo_aliases <- c("South", "Tuxedo Express", "Corporate", "NE Hub")
  churn.data[fac_name %in% tuxedo_aliases, fac_name := "Tuxedo"]
  
  return(churn.data)
}

GetReschedules <- function(churn.data){
 
  reschedules <- fread("data/input/reschedules.csv")
  reschedules <- reschedules[, c("order_id", "initiator")]
  reschedules <- dcast(reschedules, `order_id` ~ initiator)
  names(reschedules) <- c("order_id", "num_customer_reschedules", "num_internal_reschedules")
  
  reschedules[num_customer_reschedules >= 1, customer_rescheduled := T]
  reschedules[num_customer_reschedules == 0, customer_rescheduled := F]
  reschedules[num_internal_reschedules >= 1, internal_rescheduled := T]
  reschedules[num_internal_reschedules == 0, internal_rescheduled := F]
  
  churn.data <- merge(churn.data, reschedules, all.x = TRUE, by = "order_id")
  
  return(churn.data)
}

GetPunctuality <- function(churn.data){
  punctuality <- fread("data/input/punctuality.csv")
  punctuality <- punctuality[, c("order_id", "driver_db_id_DO", "driver_db_id_PU", 
                                 "delay_mins_PU", "delay_mins_DO", "punctual_5min_DO", "punctual_5min_PU", "punctual_order")]
  
  churn.data <- merge(churn.data, punctuality, by = "order_id", all.x = TRUE)
  return(churn.data)
}

GetVouchers <- function(churn.data) {
  vouchers <- fread("~/Projects/bi-reporting/etl/marketing/csv/marketing_dataset.csv")
  vouchers <- vouchers[, c("order_id", "voucher_value")]
  churn.data <- merge(churn.data, vouchers, by = "order_id", all.x = TRUE)
  
  churn.data$voucher_used = F
  churn.data[!is.na(voucher_channel), voucher_used := T]
  
  churn.data[voucher_used == T, 
             voucher_revenue_ratio := voucher_value /(voucher_value + revenue)]
  churn.data[is.na(voucher_revenue_ratio), voucher_revenue_ratio := 0]
  
  return(churn.data)
}

GetRefunds <- function(churn.data) {
  
  refunds <- fread("data/input/refunds.csv")
  churn.data <- merge(churn.data, refunds, by = "order_id", all.x = TRUE)
  
  return(churn.data)
}


GetCustomerData <- function(churn.data) {
  
  cust.data <- fread("data/input/customers.csv")
  churn.data <- merge(churn.data, cust.data, all.x = TRUE, by="customer_db_id")
  
  return(churn.data)
}

GetBaskets <- function(churn.data) {
  product.types <- c("LA", "DC", "HH", "WF")
  churn.data$product_combinations = ""
  
  for (product.type in product.types){
    col.name <- paste0("product_", product.type)
    churn.data[, (col.name) := FALSE]
    churn.data[grepl(product.type, product_type), (col.name) := TRUE]
    churn.data[grepl(product.type, product_type), product_combinations := 
                 paste0(product_combinations, product.type)]
  }

  return(churn.data)
}


GetCity <- function(churn.data){
  return(get_city(churn.data, "order_id"))
}

GetDistanceFromHub <- function(churn.data){
  
  library(geosphere)
  
  GetFleet <- function(churn.data){
    source("utils/assign_clusters.R")
    
    col.names.old <- c("order_x", "order_y", "order_id")
    col.names.new <- c("x", "y", "id")
    setnames(churn.data, col.names.old, col.names.new)
    
    churn.data <- assign_cluster(churn.data)
    churn.data <- churn.data[!duplicated(churn.data)]
    setnames(churn.data, c("coords.x1", "coords.x2", "id"), col.names.old)
    
    return(churn.data)
  }
  
  GetHubCoords <- function(churn.data){
    source("utils/constants/fleets.R")
    
    hubs <- fread("data/input/hub_locations.csv")
    for(i in 1:length(fleet.hubs)){
      hub <- names(fleet.hubs)[i]
      coords <- hubs[fac_name == hub, c("hub_x", "hub_y")]
      churn.data[fleet == fleet.hubs[i], hub_x := coords$hub_x]
      churn.data[fleet == fleet.hubs[i], hub_y := coords$hub_y]
    }
    
    return(churn.data)
  }
  
  churn.data <- GetFleet(churn.data)
  churn.data <- GetHubCoords(churn.data)
  
  churn.data$hub_distance <- distHaversine(
    cbind(churn.data$order_y, churn.data$order_x), 
    cbind(churn.data$hub_y, churn.data$hub_x))
  
  return(churn.data)
}

GetClusters <- function(churn.data){
  clusters <- fread("data/input/clustered_orders.csv")
  churn.data <- merge(churn.data, clusters, by="order_id", all.x=T)
  
  return(churn.data)
}

CalcChurnFactor <- function(churn.data){
  # Computes the customer churn factor: churn_factor = recency/frequency
  # where recency is the time in days since last order and frequency is the 
  # average time in days between orders
  
  churn.data[is.na(frequency), frequency := 0]
  churn.data[, order_num := seq_len(.N), keyby = customer_db_id]
  churn.data[, last_order := max(as.Date(order_created_datetime)), 
             by = customer_db_id]
  churn.data[, recency := as.integer(Sys.Date())-as.integer(as.Date(last_order)), 
             by = customer_db_id]
  churn.data[frequency != 0, churn_factor := recency/frequency, 
             by = customer_db_id]
  churn.data[frequency == 0, churn_factor := recency/60]
  
  churn.data[, days_since_last_order := 
               as.integer(as.Date(order_created_datetime)) - 
               shift(as.integer(as.Date(order_created_datetime))), by = customer_db_id]   
  
  churn.data[, days_until_next_order := 
               shift(as.integer(as.Date(order_created_datetime)), type = "lead")
             - as.integer(as.Date(order_created_datetime)), 
               by = customer_db_id]   
  
  return(churn.data)
}

LoadData <- function(refresh = F){
  
  library(data.table)
  library(jsonlite)
  library(ggplot2)
  
  
  source("utils/utils.R")
  
  out.file <- "data/order_churn_data.csv"
  
  data <- fread(file="~/powerbi-share/R_outputs/customer_analysis.csv")
  data <- as.data.table(data)
  data <- data[!duplicated(data)]
  churn.data <- data[order(customer_db_id, order_created_datetime)]

  if (refresh) {
    print("Refreshing data...")
    source("R/data_input_loader.R")
    LoadInputData()
  }
  
  print("Loading data...")
  transformations <- c(CalcChurnFactor, GetFacility, GetVouchers,
                       GetRatings, GetReschedules, GetPunctuality, GetRefunds, 
                       GetBaskets, GetCustomerData, GetCity, GetClusters)
  res <- lapply(transformations, function(f) churn.data <<- f(churn.data))



  churn.data <- churn.data[!duplicated(churn.data)]
  write.csv(churn.data, out.file, row.names = F, fileEncoding = "utf-8")
}


