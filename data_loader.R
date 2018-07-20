library(data.table)
library(jsonlite)
library(ggplot2)

source("utils/utils.R")

LoadRefunds <- function(){
  refunds.fields <- c("reference", "type", "state", "createdAt")
  refunds.query <- '{"type": {"$in": ["REFUND_MANUAL", "REFUND_CREDITS", "REFUND_ADYEN"]}}'
  refunds <- GetMongoTable("intwash_orders_subprocesses", query = refunds.query, 
                           fields = refunds.fields)
  refunds$refund <- "NO SUCCESS"
  refunds[state %in% c("approved", "completed"), refund := "SUCCESS"]
  refunds[, order_id := substr(refunds$reference, 0, nchar(refunds$reference)-4)]
  refunds <- refunds[order(createdAt), .SD[.N], by = order_id]
  refunds <- refunds[, c("order_id", "refund")]
  
  write.csv(refunds, "churn_analysis/data/refunds.csv", row.names = F)
}


LoadCustomerData <- function(){
  cust.fields <- c("reference", "friendReferral.referredCode", "newsletterOptIn",
                   "corporates")
  cust.data <- GetMongoTable("intwash_customers", "{}", cust.fields)
  cust.data[, corporates := as.character(corporates)]
  cust.data[corporates == "character(0)", corporates := NA]
  cust.data[!is.na(corporates), corporate := T ]
  cust.data[is.na(corporates), corporate := F]
  cust.data$referred <- FALSE
  cust.data[!is.na(friendReferral.referredCode), referred := TRUE]
  cust.data <- cust.data[, -c("corporates", "friendReferral.referredCode")]
  names(cust.data) <- c("customer_db_id", "newsletter_optin", "customer_id",
                        "corporate", "referred")
  write.csv(cust.data, "churn_analysis/data/customers.csv", row.names = F)
}


LoadFacilityLocation <- function(){
  
  fac.fields <- c("name", "reference", "address.geoLocation.coordinates")
  fac.coords <- GetMongoTable("intwash_facilities", "{}", fac.fields)
  names(fac.coords) <- c("fac_db_id", "fac_name", "facility_id", "coordinates")
  fac.coords[, fac_lng := unlist(lapply(coordinates, function(x) x[[1]]))]
  fac.coords[, fac_lat := unlist(lapply(coordinates, function(x) x[[2]]))]
  # 
  # active.hubs <- list(
  #   "London" = c("Central - Camden Road", "York House - West", "NE Camden Road", "South"),
  #   "Berlin" = c("Zentrallager"),
  #   "Paris" = c("North hub", "South hub")
  # )
  # fac.coords <- fac.coords[facility_name %in% unlist(active.hubs, use.names = F)]
  
  fac.coords <- get_city(fac.coords, id.col = "facility_id")
  fac.coords <- fac.coords[, c("fac_db_id", "fac_name", "fac_lat", "fac_lng")]
  
  write.csv(fac.coords, "churn_analysis/data/facilities.csv", row.names = F)
}

LoadOrderFacitility <- function(){
  source("operations/orders.R")
  recleans <- CalcOrders(
    start.date = "2017-01-01", file = "churn_analysis/data/orders.csv")
}

LoadReschedules <- function(){
  source("operations/reschedules.R")
  reschedules <- CalcReschedules(
    start.date = "2016-04-01", file = "churn_analysis/data/reschedules.csv")
}

LoadHubLocations <- function(churn.data){
  hubs <- c("London" = c("South", "NE Camden Road", "York House - West", 
                         "Central - Camden Road"),
            "Paris" = c("South hub", "North hub"),
            "Berlin" = c("Zentrallager"))
  
  hub.locations <- GetMongoTable("intwash_facilities", "{}", 
                                 c("name", "reference", 
                                   "address.geoLocation.coordinates"))
  names(hub.locations) <- c("fac_db_id", "fac_name", "fac_id","fac_coordinates")
  hub.locations <- hub.locations[fac_name %in% unlist(hubs, use.names = F)]
  hub.locations <- get_city(hub.locations, "fac_id")
  hub.locations$lat <- unlist(
    lapply(hub.locations$fac_coordinates, function(x) x[[2]]))
  hub.locations$lng <- unlist(
    lapply(hub.locations$fac_coordinates, function(x) x[[1]]))
  
  hub.locations <- hub.locations[, -c("fac_coordinates")]
  
  write.csv(hub.locations, "data/hub_locations.csv")
}

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
  
  recleans <- fread("churn_analysis/data/orders.csv")
  recleans.cols <- c("order_id", "fac_name", "reclean_order")
  recleans <- recleans[, reclean_order := as.logical(max(as.numeric(
    reclean_order))), by = order_id]
  recleans <- recleans[!duplicated(order_id)]
  churn.data <- merge(churn.data, recleans[, recleans.cols, with = F],
                     all.x = TRUE, by = "order_id")
  
  facilities <- fread("churn_analysis/data/facilities.csv")
  churn.data <- merge(churn.data, facilities, by = "fac_name", all.x = T)
  
  library(geosphere)
  CalcDistanceFromFacility <- function(r){
    return(
      distm(c(as.numeric(r[['order_lng']]), as.numeric(r[['order_lat']])), 
            c(as.numeric(r[['fac_lng']]), as.numeric(r[['fac_lat']])), 
            fun = distHaversine))
  }
  churn.data$fac_dist <- apply(churn.data, 1, CalcDistanceFromFacility)
  
  return(churn.data)
}

GetReschedules <- function(churn.data){
 
  reschedules <- fread("churn_analysis/data/reschedules.csv")
  reschedules <- reschedules[, c("order_id", "initiator")]
  reschedules <- dcast(reschedules, `order_id` ~ initiator)
  names(reschedules) <- c("order_id", "customer_reschedule", "internal_reschedule")
  churn.data <- merge(churn.data, reschedules, all.x = TRUE, by = "order_id")
  
  return(churn.data)
}

GetPunctuality <- function(churn.data){
  punctuality <- fread("~/powerbi-share/R_outputs/punctuality.csv")
  
  punctuality[, order_id := substr(reference, 1, nchar(reference)-3)]
  setnames(punctuality, c("late_by_more_than_one_min", "early_by_more_than_five_min"), c("late", "early"))
  punctuality <- punctuality[, c("order_id", "task_type", "late", "early")]
  
  punctuality <- melt(punctuality, id.vars = c("order_id", "task_type"), measure.vars = c("late", "early"))
  punctuality[variable == "early", value := -value]
  punctuality <- punctuality[value != 0, -c("variable")]
  punctuality <- dcast(punctuality, `order_id` ~ task_type, value.var = "value")
  names(punctuality) <- c("order_id", "dropoff_punctuality", "pickup_punctuality")
  
  churn.data <- merge(churn.data, punctuality, by = "order_id", all.x = TRUE)
  churn.data$punctual <- TRUE
  churn.data[!is.na(dropoff_punctuality) | !is.na(pickup_punctuality), punctual := FALSE]
  churn.data <- churn.data[, -c("dropoff_punctuality", "pickup_punctuality")]
  
  return(churn.data)
}

GetVouchers <- function(churn.data) {
  vouchers <- fread("marketing/csv/marketing_dataset.csv")
  vouchers <- vouchers[, c("order_id", "voucher_value")]
  churn.data <- merge(churn.data, vouchers, by = "order_id", all.x = TRUE)
  
  return(churn.data)
}

GetRefunds <- function(churn.data) {
  
  refunds <- fread("churn_analysis/data/refunds.csv")
  churn.data <- merge(churn.data, refunds, by = "order_id", all.x = TRUE)
  # set false only for orders since refund tracking is in DB
  churn.data[order_created_datetime >= "2017-03-06" & is.na(refund), 
             "refund" := "NO REQUEST"]
  churn.data[order_created_datetime < "2017-03-06", refund := "UNKNOWN"]
  
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


GetCustomerData <- function(churn.data) {
  
  cust.data <- fread("churn_analysis/data/customers.csv")
  churn.data <- merge(churn.data, cust.data, all.x = TRUE, by="customer_db_id")
  
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
  churn.data[frequency == 0, churn_factor := 1.0]
  
  return(churn.data)
}

LoadData <- function(refresh = F){
  
  out.file <- "churn_analysis/data/orders.csv"
  
  data <- fread(file="~/powerbi-share/R_outputs/customer_analysis.csv")
  data <- as.data.table(data)
  data <- data[!duplicated(data)]
  setnames(data, c("order_x", "order_y"), c("order_lat", "order_lng"))
  churn.data <- data[order(customer_db_id, order_created_datetime)]

  if (refresh){
    loaders <- list(LoadCustomerData, LoadRefunds, LoadOrderFacitility, 
                    LoadReschedules)
    lapply(loaders, function(f) f())
  }
  
  transformations <- c(CalcChurnFactor, GetFacility, GetRatings, GetReschedules, 
                       GetPunctuality, GetVouchers, GetRefunds, GetBaskets, 
                       GetCustomerData)
  res <- lapply(transformations, function(f) churn.data <<- f(churn.data))

  churn.data <- churn.data[!duplicated(churn.data)]
  write.csv(churn.data, out.file, row.names = F, fileEncoding = "utf-8")
}


