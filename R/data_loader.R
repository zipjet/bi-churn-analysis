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
  
  write.csv(refunds, "data/refunds.csv", row.names = F)
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
  write.csv(cust.data, "data/customers.csv", row.names = F)
}


LoadFacilityLocation <- function(){
  
  fac.fields <- c("name", "reference", "address.geoLocation.coordinates")
  fac.coords <- GetMongoTable("intwash_facilities", "{}", fac.fields)
  names(fac.coords) <- c("fac_db_id", "fac_name", "facility_id", "coordinates")
  fac.coords[, fac_lng := unlist(lapply(coordinates, function(x) x[[1]]))]
  fac.coords[, fac_lat := unlist(lapply(coordinates, function(x) x[[2]]))]
  
  fac.coords <- get_city(fac.coords, id.col = "facility_id")
  fac.coords <- fac.coords[, c("fac_db_id", "fac_name", "fac_lat", "fac_lng")]
  
  write.csv(fac.coords, "data/facilities.csv", row.names = F)
}

LoadItems <- function(){
  library(tidyr)
  
  LoadBaskets <- function(){
    time.query <- get_time_range_query("createdAt", "2018-04-01")
    baskets <- GetMongoTable("intwash_external_fulfillment",
                             time.query,
                             c("search.orderRef", "itemization.items"))
    baskets[, items := lapply(itemization.items, flatten)]
    baskets <- unnest(baskets, items)
    baskets.cols.old <- c("search.orderRef", "quantity", "pricePerUnit", 
                          "product.reference")
    baskets.cols.new <- c("order_id", "quantity", "price_per_unit", "product_id")
    setnames(baskets, baskets.cols.old, baskets.cols.new)
    baskets <- baskets[, ..baskets.cols.new]
    
    write.csv(baskets, "data/baskets.csv", row.names = F)
  }
  
  LoadProducts <- function() {
    product.fields <- c("segmentation", "name.translations", "category", "reference")
    products <- GetMongoTable("intwash_products", "{}", product.fields)
    products <- unnest(products, name.translations)
    products.cols.old <- c("segmentation", "category", "reference", "v")
    products.cols.new <- c("segmentation", "category", "product_id", "product_name")
    setnames(products, products.cols.old, products.cols.new)
    products <- products[k == "en", ..products.cols.new]
    
    write.csv(products, "data/products.csv", row.names = F)
  }
  
  baskets <- fread("data/baskets.csv")
  products <- fread("data/products.csv")
  
  
  items <- merge(baskets, products, all.x = T, by = "product_id")
  write.csv(items, "data/items.csv", row.names = F)
}

LoadOrderFacitility <- function(){
  source("~/Projects/bi-reporting/etl/operations/orders.R")
  recleans <- CalcOrders(start.date = "2017-01-01", out.file = "data/orders.csv")
}

LoadReschedules <- function(){
  source("~/Projects/bi-reporting/etl/operations/reschedules.R")
  reschedules <- CalcReschedules(start.date = "2016-04-01", out.file = "data/reschedules.csv")
}

LoadHubLocations <- function(){
  source("utils/constants/fleets.R")
  hubs <- names(fleet.hubs)
  
  hub.locs <- GetMongoTable("intwash_facilities", "{}",
                            c("name", "reference", 
                              "address.geoLocation.coordinates"))
  names(hub.locs) <- c("fac_db_id", "fac_name", "fac_id","fac_coords")
  hub.locs <- hub.locs[fac_name %in% unlist(hubs, use.names = F)]
  hub.locs$hub_x <- unlist(lapply(hub.locs$fac_coords, function(x) x[[2]]))
  hub.locs$hub_y <- unlist(lapply(hub.locs$fac_coords, function(x) x[[1]]))
  
  hub.locs <- hub.locs[, -c("fac_coords")]
  
  write.csv(hub.locs, "data/hub_locations.csv")
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
  
  recleans <- fread("data/orders.csv")
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
 
  reschedules <- fread("data/reschedules.csv")
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
  vouchers <- fread("~/Projects/bi-reporting/etl/marketing/csv/marketing_dataset.csv")
  vouchers <- vouchers[, c("order_id", "voucher_value")]
  churn.data <- merge(churn.data, vouchers, by = "order_id", all.x = TRUE)
  
  return(churn.data)
}

GetRefunds <- function(churn.data) {
  
  refunds <- fread("data/refunds.csv")
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
  
  cust.data <- fread("data/customers.csv")
  churn.data <- merge(churn.data, cust.data, all.x = TRUE, by="customer_db_id")
  
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
    
    hubs <- fread("data/hub_locations.csv")
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
    loaders <- list(LoadCustomerData, LoadRefunds, LoadOrderFacitility, 
                    LoadHubLocations, LoadReschedules)
    lapply(loaders, function(f) f())
  }
  
  transformations <- c(CalcChurnFactor, GetFacility, GetRatings, GetReschedules, 
                       GetPunctuality, GetVouchers, GetRefunds, GetBaskets, 
                       GetCustomerData, GetCity, GetDistanceFromHub)
  res <- lapply(transformations, function(f) churn.data <<- f(churn.data))

  churn.data <- churn.data[!duplicated(churn.data)]
  write.csv(churn.data, out.file, row.names = F, fileEncoding = "utf-8")
}


