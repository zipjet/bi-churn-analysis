LoadRefunds <- function(){
  refunds.fields <- c("reference", "type", "state", "createdAt", "booking.typeOfRefund",
                      "booking.liabilityOfRefund")
  refunds.query <- '{"type": {"$in": ["REFUND_MANUAL", "REFUND_CREDITS", "REFUND_ADYEN"]}}'
  refunds <- GetMongoTable("intwash_orders_subprocesses", query = refunds.query, 
                           fields = refunds.fields)
  refunds <- setnames(refunds, c("booking.typeOfRefund", "booking.liabilityOfRefund"),
                      c("refund_type", "refund_liability"))
  
  refunds$refund_request <- T
  refunds$refund_approved <- F
  refunds[state %in% c("approved", "completed"), refund_approved := T]
  refunds[, order_id := substr(refunds$reference, 0, nchar(refunds$reference)-4)]
  refunds <- refunds[order(createdAt), .SD[.N], by = order_id]
  
  refunds <- refunds[, c("order_id", "refund_liability", "refund_type", 
                         "refund_request", "refund_approved")]
  write.csv(refunds, "data/input/refunds.csv", row.names = F)
}


LLoadCustomerData <- function(){
  cust.fields <- c("reference", "friendReferral.referredCode", "newsletterOptIn",
                   "corporates", "userAccount.email")
  cust.data <- GetMongoTable("intwash_customers", "{}", cust.fields)
  cust.data[, corporates := as.character(corporates)]
  cust.data[corporates == "character(0)", corporates := NA]
  cust.data[!is.na(corporates), corporate := T ]
  cust.data[is.na(corporates), corporate := F]
  cust.data$referred <- FALSE
  cust.data[!is.na(friendReferral.referredCode), referred := TRUE]
  cust.data <- cust.data[, -c("corporates", "friendReferral.referredCode")]
  names(cust.data) <- c("customer_db_id", "newsletter_optin", "customer_id",
                        "email", "corporate", "referred")
  write.csv(cust.data, "data/input/customers.csv", row.names = F)
}


LoadFacilityLocation <- function(){
  
  fac.fields <- c("name", "reference", "address.geoLocation.coordinates")
  fac.coords <- GetMongoTable("intwash_facilities", "{}", fac.fields)
  names(fac.coords) <- c("fac_db_id", "fac_name", "facility_id", "coordinates")
  fac.coords[, fac_lng := unlist(lapply(coordinates, function(x) x[[1]]))]
  fac.coords[, fac_lat := unlist(lapply(coordinates, function(x) x[[2]]))]
  
  fac.coords <- get_city(fac.coords, id.col = "facility_id")
  fac.coords <- fac.coords[, c("fac_db_id", "fac_name", "fac_lat", "fac_lng")]
  
  write.csv(fac.coords, "data/input/facilities.csv", row.names = F)
}


LoadPunctuality <- function(){
  punct.fields <- c("timeslot.from", "timeslot.to", "driver", 
                    "rendezvousStartDate.date", "reference", "state")
  
  pickups <- GetMongoTable("intwash_driver_pickups", "{}", punct.fields)
  dropoffs <- GetMongoTable("intwash_driver_deliveries", "{}", punct.fields)
  
  pickups$task <- "PU"
  dropoffs$task <- "DO"
  punct <- rbind(pickups, dropoffs)
  
  rename.from <- c("_id", "timeslot.from", "timeslot.to", 
                   "rendezvousStartDate.date", "driver", "reference")
  rename.to <- c("task_db_id", "timeslot_from", "timeslot_to", 
                 "rendezvous_start", "driver_db_id", "order_id")
  setnames(punct, rename.from, rename.to)
  
  punct <- punct[, order_id := substr(order_id, 1, nchar(order_id) - 3)]
  
  punct <- punct[, timeslot_mins := difftime(timeslot_to, timeslot_from, units="mins")]
  punct <- punct[, early_mins := difftime(rendezvous_start, timeslot_from, units="mins")]
  punct <- punct[, late_mins := difftime(rendezvous_start, timeslot_to, units="mins")]
  
  punct[early_mins > 0, early_mins := 0]
  punct[late_mins < 0, late_mins := 0]
  punct[, delay_mins := round(early_mins + late_mins)]
  
  punct[!is.na(delay_mins), punctual_5min := F]
  punct[delay_mins < 5 & delay_mins > -5, punctual_5min := T]
  
  punct <- dcast(punct, `order_id` ~ task, value.var=c("driver_db_id", "delay_mins", "punctual_5min", "timeslot_mins"))
  punct[punctual_5min_DO & punctual_5min_PU, punctual_order := T]
  punct[!punctual_5min_DO | !punctual_5min_PU, punctual_order := F]
  
  write.csv(punct, "data/input/punctuality.csv", row.names = F)
}


LoadItems <- function(){
  library(tidyr)
  
  LoadBasketBatch <- function(start.date, end.date){
    
    time.query <- get_time_range_query("createdAt", start.date, end.date)
    baskets.batch <- GetMongoTable("intwash_external_fulfillment", time.query,
                                   c("reference", "createdAt", "itemization.items"))
    baskets.batch[, items := lapply(itemization.items, flatten)]
    baskets.batch <- unnest(baskets.batch, items)
    baskets.batch[, reference := substr(reference, 1, nchar(reference) - 2)]
    
    baskets.cols.old <- c("reference", "createdAt", "quantity", "pricePerUnit", 
                          "product.reference")
    baskets.cols.new <- c("order_id", "order_date", "quantity", "price_per_unit", "product_id")
    setnames(baskets.batch, baskets.cols.old, baskets.cols.new)
    baskets.batch <- baskets.batch[, ..baskets.cols.new]
    
    return(baskets.batch)
  }
  
  LoadBaskets <- function(start.date = "2017-01-01", end.date = Sys.Date()){
    
    baskets <- data.table()
    
    seq.dates <- seq(as.Date(start.date), Sys.Date(), by = "month")
    seq.dates <- append(seq.dates, Sys.Date())
    
    for (i in seq(1, length(seq.dates) - 1)) {
      batch <- LoadBasketBatch(seq.dates[i], seq.dates[i+1])
      batch$order_date <- as.character(batch$order_date)
      baskets <- rbind(baskets, batch)
      write.csv(baskets, "data/input/baskets.csv", row.names = F, fileEncoding = 'utf-8')
    }
  }
  
  LoadProducts <- function() {
    product.fields <- c("segmentation", "name.translations", "category", "reference")
    products <- GetMongoTable("intwash_products", "{}", product.fields)
    products <- unnest(products, name.translations)
    products.cols.old <- c("segmentation", "category", "reference", "v")
    products.cols.new <- c("segmentation", "category", "product_id", "product_name")
    setnames(products, products.cols.old, products.cols.new)
    products <- products[k == "en", ..products.cols.new]
    
    write.csv(products, "data/input/products.csv", row.names = F, fileEncoding = "utf-8")
  }
  
  LoadBaskets()
  LoadProducts()
  
  baskets <- fread("data/input/baskets.csv")
  products <-fread("data/input/products.csv")
  items <- merge(baskets, products, all.x = T, by = "product_id")
  
  product.groups <- fread("data/input/product_groups.csv")
  product.groups <- product.groups[, c("product_id", "product_type", "product_group")]
  items <- merge(items, product.groups, by = "product_id", all.x = T)
  
  write.csv(items, "data/input/items.csv", row.names = F)
}


LoadOrderFacitility <- function(){
  source("~/Projects/bi-reporting/etl/operations/orders.R")
  recleans <- CalcOrders(start.date = "2014-01-01", out.fi = "data/input/orders.csv")
}


LoadReschedules <- function(){
  source("~/Projects/bi-reporting/etl/operations/reschedules.R")
  reschedules <- CalcReschedules(start.date = "2016-04-01", out.fi = "data/input/reschedules.csv")
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
  
  write.csv(hub.locs, "data/input/hub_locations.csv")
}


LoadInputData <- function(){
  source("utils/utils.R")
  
  loaders <- list(LoadCustomerData, LoadRefunds, LoadOrderFacitility, 
                  LoadHubLocations, LoadFacilityLocation, LoadReschedules,
                  LoadPunctuality, LoadItems)
  lapply(loaders, function(f) f())
}