
MergeToCustomers <- function(customers, to.merge, col.name.old, col.name.new, 
                             by.x="customer_db_id", by.y="customer_db_id"){
  cust.merged <- merge(customers, to.merge, all.x = T, by.x = by.x, by.y = by.y)
  setnames(cust.merged, col.name.old, col.name.new)
  
  return(cust.merged)
}


CalcOrderCounts <- function(customers, orders, orders.last){
  
  CountOrders <- function(customers, orders, order_states, col_name){
    orders.states <- orders[order_state %in% order_states, .N , by=customer_db_id]
    customers <- MergeToCustomers(customers, orders.states, "N", col_name)
    customers[is.na(customers[[col_name]]), (col_name) := 0]
    
    return(customers)
  }
  
  customers <- CountOrders(customers, orders, c("processing", "reserved"), "open_orders")
  customers <- CountOrders(customers, orders, c("canceled"), "canceled_orders")
  customers <- CountOrders(customers, orders, c("payment_error"), "pay_error_orders")
  customers <- CountOrders(customers, orders, c("completed"), "completed_orders")
  customers[, total_orders :=  completed_orders + open_orders + canceled_orders + pay_error_orders]
  customers <- MergeToCustomers(customers, orders.last[, c("customer_db_id", "order_state")], 
                                "order_state", "last_order_state")
  
  return(customers)
}                    


CalcOrderDates <- function(customers, orders, orders.last, orders.first) {
  customers <- MergeToCustomers(customers, orders.last[, c("customer_db_id", "order_created_datetime")],
                                "order_created_datetime", "last_order_date")
  customers <- MergeToCustomers(customers, orders.first[, c("customer_db_id", "order_created_datetime")],
                                "order_created_datetime", "first_order_date")
  
  orders.second <- orders[order_state == "completed"]
  orders.second <- orders.second[order(order_created_datetime), .SD[2], by = customer_db_id]
  customers <- MergeToCustomers(customers, orders.second[, c("customer_db_id", "order_created_datetime")],
                                "order_created_datetime", "second_order_date")
  customers[, first_order_recency := as.Date(second_order_date) - as.Date(first_order_date)]
  return(customers)
}

CalcServiceClass <- function(customers, orders, orders.last, orders.first){
  cust.class <- orders[, .N, by = c("customer_db_id", "service_class")]
  cust.class <- dcast(cust.class, `customer_db_id` ~ service_class, value.var = "N")
  cust.class <- data.table(cust.class)
  for (i in names(cust.class))
    cust.class[is.na(get(i)), (i) := 0]
  customers <- merge(customers, cust.class, by = "customer_db_id", all.x = TRUE)
  
  customers <- MergeToCustomers(
    customers, orders.last[, c("customer_db_id", "service_class")], 
    "service_class", "last_order_service_class")
  customers <- MergeToCustomers(
    customers, orders.first[, c("customer_db_id", "service_class")], 
    "service_class", "first_order_service_class")
  
  return(customers)
}

CalcSoftwareType <- function(customers, orders, orders.last, orders.first){
  cust.soft <- orders[, .N, by = c("customer_db_id", "software_type")]
  cust.soft <- dcast(cust.soft, `customer_db_id` ~ software_type, value.var = "N")
  cust.soft <- data.table(cust.soft)
  for (i in names(cust.soft))
    cust.soft[is.na(get(i)), (i) := 0]
  customers <- merge(customers, cust.soft, by = "customer_db_id", all.x = TRUE)
  
  customers <- MergeToCustomers(
    customers, orders.last[, c("customer_db_id", "software_type")], 
    "software_type", "last_order_software_type")
  customers <- MergeToCustomers(
    customers, orders.first[, c("customer_db_id", "software_type")], 
    "software_type", "first_order_software_type")
  
  return(customers)
}

CalcBasketSegments <- function(customers, orders, orders.first){
  product.cols <- c("product_LA", "product_HH", "product_DC", "product_WF")
  basket.segments <- orders[, lapply(.SD, sum), by = customer_db_id, 
                                .SDcols = product.cols]
  customers <- merge(customers, basket.segments, by = "customer_db_id", all.x = TRUE)
  
  customers <- MergeToCustomers(
    customers, orders.first[, c("customer_db_id", "product_combinations")],
    "product_combinations", "first_order_products")
  
  return(customers)
}

CalcVoucherUsage <- function(customers, orders, orders.last, orders.first){
  # vouchers
  vouchers.used <- orders[!is.na(voucher_channel), .N, by = customer_db_id]
  customers <- MergeToCustomers(customers, vouchers.used, "N", "vouchers_used")
  customers[is.na(vouchers_used), vouchers_used := 0]
  
  vouchers.ratio <- orders[, sum(voucher_value, na.rm = T) / (sum(
    voucher_value, na.rm = T) + sum(revenue, na.rm = T)), by = customer_db_id]
  customers <- MergeToCustomers(customers, vouchers.ratio, "V1", 
                                "vouchers_revenue_ratio")
  customers[is.na(vouchers_revenue_ratio), vouchers_revenue_ratio := 0]
  
  vouchers.first <- unique(orders.first[!is.na(voucher_channel)])
  vouchers.last <- unique(orders.last[!is.na(voucher_channel)])
  
  customers <- MergeToCustomers(
    customers, orders.first[, c("customer_db_id", "voucher_channel")], 
    "voucher_channel", "first_order_voucher_channel")
  customers <- MergeToCustomers(
    customers, orders.first[, c("customer_db_id", "voucher_value")], 
    "voucher_value", "first_order_voucher_value")
  customers <- MergeToCustomers(
    customers, orders.last[, c("customer_db_id", "voucher_channel")], 
    "voucher_channel", "last_order_voucher_channel")
  customers <- MergeToCustomers(
    customers, orders.last[, c("customer_db_id", "voucher_value")], 
    "voucher_value", "last_order_voucher_value")
  
  customers$first_order_voucher <- F
  customers[!is.na(first_order_voucher_channel), first_order_voucher := T]
  customers$last_order_voucher <- F
  customers[!is.na(last_order_voucher_channel), last_order_voucher := T]
  
  return(customers)
}

CalcRecleans <- function(customers, orders, orders.last){
  recleans <- orders[, sum(reclean_order, na.rm = T), by = customer_db_id]
  customers <- MergeToCustomers(customers, recleans, "V1", "reclean_orders")
  customers[is.na(reclean_orders), reclean_orders := 0]
  customers <- MergeToCustomers(customers, orders.last[, c("customer_db_id", "reclean_order")], 
                                "reclean_order", "last_reclean_order")
  customers[is.na(last_reclean_order), last_reclean_order := FALSE]
  customers$reclean_ratio <- 0
  customers[completed_orders > 0, reclean_ratio := reclean_orders / completed_orders]
  
  return(customers)
}

CalcReschedules <- function(customers, orders, orders.last){
  # reschedules
  reschedules <- orders[
    , .(internal_reschedules = sum(internal_reschedule, na.rm = T), 
        customer_reschedules = sum(customer_reschedule, na.rm = T)), 
    by = customer_db_id]
  customers <- merge(customers, reschedules, all.x = T, by = "customer_db_id")
  
  customers <- MergeToCustomers(customers, orders.last[, c("customer_db_id", "internal_reschedule")], 
                                "internal_reschedule", "last_order_int_reschedules")
  customers[last_order_int_reschedules > 1, last_order_rescheduled := T]
  customers[last_order_int_reschedules == 0, last_order_rescheduled := F]
  
  
  return(customers)
}

CalcRatings <- function(customers, orders, orders.last, orders.first){

  ratings <- orders[!is.na(rating), 
                    .(rated_orders = .N, avg_rating = mean(rating)), 
                    by = customer_db_id]

  customers <- merge(customers, ratings, all.x = T, by = "customer_db_id")
  customers[is.na(rated_orders), rated_orders := 0]
  customers[, rated_orders_ratio := rated_orders / (completed_orders + canceled_orders)]
  customers[is.na(rated_orders_ratio), rated_orders_ratio := 0]
  
  customers <- MergeToCustomers(customers, 
                                orders.last[, c("customer_db_id", "rating")], 
                                "rating", "last_order_rating")
  customers[, rating_diff := last_order_rating - avg_rating]
  
  customers <- MergeToCustomers(customers, 
                                orders.first[, c("customer_db_id", "rating")], 
                                "rating", "first_order_rating")
  
  return(customers)
}

CalcRefunds <- function(customers, orders, orders.last){
  refunds.success <- orders[refund == "SUCCESS", .(refunds_success = .N), 
                            by = customer_db_id]
  customers <- merge(customers, refunds.success, all.x = T, by = "customer_db_id")
  refunds.unsuccess <- orders[refund == "NO SUCCESS", .(refunds_unsuccess = .N),
                              by = customer_db_id]
  customers <- merge(customers, refunds.unsuccess, all.x = T, by = "customer_db_id")
  customers <- MergeToCustomers(customers, orders.last[, c("customer_db_id", "refund")], 
                                "refund", "last_order_refund")
  customers$last_order_refund_request <- FALSE
  customers <- customers[last_order_refund %in% c("SUCCESS", "NO SUCCESS"),
                         last_order_refund_request := TRUE]
  customers <- customers[, -c("last_order_refund")]

  return(customers)  
}

CalcPickUps <- function(customers, orders){
  
  CalcPickUpRatio <- function(pickups, col.name){
    cols <- c("customer_db_id", col.name)
    pickup.ratio <- pickups[, .(num_pickups = .N), by = mget(cols)]
    pickup.ratio[, total_pickups := sum(num_pickups), by = customer_db_id]
    pickup.ratio[, pickup_ratio := num_pickups / total_pickups]
    dcast.form <- as.formula(paste0("customer_db_id~", col.name))
    pickup.ratio <- dcast(pickup.ratio, dcast.form, value.var = "pickup_ratio", 
                          fill = 0)
    
    return(pickup.ratio)
  }
  
  pickups <- orders[, c("customer_db_id", "pickup_timeslot_from_datetime")]
  pickups$weekday <- weekdays.Date(as.Date(pickups$pickup_timeslot_from_datetime))
  pickups$hour <- substr(pickups$pickup_timeslot_from_datetime, 12, 13)
  pickups$timeofday <- "early_morning"
  pickups[hour >= 10 & hour <= 14, timeofday := "before_noon"]
  pickups[hour > 14 & hour <= 18, timeofday := "after_noon"]
  pickups[hour > 18, timeofday := "evening"]
  
  pickup.days <- CalcPickUpRatio(pickups, "weekday")
  pickup.hours <- CalcPickUpRatio(pickups, "timeofday")
  pickups <- merge(pickup.days, pickup.hours, all = T, by = "customer_db_id")
  
  pickup.names.old <- names(pickups[, -c("customer_db_id")])
  pickup.names.new <- unlist(lapply(pickup.names.old, 
                                    function(x) paste0("pickup_", tolower(x))))
  setnames(pickups, pickup.names.old, pickup.names.new)
  
  customers <- merge(customers, pickups, all.x = T, by = "customer_db_id")

  return(customers)
}

CalcZip <- function(customers, orders, orders.last){
  # group zips into zip areas based on http://www.londontown.com/LondonPC
  customers <- MergeToCustomers(customers, 
                                orders.last[,c("customer_db_id", "pickup_zip")],
                                "pickup_zip", "zip")
  customers$zip_area <- unlist(lapply(customers$zip, function(x) 
    tolower(trimws(substr(x, 1, (regexpr("\\d", x)[1]) - 1)))))
  
  return(customers)
}

CalcFacility <- function(customers, orders, orders.last, orders.first) {
  customers <- MergeToCustomers(customers, orders.last[, c("customer_db_id", "fac_name")],
                                "fac_name", "last_order_fac_name")
  customers <- MergeToCustomers(customers, orders.first[, c("customer_db_id", "fac_name")],
                                "fac_name", "first_order_fac_name")
  
  return(customers)
}

CalcClosestLaundry <- function(customers, orders.last){
  
  library(geosphere)
  
  laundries = fread("data/laundries.csv")
  
  GetClosestLaundry <- function(r){
    if(is.na(r["order_x"])){
      return(data.frame())
    } else {
      dist.mat = distm(cbind(as.numeric(r["order_y"]), 
                             as.numeric(r["order_x"])), 
                       cbind(laundries$lng, 
                             laundries$lat))
      dist = min(dist.mat)
      idx = which.min(dist.mat)
      rating = laundries[idx, "rating"][[1]]
      within_1km = sum(dist.mat <= 1000)
      
      return(
        data.frame(laundry_distance = dist,
                   laundry_rating = rating,
                   laundry_within_1km = within_1km))
    }
  }
  
  orders.coords <- orders.last[, c("customer_db_id", "order_x", "order_y")]
  orders.coords$laundry <- apply(orders.coords, 1, GetClosestLaundry)
  orders.coords <- unnest(orders.coords, laundry)
  
  orders.coords <- orders.coords[, -c("order_x", "order_y")]
  customers <- merge(customers, orders.coords,
                    by = "customer_db_id", all.x = T)
  
  return(customers)
}

CalcDistances <- function(customers, orders, orders.last, orders.first){

  avg.dist <- orders[, mean(hub_distance, na.rm = T), by = customer_db_id]
  customers <- MergeToCustomers(customers, avg.dist, "V1", "avg_hub_distance")
  
  last.dist <- orders.last[, c("customer_db_id", "hub_distance")]
  customers <- MergeToCustomers(customers, last.dist, 
                                "hub_distance", "last_order_hub_distance")
  
  first.dist <- orders.first[, c("customer_db_id", "hub_distance")]
  customers <- MergeToCustomers(customers, first.dist, 
                                "hub_distance", "first_order_hub_distance")
  
  return(customers)
}

CalcRevenue <- function(customers, orders.last, orders.first) {
  customers <- MergeToCustomers(customers, 
                                orders.first[, c("customer_db_id", "revenue")],
                                "revenue", "first_order_revenue")
  customers <- MergeToCustomers(customers, 
                                orders.last[, c("customer_db_id", "revenue")],
                                "revenue", "last_order_revenue")
  
  customers[, last_order_revenue_diff := (last_order_revenue - aov)/aov]
  
  return(customers)
}

CalcPunctuality <- function(customers, orders, orders.last){
  punct <- orders[punctual == FALSE, .(unpunctual_orders = .N), by = customer_db_id]
  punct.late <- orders[punctual_mins_DO > 0 | punctual_mins_PU > 0,
                       .(late_orders = .N), by = customer_db_id]
  punct.early <- orders[punctual_mins_DO < 0 | punctual_mins_PU < 0,
                       .(early_orders = .N), by = customer_db_id]
  
  customers <- merge(customers, punct.late, by = "customer_db_id", all.x = T)
  customers <- merge(customers, punct.early, by = "customer_db_id", all.x = T)
  customers <- merge(customers, punct, all.x = T, by = "customer_db_id")
  
  customers[unpunctual_orders > 0 & completed_orders > 0,
            unpunctual_ratio := unpunctual_orders / completed_orders]
  
  punct.last <- orders.last[, c("customer_db_id", "punctual", 
                                "punctual_mins_ratio_DO", "punctual_mins_ratio_PU")]
  customers <- merge(customers, punct.last, by = "customer_db_id", all.x = T)
  
  
  return(customers)
}

CalcClusters <- function(customers, orders){
  
  clusters <- orders[!is.na(cluster), c("customer_db_id", "cluster")]
  clusters <- dcast(clusters, `customer_db_id` ~ cluster)
  
  cluster.cols <- names(clusters)[2:length(names(clusters))]
  cluster.cols <- unlist(lapply(cluster.cols, function(x) paste0('cluster_', x)))
  names(clusters) <-  c("customer_db_id", cluster.cols)
  
  clusters <- cbind(clusters[, "customer_db_id"],
                    prop.table(data.matrix(clusters[, ..cluster.cols]), margin=1))
  
  customers <- merge(customers, clusters, by="customer_db_id", all.x = T)
  
  # first order cluster
  orders.first.completed <- orders[order_state == "completed" 
                                   & order(order_created_datetime), 
                                   .SD[1], by = customer_db_id]
  customers <- MergeToCustomers(customers, 
                                orders.first.completed[, c("customer_db_id", "cluster")],
                                "cluster",
                                "first_order_cluster")
  
}

TransformData <- function(){

  library(data.table)
  library(tidyr)
  
  source("utils/utils.R")
  
  
  orders <- fread("data/order_churn_data.csv")
  constant.cols <- c("customer_db_id", "customer_id", "gender", "segment", 
                     "aov", "recency", "frequency", "churn_factor", "referred", 
                     "newsletter_optin", "city")
  customers <- orders[, constant.cols, with = F]
  customers <- customers[!duplicated(customers)]
  
  orders.last <- orders[order(order_created_datetime, decreasing = T), 
                            .SD[1], by = customer_db_id]
  orders.first <- orders[order(order_created_datetime), .SD[1], by = customer_db_id]
  
  # BEHAVIOURAL
  customers <- CalcOrderCounts(customers, orders, orders.last)
  customers <- CalcOrderDates(customers, orders, orders.last, orders.first)
  customers <- CalcServiceClass(customers, orders, orders.last, orders.first)
  customers <- CalcSoftwareType(customers, orders, orders.last, orders.first)
  customers <- CalcBasketSegments(customers, orders, orders.first)
  customers <- CalcVoucherUsage(customers, orders, orders.last, orders.first)
  customers <- CalcRevenue(customers, orders.last, orders.first)
  customers <- CalcClusters(customers, orders)
  
  # EXPERIENCE
  customers <- CalcRecleans(customers, orders, orders.last)
  customers <- CalcReschedules(customers, orders, orders.last)
  customers <- CalcRatings(customers, orders, orders.last, orders.first)
  customers <- CalcRefunds(customers, orders, orders.last)
  
  ## OPERATIONAL
  customers <- CalcPickUps(customers, orders)
  customers <- CalcPunctuality(customers, orders, orders.last)
  customers <- CalcFacility(customers, orders, orders.last, orders.first)
  
  # INDIVIDUAL
  customers <- CalcZip(customers, orders, orders.last)
  customers <- CalcClosestLaundry(customers, orders.last)
  # customers <- CalcDistances(customers, orders, orders.last, orders.first)
  
  write.csv(customers, "data/churn_dataset.csv", row.names = F,
            fileEncoding = "utf-8")
}

