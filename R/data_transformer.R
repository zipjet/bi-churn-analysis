
MergeToCustomers <- function(customers, to.merge, col.name.old, col.name.new, 
                             by.x="customer_db_id", by.y="customer_db_id"){
  cust.merged <- merge(customers, to.merge, all.x = T, by.x = by.x, by.y = by.y)
  setnames(cust.merged, col.name.old, col.name.new)
  
  return(cust.merged)
}

MergeFirstAndLastOrder <- function(customers, orders) {
  merge.cols <- c("customer_id", "order_state", "order_created_datetime", 
                  "days_since_last_order", "days_until_next_order",
                  "service_class", "software_type", "product_combinations",
                  "voucher_channel", "voucher_used", "voucher_value", 
                  "voucher_revenue_ratio", "revenue", "reclean_order",
                  "customer_rescheduled", "internal_rescheduled", "rating",
                  "refund_request", "refund_approved", "refund_type", "pickup_zip", 
                  "fac_name", "punctual_order")
  
  orders.last <- orders[order(order_created_datetime, decreasing = T), 
                        .SD[1], by = customer_db_id]
  orders.first <- orders[order(order_created_datetime), .SD[1], 
                        by = customer_db_id]
  
  orders.last <- orders.last[, ..merge.cols]
  names(orders.last) <- unlist(lapply(merge.cols, function(x) paste0('last_', x)))
  customers <- merge(customers, orders.last, by.x = "customer_id", by.y = "last_customer_id", all.x = T)
  
  orders.first <- orders.first[, ..merge.cols]
  names(orders.first) <- unlist(lapply(merge.cols, function(x) paste0('first_', x)))
  customers <- merge(customers, orders.first, by.x = "customer_id", by.y = "first_customer_id", all.x = T)
  
  return(customers)
}


CalcOrderCounts <- function(customers, orders){
  
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

  return(customers)
}                    


CalcOrderDates <- function(customers, orders) {

  orders.second <- orders[order_state == "completed"]
  orders.second <- orders.second[order(order_created_datetime), .SD[2], by = customer_db_id]
  customers <- MergeToCustomers(customers, orders.second[, c("customer_db_id", "order_created_datetime")],
                                "order_created_datetime", "second_order_date")
  customers[, first_order_recency := as.Date(second_order_date) - as.Date(first_order_created_datetime)]
  return(customers)
}

CalcServiceClass <- function(customers, orders){
  cust.class <- orders[, .N, by = c("customer_db_id", "service_class")]
  cust.class <- dcast(cust.class, `customer_db_id` ~ service_class, value.var = "N")
  cust.class <- data.table(cust.class)
  for (i in names(cust.class))
    cust.class[is.na(get(i)), (i) := 0]
  customers <- merge(customers, cust.class, by = "customer_db_id", all.x = TRUE)
  
  return(customers)
}

CalcSoftwareType <- function(customers, orders){
  cust.soft <- orders[, .N, by = c("customer_db_id", "software_type")]
  cust.soft <- dcast(cust.soft, `customer_db_id` ~ software_type, value.var = "N")
  cust.soft <- data.table(cust.soft)
  for (i in names(cust.soft))
    cust.soft[is.na(get(i)), (i) := 0]
  customers <- merge(customers, cust.soft, by = "customer_db_id", all.x = TRUE)
  
  return(customers)
}

CalcBasketSegments <- function(customers, orders){
  product.cols <- c("product_LA", "product_HH", "product_DC", "product_WF")
  basket.segments <- orders[, lapply(.SD, sum), by = customer_db_id, 
                                .SDcols = product.cols]
  customers <- merge(customers, basket.segments, by = "customer_db_id", all.x = TRUE)
  
  return(customers)
}

CalcVoucherUsage <- function(customers, orders){
  # vouchers
  vouchers.used <- orders[voucher_used == T, .N, by = customer_db_id]
  customers <- MergeToCustomers(customers, vouchers.used, "N", "vouchers_used")
  customers[is.na(vouchers_used), vouchers_used := 0]
  
  vouchers.ratio <- orders[, sum(voucher_value, na.rm = T) / (sum(
    voucher_value, na.rm = T) + sum(revenue, na.rm = T)), by = customer_db_id]
  customers <- MergeToCustomers(customers, vouchers.ratio, "V1", 
                                "vouchers_revenue_ratio")
  customers[is.na(vouchers_revenue_ratio), vouchers_revenue_ratio := 0]
  
  return(customers)
}

CalcRecleans <- function(customers, orders){
  recleans <- orders[, sum(reclean_order, na.rm = T), by = customer_db_id]
  customers <- MergeToCustomers(customers, recleans, "V1", "reclean_orders")
  customers[is.na(reclean_orders), reclean_orders := 0]
  

  customers$reclean_ratio <- 0
  customers[completed_orders > 0, reclean_ratio := reclean_orders / completed_orders]

  return(customers)
}

CalcReschedules <- function(customers, orders){
  # reschedules
  reschedules <- orders[
    , .(internal_reschedules = sum(num_internal_reschedules, na.rm = T), 
        customer_reschedules = sum(num_customer_reschedules, na.rm = T)), 
    by = customer_db_id]
  customers <- merge(customers, reschedules, all.x = T, by = "customer_db_id")
  
  return(customers)
}

CalcRatings <- function(customers, orders){

  ratings <- orders[!is.na(rating), 
                    .(rated_orders = .N, avg_rating = mean(rating)), 
                    by = customer_db_id]

  customers <- merge(customers, ratings, all.x = T, by = "customer_db_id")
  customers[is.na(rated_orders), rated_orders := 0]
  customers[, rated_orders_ratio := rated_orders / (completed_orders + canceled_orders)]
  customers[is.na(rated_orders_ratio), rated_orders_ratio := 0]
  customers[, rating_diff := last_rating - avg_rating]
  
  return(customers)
}

CalcRefunds <- function(customers, orders){
  refunds.success <- orders[refund_approved == T, .(refund_approved = .N), 
                            by = customer_db_id]
  customers <- merge(customers, refunds.success, all.x = T, by = "customer_db_id")
  refunds.unsuccess <- orders[refund_approved == F, .(refund_not_approved = .N),
                              by = customer_db_id]
  customers <- merge(customers, refunds.unsuccess, all.x = T, by = "customer_db_id")
  refunds.request <- orders[refund_request == T, .(refund_requests = .N),
                              by = customer_db_id]
  customers <- merge(customers, refunds.request, all.x = T, by = "customer_db_id")
  
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

CalcClosestLaundry <- function(customers, orders){
  
  library(geosphere)
  
  laundries = fread("data/input/laundries.csv")
  
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
  
  orders.last <- orders[order(order_created_datetime, decreasing = T), 
                        .SD[1], by = customer_db_id]
  
  orders.coords <- orders.last[, c("customer_db_id", "order_x", "order_y")]
  orders.coords$laundry <- apply(orders.coords, 1, GetClosestLaundry)
  orders.coords <- unnest(orders.coords, laundry)
  
  orders.coords <- orders.coords[, -c("order_x", "order_y")]
  customers <- merge(customers, orders.coords,
                    by = "customer_db_id", all.x = T)
  
  return(customers)
}

CalcRevenue <- function(customers, orders) {
  customers[, last_order_revenue_diff := (last_revenue - aov)/aov]
  
  return(customers)
}

CalcPunctuality <- function(customers, orders){
  punct <- orders[punctual_order == FALSE, .(unpunctual_orders = .N), by = customer_db_id]
  punct.late <- orders[delay_mins_DO > 0 | delay_mins_PU > 0,
                       .(late_orders = .N), by = customer_db_id]
  punct.early <- orders[delay_mins_DO < 0 | delay_mins_PU < 0,
                       .(early_orders = .N), by = customer_db_id]
  
  customers <- merge(customers, punct.late, by = "customer_db_id", all.x = T)
  customers <- merge(customers, punct.early, by = "customer_db_id", all.x = T)
  customers <- merge(customers, punct, all.x = T, by = "customer_db_id")
  
  customers[unpunctual_orders > 0 & completed_orders > 0,
            unpunctual_ratio := unpunctual_orders / completed_orders]
  
  return(customers)
}

GetNPS <- function(customers){
  nps <- fread("data/input/NPS.csv")
  nps <- nps[, c("NPS", "email")]
  customers <- merge(customers, nps, all.x = T, by = "email")
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
  
  # first order cluster
  orders.last.completed <- orders[order_state == "completed" 
                                   & order(order_created_datetime), 
                                   .SD[.N], by = customer_db_id]
  customers <- MergeToCustomers(customers, 
                                orders.last.completed[, c("customer_db_id", "cluster")],
                                "cluster",
                                "last_order_cluster")
  
}

TransformData <- function(){

  library(data.table)
  library(tidyr)
  
  source("utils/utils.R")
  
  
  orders <- fread("data/order_churn_data.csv")
  constant.cols <- c("customer_db_id", "customer_id", "gender", "segment", "email",
                     "aov", "recency", "frequency", "churn_factor", "referred", 
                     "newsletter_optin", "city")
  customers <- orders[, constant.cols, with = F]
  customers <- customers[!duplicated(customers)]
  
  customers <- MergeFirstAndLastOrder(customers, orders)
  
  transformations <- c(CalcOrderCounts, CalcOrderDates, CalcServiceClass,
                       CalcSoftwareType, CalcBasketSegments, CalcVoucherUsage,
                       CalcRevenue, CalcClusters, 
                       CalcRecleans, CalcReschedules, CalcRatings, CalcRefunds,
                       CalcPickUps, CalcPunctuality, CalcClosestLaundry, GetNPS)
  res <- lapply(transformations, function(f) customers <<- f(customers, orders))
  
  write.csv(customers, "data/churn_dataset.csv", row.names = F,
            fileEncoding = "utf-8")
}

