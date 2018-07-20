library(data.table)

MergeToCustomers <- function(customers, to.merge, col.name.old, col.name.new, 
                             by.x="customer_db_id", by.y="customer_db_id"){
  cust.merged <- merge(customers, to.merge, all.x = T, by.x = by.x, by.y = by.y)
  setnames(cust.merged, col.name.old, col.name.new)
  
  return(cust.merged)
}


CalcOrders <- function(customers, orders, orders.last){
  
  CountOrders <- function(customers, orders, order_states, col_name){
    orders.states <- orders[order_state %in% order_states, .N , by=customer_db_id]
    customers <- MergeToCustomers(customers, orders.states, "N", col_name)
    customers[is.na(customers[[col_name]]), (col_name) := 0]
    
    return(customers)
  }
  
  customers <- CountOrders(customers, orders, c("completed", "reserved"), "valid_orders")
  customers <- CountOrders(customers, orders, c("processing"), "open_orders")
  customers <- CountOrders(customers, orders, c("canceled"), "canceled_orders")
  customers <- CountOrders(customers, orders, c("payment_error"), "pay_error_orders")
  customers[, total_orders := valid_orders + open_orders + canceled_orders + pay_error_orders]
  customers <- MergeToCustomers(customers, orders.last[, c("customer_db_id", "order_state")], 
                                "order_state", "last_order_state")
  
  return(customers)
}                    


CalcServiceClass <- function(customers, orders, orders.last, orders.first){
  cust.class <- orders[, .N, by = c("customer_db_id", "service_class")]
  cust.class <- dcast(cust.class, `customer_db_id` ~ service_class, value.var = "N")
  cust.class <- data.table(cust.class)
  for (i in names(cust.class))
    cust.class[is.na(get(i)), (i):=0]
  customers <- merge(customers, cust.class, by = "customer_db_id", all.x = TRUE)
  
  customers <- MergeToCustomers(
    customers, orders.last[, c("customer_db_id", "service_class")], 
    "service_class", "last_order_service_class")
  customers <- MergeToCustomers(
    customers, orders.first[, c("customer_db_id", "service_class")], 
    "service_class", "first_order_service_class")
  
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
    customers, orders.last[, c("customer_db_id", "voucher_channel")], 
    "voucher_channel", "last_order_voucher_channel")
  
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
  customers[valid_orders > 0, reclean_ratio := reclean_orders / valid_orders]
  
  return(customers)
}

CalcReschedules <- function(customers, orders){
  # reschedules
  reschedules <- orders[
    , .(internal_reschedules = sum(internal_reschedule, na.rm = T), 
        customer_reschedules = sum(customer_reschedule, na.rm = T)), 
    by = customer_db_id]
  customers <- merge(customers, reschedules, all.x = T, by = "customer_db_id")
  
  return(customers)
}

CalcRatings <- function(customers, orders, orders.last, orders.first){

  ratings <- orders[!is.na(rating), 
                    .(rated_orders = .N, avg_rating = mean(rating)), 
                    by = customer_db_id]

  customers <- merge(customers, ratings, all.x = T, by = "customer_db_id")
  customers[is.na(rated_orders), rated_orders := 0]
  customers[, rated_orders_ratio := rated_orders / (valid_orders + canceled_orders)]
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


CalcFacilities <- function(customers, orders, orders.last, orders.first){
  avg.dist <- orders[, mean(fac_dist, na.rm = T), by = customer_db_id]
  customers <- MergeToCustomers(customers, avg.dist, "V1", "avg_fac_distance")
  
  customers <- MergeToCustomers(
    customers, orders.last[, c("customer_db_id", "fac_name")],
    "fac_name", "last_order_fac_name")
  
  customers <- MergeToCustomers(
    customers, orders.first[, c("customer_db_id", "fac_name")],
    "fac_name", "first_order_fac_name")
  
  return(customers)
}

orders <- fread("churn_analysis/data/orders.csv")
constant.cols <- c("customer_db_id", "customer_id", "gender", "segment", 
                   "aov", "recency", "frequency", "churn_factor", "referred", 
                   "newsletter_optin")
customers <- orders[city == "London", constant.cols, with = F]
customers <- customers[!duplicated(customers)]

orders.last <- orders[order(order_created_datetime, decreasing = T), 
                          .SD[1], by = customer_db_id]
orders.first <- orders[order(order_created_datetime), .SD[1], by = customer_db_id]

# BEHAVIOURAL
customers <- CalcOrders(customers, orders, orders.last)
customers <- CalcServiceClass(customers, orders, orders.last, orders.first)
customers <- CalcBasketSegments(customers, orders, orders.first)
customers <- CalcVoucherUsage(customers, orders, orders.last, orders.first)

# EXPERIENCE
customers <- CalcRecleans(customers, orders, orders.last)
customers <- CalcReschedules(customers, orders)
customers <- CalcRatings(customers, orders, orders.last, orders.first)
customers <- CalcRefunds(customers, orders, orders.last)


## OPERATIONAL
customers <- CalcPickUps(customers, orders)
customers <- CalcFacilities(customers, orders, orders.last, orders.first)

# INDIVIDUAL
customers <- CalcZip(customers, orders, orders.last)

write.csv(customers, "churn_analysis/data/churn_dataset.csv", row.names = F,
          fileEncoding = "utf-8")

