library(data.table)


GetData <- function(){
  final.cols <- c("order_id", "customer_db_id", "order_created_datetime", "city",
                  "net_before_voucher_eur", "voucher_value")
  
  data <- fread("../reporting/powerbi-share/R_outputs/marketing_dataset.csv")
  item.clusters <- fread("data/item_clusters_seasons.csv")
  
  data <- data[order_state == "completed" & is_corporate == F, final.cols, with = F]
  
  data[, order_created_datetime := as.POSIXct(order_created_datetime)]
  data <- data[order(-order_created_datetime)]
  data[, first_order_date := order_created_datetime[1], by = customer_db_id]
  
  data[, num_orders := .N, by = customer_db_id]
  
  data <- data[as.Date(order_created_datetime) >= as.Date("2017-01-01")]
  data <- data[as.Date(order_created_datetime) <= as.Date("2018-06-30")]
  
  return(data)
}


FindFirstRevenueThreshold <- function(data){
  data <- GetData()
  item.clusters <- fread("data/item_clusters_cities.csv")
  
  data <- merge(data, item.clusters, all.x = T, by = "order_id")
  data <- data[!is.na(cluster)]
  
  
  data <- data[order_created_datetime == first_order_date]
  x1 = data[city == "Paris" & num_orders == 1]
  x2 = data[city == "Paris" & num_orders >= 5]
  x3 = data[city == "Paris" & num_orders >= 10]
  
  ggplot(data[city == "Paris" & num_orders == 1]) +
    geom_boxplot(aes(x = cluster, y = net_before_voucher_eur, alpha = 0.5,
                     color = "one_order")) + 
    geom_boxplot(data = data[city == "Paris" & num_orders >= 5], aes(x = cluster,
                                                                      y = net_before_voucher_eur,
                                                                      alpha = 0.5,
                                                                      color = "10+orders")) + 
    ylim(c(0, 125))
}


GetNotPunctual <- function(){
  GetNotPunctualByTime <- function(data){
    FilterDelays <- function(x){
      x[x > max.punct.lim] <- 0
      return(x)
    }
    
    
    data[, (delay.cols) := lapply(.SD, abs), .SDcols = delay.cols]
    data[, (delay.cols) := lapply(.SD, FilterDelays), .SDcols = delay.cols]
    data$delay <- apply(data[, delay.cols, with = F], 1, max)
    
    for (sc in names(punct.th.class)){
      data[service_class == sc & delay > punct.th.class[sc], not_punctual := T]  
    }
    
    data[!service_class %in% names(punct.th.class) & delay > punct.th,
         not_punctual := T]
    
    return (data)
  }
  
  
  punct.th.class <- c("PLUS" = 10, "EXPRESS" = 5)
  punct.th <- 15
  max.punct.lim <- 360 
  used.cols <- c("order_id", "delay_mins_PU", "delay_mins_DO",
                 "internal_rescheduled", "rating_punctuality", "rating",
                 "refund_type", "service_class")
  delay.cols <- c("delay_mins_PU", "delay_mins_DO")
  
  data <- fread("data/order_churn_data.csv")

  data <- data[, used.cols, with = F]
  data[is.na(internal_rescheduled), internal_rescheduled := F] # What are NAs???
  data[is.na(rating_punctuality), rating_punctuality := 0]
  data[is.na(rating), rating := 0]
  data[is.na(delay_mins_PU), delay_mins_PU := 0]
  data[is.na(delay_mins_DO), delay_mins_DO := 0]
  
  data <- GetNotPunctualByTime(data)
  data[internal_rescheduled == T, not_punctual := T]
  data[rating_punctuality == 1 & rating != 5, not_punctual := T]
  data[refund_type == "PUNCTUALITY", not_punctual := T]
  
  data <- data[not_punctual == T, c("order_id", "not_punctual"), with = F]
  
  return (data)
}


GetPoorCleanQual <- function(){
  data <- fread("data/order_churn_data.csv")
  
  used.cols <- c("order_id", "rating_cleaning quality",
                 "rating_ironing quality", "rating", "reclean_order",
                 "refund_type")
  data <- data[, used.cols, with = F]
  data[is.na(`rating_cleaning quality`), `rating_cleaning quality` := 0]
  data[is.na(`rating_ironing quality`), `rating_ironing quality` := 0]
  data[is.na(rating), rating := 0]
  data[is.na(reclean_order), reclean_order := F]
  
  data[`rating_cleaning quality` == 1 & rating != 5, poor_clean_qual := T]
  data[`rating_ironing quality` == 1 & rating != 5, poor_clean_qual := T]
  data[reclean_order == T, poor_clean_qual := T]
  
  data[refund_type == "CLEANING_ISSUES", poor_clean_qual := T]
  
  data <- data[poor_clean_qual == T, c("order_id", "poor_clean_qual"), with = F]
  
  return (data) 
}


GetLostDamaged <- function(){
  data <- fread("data/order_churn_data.csv")
  
  used.cols <- c("order_id", "rating_damaged item", "rating_missing items",
                 "refund_type")
  data <- data[, used.cols, with = F]
  
  data[is.na(`rating_damaged item`), `rating_damaged item` := 0]
  data[is.na(`rating_missing items`), `rating_missing items` := 0]
  
  data[`rating_damaged item` == 1, damaged_item := T]
  data[refund_type == "DAMAGED_ITEMS", damaged_item := T]
  
  data[`rating_missing items` == 1, lost_item := T]
  data[refund_type == "LOST_ITEMS", lost_item := T]
  
  data <- data[damaged_item | lost_item,
               c("order_id", "damaged_item", "lost_item"), with = F]
  
  return(data)
}


GetPoorServiceOther <- function(){
  used.cols <- c("order_id", "customer_id", "refund_type", "refund_approved",
                 "voucher_channel")
  nps.threshold <- 6
  
  
  data <- fread("data/order_churn_data.csv")
  customers <- fread("data/churn_dataset.csv")
  
  customers <- customers[, c("customer_id", "NPS")]
  data <- data[, used.cols, with = F]
  data <- merge(data, customers, all.x = T, by = "customer_id")

  data[refund_type == "ITEMIZATION_ISSUES", poor_serv_other := T]
  data[refund_type == "OTHER" & refund_approved == T, poor_serv_other := T]
  data[voucher_channel == "CC", poor_serv_other := T]
  data[NPS <= nps.threshold, poor_serv_other := T]
  
  data <- data[poor_serv_other == T, c("order_id", "poor_serv_other"), with = F]
  
  return(data)
}