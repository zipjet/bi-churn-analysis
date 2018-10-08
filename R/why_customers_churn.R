library(data.table)


final.cols <- c("order_id", "customer_db_id", "order_created_datetime", "city",
                "net_before_voucher_eur", "voucher_value")

data <- fread("../reporting/powerbi-share/R_outputs/marketing_dataset.csv")
item.clusters <- fread("data/item_clusters_seasons.csv")

data <- data[order_state == "completed", final.cols, with = F]

data[, order_created_datetime := as.POSIXct(order_created_datetime)]
data <- data[order(-order_created_datetime)]
data[, first_order_date := order_created_datetime[1], by = customer_db_id]

data[, num_orders := .N, by = customer_db_id]

data <- data[as.Date(order_created_datetime) >= as.Date("2017-01-01")]
data <- data[as.Date(order_created_datetime) <= as.Date("2018-06-30")]

data <- merge(data, item.clusters, all.x = T, by = "order_id")


# add features for cluster 2

GetNotPunctualOrders <- function(){
  min.punct.lim <- 15
  max.punct.lim <- 360 
  
  used.cols <- c("order_id", "delay_mins_PU", "delay_mins_DO",
                 "internal_rescheduled", "rating_punctuality", "rating")
  
  orders <- fread("data/order_churn_data.csv")
  data <- orders[, used.cols, with = F]
  data[is.na(internal_rescheduled), internal_rescheduled := F] # What are NAs???
  data[is.na(rating_punctuality), rating_punctuality := 0]
  data[is.na(rating), rating := 0]
  
  data[delay_mins_PU > min.punct.lim & delay_mins_PU < max.punct.lim,
       not_punctual := T]
  data[delay_mins_PU < - min.punct.lim & delay_mins_PU > - max.punct.lim,
       not_punctual := T]
  data[delay_mins_DO > min.punct.lim & delay_mins_DO < max.punct.lim,
       not_punctual := T]
  data[delay_mins_DO < - min.punct.lim & delay_mins_DO > - max.punct.lim,
       not_punctual := T]
  
  data[internal_rescheduled == T, not_punctual := T]
  data[rating_punctuality == 1 & rating != 5, not_punctual := T]
  
  data <- data[not_punctual == T, c("order_id", "not_punctual"), with = F]

  return (data)
}






