library(data.table)
library(zoo)
library(ggplot2)
library(plyr)

# Each order should have s2 cell data, order -> pu and do (b2c and b2b)

GetSupplyByS2CellTime <- function(){
  supply <- fread("../demand_prediction/dp/data/supply/temp.csv")
  supply[timeslot_from > "14:00", shift := "ES"]
  supply[is.na(shift), shift := "MS"]
  supply <- supply[t_back_bucket >= -48]
  supply[, month_year := as.yearmon(as.Date(day))]
  
  supply <- supply[, mean(avail_area_ratio),
                   by = c("cover_cell_id", "shift", "day_of_week")]
  setnames(supply, "V1", "mean_supply")
  
  return (supply)
}



GetS2Cells <- function(data){
  demand <- fread("../demand_prediction/dp/data/supply/demand_berlin_lvl12.csv")
  demand[, id := gsub("-PU|-RC2-PU|-DO|-RC2-DO", "", id)]
  demand <- demand[, c("id", "cover_cell_id")]
  demand <- demand[!duplicated(id)]
  
  data <- merge(data, demand, all.x = T, by.x = c("order_id"), by.y=c("id")) 
  #View(head(demand))
  
  return(data)
}


GetCustomerExp <- function(data){
  ratings <- fread("../reporting/powerbi-share/R_outputs/ratings.csv")
  final.cols <- c("order_ref", "topics_cleaning quality", "topics_damaged item", 
                  "topics_driver conduct", "topics_ironing quality", 
                  "topics_missing items", "topics_punctuality")
  ratings <- ratings[, final.cols, with = F]
  
  data <- merge(data, ratings, all.x = T, by.x = "order_id", by.y = "order_ref")
  
  return (data)
}


GetPunctuality <- function(data, late.threshold = 10, early.threshold = 10){
  final.cols <- c("id", "task_type", "late_by_more_than_one_min",
                  "early_by_more_than_five_min")
  punct <- fread("../reporting/powerbi-share/R_outputs/punctuality.csv")
  punct[grepl("PU", reference), task_type := "PU"]
  punct[is.na(task_type), task_type := "DO"]
  punct[, id := gsub("-PU|-RC2-PU|-DO", "", reference)]
  punct <- punct[, final.cols, with = F]
  
  punct <- punct[late_by_more_than_one_min > late.threshold |
                   early.threshold > early.threshold]
  punct <- unique(punct, by = "id")
  punct[, not_punctual := T]
  punct <- punct[, c("id", "not_punctual"), with = F]
  
  data <- merge(data, punct, all.x = T, by.x = "order_id", by.y = "id")
  data[is.na(not_punctual), not_punctual := F]
  
  return (data)
}


GetPickupDate <- function(data){
  final.cols <- c("id", "task_type", "late_by_more_than_one_min",
                  "early_by_more_than_five_min")
  punct <- fread("../reporting/powerbi-share/R_outputs/punctuality.csv")
  
  punct <- punct[grepl("PU", reference)]
  punct[, order_id := gsub("-PU|-RC2-PU|-DO", "", reference)]
  punct <- punct[, c("order_id", "timeslot_from"), with = F]
  punct[, day := as.Date(timeslot_from)]
  punct[, day_of_week := weekdays(day)]
  punct[, time := strftime(timeslot_from, format="%H:%M")]
  punct[time > "14:00", shift := "ES"]
  punct[is.na(shift), shift := "MS"]
  
  punct <- punct[, c("order_id", "day_of_week", "shift")]
  data <- merge(data, punct, all.x = T, by = "order_id")
  
  return(data)
}


GetCustomersByNumOrders <- function(num.orders, current.date, churn.threshold){
  start.date <- as.Date("2017-01-01")
  end.date <- as.Date("2018-06-01")
  final.cols <- c("order_id", "city", "churned", "net_before_voucher_eur",
                  "voucher_value", "voucher_ratio", "customer_db_id")
  
  data <- fread("../reporting/powerbi-share/R_outputs/marketing_dataset.csv")
  data <- data[order_state == "completed"]
  
  data[, num_orders := .N, by = "customer_db_id"]
  
  data <- data[, order_created_datetime := as.POSIXct(order_created_datetime)]
  data <- data[order(-order_created_datetime)]
  data[, last_order_date := order_created_datetime[1], by = "customer_db_id"]
  
  data[, days_since_last_order := difftime(current.date,
                                           as.Date(last_order_date))]
  data <- data[days_since_last_order >= churn.threshold, churned := T]
  data[is.na(churned), churned := F]
  
  data <- data[as.Date(order_created_datetime) >= start.date]
  data <- data[as.Date(order_created_datetime) <= end.date]
  
  
  data[, voucher_ratio := voucher_value / net_before_voucher_eur]
  
  data <- data[num_orders == num.orders]
  data <- data[, final.cols, with = F]
  
  data <- GetCustomerExp(data)
  data <- GetPunctuality(data)
  data <- GetS2Cells(data)
  data <- GetPickupDate(data)

  supply <- GetSupplyByS2CellTime()
  data <- merge(data, supply, all.x = T, by = c("cover_cell_id", "shift", "day_of_week"))

  return (data)
  
}


data <- GetCustomersByNumOrders(1, as.Date("2018-10-02"), 120)
data <- data[city == "Berlin"]




# Cluster
data[(net_before_voucher_eur - voucher_value) < 30 | voucher_ratio > 0.7, churn_cluster_1 := T]
data[is.na(churn_cluster_1), churn_cluster_1 := F]

data[`topics_cleaning quality` == 1|`topics_damaged item`  == 1|
     `topics_driver conduct` == 1 | `topics_ironing quality`  == 1|
     `topics_missing items` == 1| topics_punctuality == 1, churn_cluster_2 := T]
data[not_punctual == T, churn_cluster_2 := T]
data[is.na(churn_cluster_2), churn_cluster_2 := F]
data[mean_supply < 0.1]

# Count by clusters
data[, .N, by = churned]
dim(data[churned == T & churn_cluster_1 == T & churn_cluster_2 == T])
dim(data[churned == T & churn_cluster_1 == F & churn_cluster_2 == T])



# Items
library(cluster)
items <- fread("items.csv")
items[is.na(product_type)]
items[product_type == "Special Price / PKW / Plaid / Quote / Test / Minimum order value / Delivery / Rescheduling / Express / DefaultItem / Outfittery", product_type := "Other"]
items <- items[, product_type_quantity := sum(quantity), by = c("order_id", "product_type")]
items <- items[, c("order_id", "product_type", "product_type_quantity")]
items <- unique(items, by = c("order_id", "product_type"))
items <- dcast(items, order_id ~ product_type)
items[is.na(items)] <- 0
write.csv(items, file = "items.csv", row.names = F)


# items <- items[variable != "Special Price / PKW / Plaid / Quote / Test / Minimum order value / Delivery / Rescheduling / Express / DefaultItem / Outfittery"]
# items <- dcast(items, order_id ~ variable)


# Clean data
items <- fread("items.csv")
items <- melt(items, id.vars = "order_id")
items <- items[!grepl("RC", order_id)]
items <- dcast(items, order_id ~ variable)
# 2. remove maximum outliers???


# 3. add features as gender, season, city, and b2c/b2b
data <- fread("../reporting/powerbi-share/R_outputs/marketing_dataset.csv")
data <- data[, c("is_corporate", "order_created_datetime", "city", "order_id")]
data[, month := month(as.Date(order_created_datetime))]
data[month < 3 | month == 12, season := 1] # winter - season 1
data[month > 2 & month <= 5, season := 2]
data[month > 5 & month <= 8, season := 3]
data[month > 8 & month <= 11, season := 4]

data$is_corporate <- mapvalues(data$is_corporate, c("TRUE", "FALSE"), c(1, 0))
data <- data[, -c("month", "order_created_datetime"), with = F]

items <- merge(items, data, all.x = T, by = "order_id")

items$city <- mapvalues(items$city, c("London", "Berlin", "Paris"), c(1, 2, 3))
write.csv(items, file = "items.csv", row.names = F)



# one hot season and city:
items <- fread("items.csv")
seasons <- items[, c("order_id", "season"), with = F]
seasons <- dcast(seasons, order_id ~ season)

seasons[is.na(seasons)] <- 0
bin.cols <- seasons[, 2:5]
bin.cols[bin.cols > 0] <- 1
seasons[, 2:5] <- bin.cols
names(seasons) <- c("order_id", "winter", "spring", "summer", "fall")
items <- items[, -c("season"), with = F]
items <- merge(items, seasons, all.x = T, by = "order_id")


items <- items[, -c("city"), with = F]
items <- melt(items, id.vars = c("order_id", "is_corporate", "winter",
                                 "spring", "summer", "fall"))





# Bucket item counts
items <- fread("items.csv")

BucketItem <- function(item.col){
  breaks <- quantile(item.col[item.col > 0], c(0.25, 0.5, 0.75))
  if (breaks[1] != 0){
    breaks <- c(0, breaks, max(item.col))  
  }else{
    breaks <- c(breaks, max(item.col))
  }
  
  buckets <- cut(item.col, breaks)
  buckets <- as.character(buckets)
  
  ParseBucket <- function(s){
    if (is.na(s)) return (NA)
    
    s <- gsub("\\(", "", strsplit(s, split=",")[[1]][1])
    if (s == "0") return ("1")
    
    return (s)
  }
  
  buckets <- sapply(buckets, ParseBucket)
  
  return (buckets)
}

