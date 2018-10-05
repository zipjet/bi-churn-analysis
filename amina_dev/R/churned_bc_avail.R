# Berlin customers only
library(data.table)

customers <- fread("../reporting/powerbi-share/R_outputs/customer_analysis.csv")
customers <- customers[city == "Berlin"]
customers[, order_created_datetime := as.POSIXct(order_created_datetime)]
customers <- customers[order(-order_created_datetime)]
customers <- customers[!duplicated(customer_db_id)]
customers <- customers[, c("customer_db_id", "order_x", "order_y", "order_created_datetime")]
customers[, days_since_last_order := as.numeric(Sys.Date() - as.Date(order_created_datetime))]
customers <- customers[days_since_last_order >= 365, churned := T]
customers[is.na(churned), churned := F]


laundries <- fread("laundries.csv")
laundries <- laundries[city == "Berlin"]
# add feature closest dist to laundry
# add feature is area undersupplied
# add feature timeslot is undersupplied

# Analayis of correlation
