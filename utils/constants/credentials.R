alyx_url <- "https://alyx-lb.zipjet.com/api/v2//orgs/zipjet-"

db_credentials <- list(
  dbs = c(
    "uk_live",
    "alyx-live",
    "cleanio_db",
    "adjust"
  ),
  urls = c(
    "mongodb://172.31.51.215:27017",
    "mongodb://172.31.51.215:27017",
    "mongodb://app_prod:Gdk9LnEDyFF8EDgVF2x2Qrv3FV8hPjqD@lamppost.4.mongolayer.com:10182/cleanio_db",
    "mongodb://10.10.15.193:27017"
  ),
  collections = list(
    c("intwash_orders", "intwash_customers", "da_action_results",
      "intwash_driver_pickups", "intwash_driver_deliveries",
      "intwash_external_fulfillment", "intwash_notifications", 
      "intwash_invoices", "intwash_driver_deliveries", "providers", "ratings",
      "intwash_corporate_visits", "intwash_facilities",
      "intwash_orders_reschedules", "intwash_corporate_invoices",
      "intwash_orders_subprocesses", "intwash_corporate_partners",
      "intwash_voucher_items", "intwash_voucher_models",
      "intwash_voucher_blocks"),
    c("bi_task_predictions", "organisations.areas",
      "organisations.reaches", "organisations.fleets"),
    c("orders"),
    c("events")
  )
)