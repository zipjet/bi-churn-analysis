GenItemsOneHot <- function(){
  library(data.table)
  
  
  items <- fread("data/items.csv")
  
  not.items.group <- "Special Price / PKW / Plaid / Quote / Test / Minimum order value / Delivery / Rescheduling / Express / DefaultItem / Outfittery"
  other.items.group <- c("Teddy", "Bolster", "Hat / Cap", "Curtain", "Cushion")
  
  items.from <- c("Napkin / Serviette", "Bathmat")
  items.to <- c("Tablecloth", "WashnFold")
  
  prod.type.long <- c("Evening Dress / Evening Gown / Wedding Dress",
                      "Cardigan / Sweater / Sweatshirt / Jumper / Knitwear",
                      "Wash and Fold / Bag of Folded Laundry / Colour Separation",
                      "Smock / Tuxedo  / Dinner Jacket / Tailcoat", 
                      "Ski Pants / Skiing Suit / Snow Suit / Ski trouser",
                      "Chef / Chef Jacket / Cook Jacket / Chef Trousers")
  
  prod.type.short <- c("Evening Dress", "Sweater", "WashnFold", "Smock",
                       "Ski", "Chef")
  
  items <- items[product_type != not.items.group]
  items <- items[product_type %in% other.items.group, product_type := "Other"]
  for (i in 1:length(items.from)){
    items[product_type == items.from[i], product_type := items.to[i]]
  }
  
  for (i in 1:length(prod.type.short)){
    items[product_type == prod.type.long[i], product_type := prod.type.short[i]]
  }
  
  items <- items[!grepl("RC", order_id)]
  
  items <- items[, c("order_id", "product_type", "quantity"), with = F]
  
  
  items <- items[, quantity := sum(quantity), by = c("order_id", "product_type")]
  items <- unique(items, by = c("order_id", "product_type"))
  items <- dcast(items, order_id ~ product_type)
  
  order.ids <- items$order_id
  items[!is.na(items)] <- 1
  items[is.na(items)] <- 0
  
  items$order_id <- order.ids
  
  write.csv(items, file = "data/one_hot_items.csv", row.names = F)
}



items <- fread("data/items_with_clusters.csv")
items <- melt(items, id.vars = "cluster", variable.name = "product_type", value.name = "count")
items <- items[, sum(count), by = c("cluster", "product_type")]


ggplot(items, aes(cluster, product_type)) +
  geom_tile(aes(fill = V1)) + 
  geom_text(aes(label = round(V1, 1))) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
