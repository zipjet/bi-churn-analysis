GenItemsOneHot <- function(){
  library(data.table)
  library(hydroTSM)
  
  source("../reporting/etl/utils/utils.R", local = T)
  
  
  GetCity <- function(){
    items[grepl("DE", order_id), city := "Berlin"]
    items[grepl("GB", order_id), city := "London"]
    items[is.na(city), city := "Paris"]
    items <<- items
  }
  
  
  FilterData <- function(){
    items <- items[product_type != not.items.group]
    items <- items[product_type %in% other.items.group, product_type := "Other"]
    for (i in 1:length(items.from)){
      items[product_type == items.from[i], product_type := items.to[i]]
    }
    
    for (i in 1:length(prod.type.short)){
      items[product_type == prod.type.long[i],
            product_type := prod.type.short[i]]
    }
    
    items <- items[!grepl("RC", order_id)]
    
    items <<- items
  }
  
  
  OneHotCounts <- function(items){
    id.table <- items[, c("order_id", "city", "season"), with = F]
    items.table <- items[, -c("order_id", "city", "season"), with = F]
    items.table[!is.na(items.table)] <- 1
    items.table[is.na(items.table)] <- 0

    return (cbind(id.table, items.table))
  }
  
  
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
  
  FilterData()
  GetCity()
  items[, season := time2season(as.Date(order_date))]
  
  items <- items[, c("order_id", "product_type", "quantity", "city", "season"),
                 with = F]
  
  items <- items[, quantity := sum(quantity),
                 by = c("order_id", "product_type")]
  items <- unique(items, by = c("order_id", "product_type"))
  items <- dcast(items, order_id + city + season ~ product_type, value.var = "quantity")
  
  items <- OneHotCounts(items)
  
  write.csv(items, file = "data/one_hot_items.csv", row.names = F)
}



library(ggplot2)
library(data.table)

#item.clusters <- fread("data/item_clusters.csv")
item.clusters <- fread("data/item_clusters_cities.csv")
items <- fread("data/one_hot_items.csv")
items <- merge(items, item.clusters, by = "order_id")
items <- items[city == "London"]
items <- items[, -c("order_id", "city", "season"), with = F]
items <- melt(items, id.vars = "cluster", variable.name = "product_type", value.name = "count")
items <- items[, sum(count), by = c("cluster", "product_type")]


ggplot(items, aes(cluster, product_type)) +
  geom_tile(aes(fill = V1)) + 
  geom_text(aes(label = round(V1, 1))) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
