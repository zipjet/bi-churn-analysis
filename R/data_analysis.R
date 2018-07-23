print(dim(customers))
cols.nan <- data.table(sapply(orders, function(x) sum(is.na(x))))
cols.nan$col_name <- names(orders)

# rating
rating.ratio <- dim(customers[rated_orders > 0])[1] / dim(customers)[1]
print(rating.ratio)
# first rating 2016-02-15 15:20:30.000Z

