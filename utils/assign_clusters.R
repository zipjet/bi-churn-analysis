# data is data table that must have following cols:
# x, y <- lat, long
# city, id
assign_cluster <- function(data, rm.coords=T){
  library(data.table)

  data_na <- data[is.na(x)]
  data_na[, fleet := NA]
  
  data <- data[!is.na(x)]
  
  data_berlin <- data[city == "Berlin"]
  data_berlin[, fleet := "Zentrallager"]
  data_berlin$coords.x1 <- data_berlin[['x']]
  data_berlin$coords.x2 <- data_berlin[['y']]
  
  data <- rbind(data_berlin, 
                assign_cluster_helper(data, "London"),
                assign_cluster_helper(data, "Paris"), 
                data_na, fill = T)
  
  ifelse (rm.coords, return(data[, -c("x", "y")]), return(data))
}


assign_cluster_helper <- function(data, city_name){
  library(sp)
  library(spatialEco)
  
  
  source("utils/constants/outliers.R", local=T)
  source("utils/utils.R", local=T)
  source("utils/vis.R", local=T)
  
  
  get_closest_cluster <- function(x){
    library(geosphere)
    
    min_dist <- .Machine$double.xmax
    min_dist_fleet <- "NA"
    point <- as.numeric(x[c("y", "x")])
    fleets <- clusters_df$fleet
    
    for (i in 1:length(fleets)){
      cluster <- clusters_df@polygons[[i]]@Polygons[[1]]@coords[, c(2, 1)]
      dist <- dist2Line(point, cluster)
      if (dist < min_dist){
        min_dist <- dist
        min_dist_fleet <- fleets[i]
      }
    }
    return(min_dist_fleet)
  }
  
  
  lookup_fleet <- fread("data/input/lookup_fleet.csv")
  data <- data[city == city_name]
  points <- data
  
  if (nrow(points) != 0){
    coordinates(points) <- ~x+y
    clusters_df <- GetBordersDF(city_name)
    points <- data.table(as.data.frame(point.in.poly(points, clusters_df)))
    
    points_out <- data[!id %in% points$id]
    points_out <- points_out[!id %in% spatial_outliers_ids]
    
    lookup_fleet <- lookup_fleet[, id := sub('-[^-]*$', '', id)]
    lookup_fleet <- lookup_fleet[!duplicated(id)]
    points_out <- merge(points_out, lookup_fleet, all.x=T, by="id")
    
    points <- rbind(points, points_out[!is.na(fleet)], use.names=T, fill=T)
    
    points_out <- points_out[is.na(fleet)]
    if (nrow(points_out)>0){
      points_out$fleet <- apply(points_out, 1, get_closest_cluster)
      lookup_fleet <- rbind(lookup_fleet, points_out[, c("id", "fleet")])
      write.csv(lookup_fleet, file = "data/input/lookup_fleet.csv",
                row.names = F)
    }
    points <- rbind(points, points_out, use.names=T, fill = T)
    
    # gg_obj <- vis_assign_clusters(points, clusters_df)
    # ggsave(paste0("../imgs/clusters_", city_name, ".png"), gg_obj)  
  }
  
  return(points)
}


GetBordersDF <- function(city.name){
  library(sp)

  
  load("data/input/fleet_areas.RData")
  borders <- borders[city == city.name]
  cluster.polygons <- list()
  
  for (i in 1:nrow(borders)){
    coords.df <- as.matrix(borders[i, ]$coords[[1]])
    cluster.polygons <- append(
      cluster.polygons,
      Polygons(list(Polygon(as.matrix(coords.df[, 1:2]))), as.character(i))
    )
  }
  borders.df <- SpatialPolygonsDataFrame(
    SpatialPolygons(cluster.polygons), 
    data.frame(fleet = borders$fleet)
  )
  
  return(borders.df)
}