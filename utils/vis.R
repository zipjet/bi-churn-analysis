vis_assign_clusters <- function(data, cluster_df){
  library(ggplot2)
  
  cluster_df@data$id <- rownames(cluster_df@data)
  cluster_gg <- fortify(cluster_df, region = "id")
  cluster_gg <- merge(cluster_gg, cluster_df@data, by="id")
  
  color_pallete <- c("red", "blue", "green", "magenta", "cyan")
  fleets <- unique(data$fleet)
  
  gg_obj <- ggplot() + geom_polygon(data = cluster_gg, 
                                    aes(x=lat, y=long, group=group), alpha = 0.5)
  
  gg_obj <- gg_obj + geom_point(aes(x = data[fleet == fleets[1]]$y, 
                                    y = data[fleet == fleets[1]]$x), 
                                color = color_pallete[1], alpha=0.5)
  
  gg_obj <- gg_obj + geom_point(aes(x = data[fleet == fleets[2]]$y, 
                                    y = data[fleet == fleets[2]]$x), 
                                color = color_pallete[2], alpha=0.5)
  
  gg_obj <- gg_obj + geom_point(aes(x = data[fleet == fleets[3]]$y, 
                                    y = data[fleet == fleets[3]]$x), 
                                color = color_pallete[3], alpha=0.5)
  
  gg_obj <- gg_obj + geom_point(aes(x = data[fleet == fleets[4]]$y, 
                                    y = data[fleet == fleets[4]]$x), 
                                color = color_pallete[4], alpha=0.5)
  return(gg_obj)
}