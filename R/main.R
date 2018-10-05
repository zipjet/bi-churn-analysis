Main <- function(){
  source("R/data_transformer.R", local = T)
  source("utils/configs.R", local = T)
  
  
  SetPackages("packages.csv")
  TransformData(T)
}


Main()