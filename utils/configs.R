SetPackages <- function(packages.fi){
  library(data.table)
  library(devtools)
  
  
  installed.packs <- data.table(installed.packages()[, c("Package", "Version")])
  project.packs <- fread(packages.fi)
  
  missing.packs <- project.packs[!installed.packs, on = names(project.packs)]
  
  if(nrow(missing.packs) != 0){
    for (i in 1:nrow(missing.packs)){
      install_version(missing.packs$Package[i], missing.packs$Version[i])
    }  
  }  
}