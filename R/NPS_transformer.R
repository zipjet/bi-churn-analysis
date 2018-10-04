library(data.table)

path <- "data/NPS/"
files <- list.files(path = path) # reversed to start with UK to keep english column names

data <- data.table()

for (file in files){
  print(file)
  survey <- fread(paste0(path, file))
  names(survey) <- c("NPS", "email")
  survey$country <- substr(file, 1, 2)
  
  data <- rbind(data, survey, use.names = F)
}

write.csv(data, "data/input/NPS.csv", row.names = F, fileEncoding = "utf-8")
