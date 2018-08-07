library(data.table)

path <- "data/NPS/"
files <- rev(list.files(path = path)) # reversed to start with UK to keep english column names

data <- data.table()

for (file in files){
  print(file)
  survey <- fread(paste0(path, file))
  survey.cols <- names(survey)
  
  question.idx.start <- which(survey.cols == "Email & Nps") + 1
  question.idx.end <- which(grepl("feedback|Feedback|remarques", survey.cols))
  question.cols <- survey.cols[question.idx.start:question.idx.end]
  info.cols <- c("Response ID", "Date Submitted", "URL Variable: email", "URL Variable: nps")
  
  cols <- c(info.cols, question.cols)
  print(cols)
  survey <- survey[, ..cols]
  survey$file <- file
  
  data <- rbind(data, survey, use.names = F)
}

write.csv(data, "data/NPS.csv", row.names = F, fileEncoding = "utf-8")
