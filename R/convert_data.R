library("xts")
dir <- "/home/rstudio/R/Billionaire_API/data/tickers"
shares <- list.files(path = dir, pattern = ".csv")
shares_dir <- paste(dir, shares, sep = "/")

stchnames_dir <- "/home/rstudio/R/Billionaire_API/data/stocknames.csv"
stocknames <- read.csv(stchnames_dir, header = F)

n <- length(shares_dir)
for(i in 377:n) {
  stochs_dir <- paste0("/home/rstudio/R/Billionaire_API/data/tickers/", stocknames[i,1], ".csv")
  a <- read.csv(stochs_dir)
  b <- cumsum(!is.na(a$close))
  c <- which(b > 0)[1]
  m <- nrow(a)
  data <- a[c:m,]
  aa <- a[complete.cases(a),]
  
  data = data.frame(Open = aa$open, High = aa$high, Low = aa$low, Close = aa$close, Volume = aa$volume)
  date = as.Date(as.character(aa$date), "%Y%m%d")
  time_tmp <- as.POSIXct(paste(date, aa$time), tz = "Asia/Tehran")
  data <- xts(data, order.by = time_tmp)
  
  save(data, file = paste0("/home/rstudio/R/Billionaire-Jobs/data/tickers/", stocknames[i,2], ".rda"))
}

invalid_ids <- c(37, 134, 146, 175, 182, 205, 213, 216, 274, 276, 284, 303, 312, 313, 316, 320, 349, 366, 376)
invalid_stochs <- stocknames[invalid_ids,]
