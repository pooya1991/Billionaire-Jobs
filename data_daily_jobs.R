setwd("/Users/pooya/Desktop/Billionaire/Jobs/Billionaire-Jobs")
source("R/utils.R")
source("R/getting_token.R")
source("R/getting_realtime.R")
library(httr)
library(jsonlite)
library(xts)

if(!weekdays(Sys.Date()) %in% c("Thursday", "Friday")) {
  today_path <- paste0("data/", Sys.Date(), ".rda")
  load(today_path)
  if(!is.null(today_path)) {
    CoIDs <- unique(today_df$CoID)
    for (coID in CoIDs) {
      today_total <- getting_daily_minutely(coID = coID)
      df_today <- today_total[, c("Open", "High", "Low", "Close", "Volume")]
      today_xts <- xts(df_today, order.by = as.POSIXct(today_total$Date))
      today_total_m <- to.minutes(today_xts)
      symbol <- as.character(today_total$Symbol[1])
      symbol_path <- paste0("data/tickers/", symbol, ".rda")
      if(file.exists(symbol_path)){
        load(symbol_path)
        data <- rbind(data, today_total_m)
        save(data, file = symbol_path)
      } else {
        data <- today_total_m
        save(data, file = symbol_path)
      }
    }
  } 
}


