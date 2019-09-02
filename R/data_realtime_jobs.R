setwd("/home/rstudio/R/Billionaire-Jobs/")
source("R/utils.R")
source("R/getting_token.R")
source("R/getting_realtime.R")
library(httr)
library(jsonlite)

service_log("data_realtime_jobs", 100, "done")

if(!weekdays(Sys.Date()) %in% c("Thursday", "Friday") && as.numeric(format(Sys.time(), "%H")) <= 12) {
  today_path <- paste0("data/", Sys.Date(), ".rda")
  if(!file.exists(today_path)) {
    minutely_data <- getting_realtime_minutely()
    today_df <- minutely_data
    save(today_df, file = today_path)
  } else {
    load(today_path)
    minutely_data <- getting_realtime_minutely()
    today_df <- rbind(today_df,minutely_data)
    today_df <- unique(today_df)
    save(today_df, file = today_path)
  }
}


