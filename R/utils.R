service_log <- function(service, statusCode, message) {
  log_dist <- paste0("log/", service, ".txt")
  sink(log_dist, append = TRUE)
  time <- Sys.time()
  log_text <- paste(time, statusCode, message, "\n")
  cat(log_text)
  sink()
}
