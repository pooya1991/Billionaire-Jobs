library(httr)
library(jsonlite)

get_instruments <- function() {
  
  load("data/token.rda")
  
  service <- "companies"
  
  address <- "https://api.finnotech.ir"
  clientID <- "Billionaire"
  finnotech_version <- "v1"
  token <- content_token[["access_token"]][["value"]]
  
  service_url <- paste(address, "bourse", finnotech_version, "clients", clientID, service, sep = "/")
  
  instrument_get <- GET(service_url, add_headers(Authorization = paste("Bearer", token)))
  if(status_code(instrument_get) == 200) {
    instruments <- jsonlite::fromJSON(content(instrument_get, "text"),simplifyVector = FALSE)
    instruments <- instruments[["result"]]
    service_log(service = service, statusCode = status_code(instrument_get), message = "Success")
    save(instruments, file = "data/instruments.rda")
    return()
  } else {
    if(status_code(instrument_get) == 401) {
      result <- jsonlite::fromJSON(content(instrument_get, "text"),simplifyVector = FALSE)
      service_log(service = service, statusCode = status_code(instrument_get), message = result$message)
      refresh_token(content_token$access_token$refreshToken)
      load("data/token.rda")
      get_instruments()
    } else {
      result <- jsonlite::fromJSON(content(instrument_get, "text"),simplifyVector = FALSE)
      service_log(service = service, statusCode = status_code(instrument_get), message = result$message)
    }
  }
}

