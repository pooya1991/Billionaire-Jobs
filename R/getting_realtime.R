source("R/utils.R")
source("R/getting_token.R")
library(httr)
library(jsonlite)

getting_realtime_minutely <- function() {
  
  load("data/token.rda")
  
  service <- "realtime"
  
  address <- "https://apibeta.finnotech.ir"
  clientID <- "Billionaire"
  finnotech_version <- "v2"
  token <- content_token[["result"]][["value"]]
  
  national_number = "2740370733"
  
  service_url <- paste(address, "bourse", finnotech_version, "clients", clientID, "trades", service, sep = "/")
  realtime_req <- GET(service_url, add_headers(Authorization = paste("Bearer", token)))
  realtime_content <- jsonlite::fromJSON(content(realtime_req, "text"),simplifyVector = FALSE)
  k = 0
  if(status_code(realtime_req) == 200) {
    realtime <- realtime_content[["result"]]
    l_realtime <- lapply(realtime, realtime_parser)
    df_realtime <- do.call(rbind, l_realtime)
    service_log(service = service, statusCode = status_code(realtime_req), message = "Success")
    return(df_realtime)
  } else {
    if(status_code(realtime_req) == 403 && k < 3) {
      k = k + 1
      service_log(service = service, statusCode = status_code(realtime_req), message = realtime_content$message)
      refresh_token(content_token$result$refreshToken)
      load("data/token.rda")
      getting_realtime_minutely()
    } else {
      service_log(service = service, statusCode = status_code(realtime_req), message = realtime_content$message)
    }
  }
}


realtime_parser <- function(realtime_element){
  result <- data.frame(CoID = realtime_element$CoID, Symbol = realtime_element$TseSymbolCode,
                       Date = realtime_element$TradeDateGre, Date_fa = realtime_element$TradeDate,
                       Open = realtime_element$OpeningPrice, Close = realtime_element$ClosingPrice,
                       High = realtime_element$MaxPrice, Low = realtime_element$minPrice,
                       Last = realtime_element$LastPrice, Volume = realtime_element$TradeVolume,
                       Value = realtime_element$TradeValue, Qty = realtime_element$TradeQty)
  return(result)
}



# Daily data --------------------------------------------------------------

getting_daily_minutely <- function(coID) {
  
  load("data/token.rda")
  
  service <- "realtime"
  
  address <- "https://apibeta.finnotech.ir"
  clientID <- "Billionaire"
  finnotech_version <- "v2"
  token <- content_token[["result"]][["value"]]
  coid <- paste0("?coId=", coID)
  
  national_number = "2740370733"
  
  service_url <- paste(address, "bourse", finnotech_version, "clients", clientID, "trades", service, sep = "/")
  service_url <- paste0(service_url, coid)
  realtime_req <- GET(service_url, add_headers(Authorization = paste("Bearer", token)))
  realtime_content <- jsonlite::fromJSON(content(realtime_req, "text"),simplifyVector = FALSE)
  k = 0
  if(status_code(realtime_req) == 200) {
    realtime <- realtime_content[["result"]]
    l_realtime <- lapply(realtime, realtime_parser)
    df_realtime <- do.call(rbind, l_realtime)
    service_log(service = service, statusCode = status_code(realtime_req), message = "Success")
    return(df_realtime)
  } else {
    if(status_code(realtime_req) == 401 && k < 3) {
      k = k + 1
      service_log(service = service, statusCode = status_code(realtime_req), message = realtime_content$message)
      refresh_token(content_token$access_token$refreshToken)
      load("data/token.rda")
      getting_daily_minutely(coID)
    } else {
      service_log(service = service, statusCode = status_code(realtime_req), message = realtime_content$message)
    }
  }
}
