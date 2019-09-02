Sys.setenv(TZ='IR')


# log service -------------------------------------------------------------


service_log <- function(service, statusCode, message) {
  log_dist <- paste0("log/", service, ".txt")
  sink(log_dist, append = TRUE)
  time <- Sys.time()
  log_text <- paste(time, statusCode, message, "\n")
  cat(log_text)
  sink()
}


# dat aof instruments -----------------------------------------------------

instrument_data <- function(ticker, timeframe) {
  data_path <- paste0("data/tickers/", ticker, ".rda")
  load(data_path)
  switch(timeframe,
         m1 = result <- data,
         m3 = result <- to.minutes3(dataa),
         m5 = result <- to.minutes5(data),
         m10 = result <- to.minutes10(data),
         m15 = result <- to.minutes15(data),
         m30 = result <- to.minutes30(data),
         hourly = result <- to.hourly(data),
         daily = result <- to.daily(data),
         weekly = result <- to.weekly(data),
         monthly = result <- to.monthly(data),
         quarterly = result <- to.quarterly(data)
  )
}


# data of instruments -----------------------------------------------------

instrument_data <- function(ticker, timeframe) {
  data_path <- paste0("data/tickers/", ticker, ".rda")
  load(data_path)
  switch(timeframe,
         m1 = result <- data,
         m3 = result <- to.minutes3(dataa),
         m5 = result <- to.minutes5(data),
         m10 = result <- to.minutes10(data),
         m15 = result <- to.minutes15(data),
         m30 = result <- to.minutes30(data),
         hourly = result <- to.hourly(data),
         daily = result <- to.daily(data),
         weekly = result <- to.weekly(data),
         monthly = result <- to.monthly(data),
         quarterly = result <- to.quarterly(data)
  )
}


# Re Entering function ----------------------------------------------------

ReEnt <- function(B,bb,type,amm){
  n <- nrow(B)
  tar <- index(B)
  nn <- -c(1:n)
  pp <- as.numeric(bb[tar,4])
  BB <- data.frame(Price = pp,Trade = nn)
  res <- xts(BB,order.by = tar)
  tar[n+1] <- Sys.time()
  ta <- tar[-1]
  nB <- vector()
  nBp <- vector()
  nBt <- vector()
  k <- 0
  if(type == "Percentage_Below"){
    for (i in 1:n) {
      baz <- paste(tar[i],ta[i],sep = "/")
      temp <- bb[baz]
      np <- as.numeric(temp[tar[i],4])
      m <- tar[i]
      while (!is.na(m)) {
        np <- floor(GeometricSequence(2,np,((100 - amm)/100))[2])
        baz <- paste(m,ta[i],sep = "/")
        ntemp <- temp[baz]
        m <- index(ntemp[which(ntemp[,3] < np & np < ntemp[,4]),])[1]
        if(!is.na(m)){
          k <- k + 1
          nB[k] <- as.character(m)
          nBp[k] <- np
          nBt[k] <- i
        }
      }
    }
  }else if(type == "PriceTick_Below"){
    for (i in 1:n) {
      baz <- paste(tar[i],ta[i],sep = "/")
      temp <- bb[baz]
      np <- as.numeric(temp[tar[i],4])
      m <- tar[i]
      while (!is.na(m)) {
        np <- np - amm
        baz <- paste(m,ta[i],sep = "/")
        ntemp <- temp[baz]
        m <- index(ntemp[which(ntemp[,3] < np & np < ntemp[,4]),])[1]
        if(!is.na(m)){
          k <- k + 1
          nB[k] <- as.character(m)
          nBp[k] <- np
          nBt[k] <- i
        }
      }
    }
  }else if(type == "Percentage_Above"){
    for (i in 1:n) {
      baz <- paste(tar[i],ta[i],sep = "/")
      temp <- bb[baz]
      np <- as.numeric(temp[tar[i],4])
      m <- tar[i]
      while (!is.na(m)) {
        np <- floor(GeometricSequence(2,np,((100 + amm)/100))[2])
        baz <- paste(m,ta[i],sep = "/")
        ntemp <- temp[baz]
        m <- index(ntemp[which(ntemp[,4] > np & np > ntemp[,3]),])[1]
        if(!is.na(m)){
          k <- k + 1
          nB[k] <- as.character(m)
          nBp[k] <- np
          nBt[k] <- i
        }
      }
    }
  }else if(type == "PriceTick_Above"){
    for (i in 1:n) {
      baz <- paste(tar[i],ta[i],sep = "/")
      temp <- bb[baz]
      np <- as.numeric(temp[tar[i],4])
      m <- tar[i]
      while (!is.na(m)) {
        np <- np + amm
        baz <- paste(m,ta[i],sep = "/")
        ntemp <- temp[baz]
        m <- index(ntemp[which(ntemp[,4] > np & np > ntemp[,3]),])[1]
        if(!is.na(m)){
          k <- k + 1
          nB[k] <- as.character(m)
          nBp[k] <- np
          nBt[k] <- i
        }
      }
    }
  }
  BB <- data.frame(Price = nBp,Trade = nBt)
  result <- xts(BB,order.by = as.POSIXct(nB))
  result <- rbind(res,result)
  result
}


