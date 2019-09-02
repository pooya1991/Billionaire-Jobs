library(dplyr)
library(xts)
dir <- "data"
load(paste(dir, "active_strategies.rda", sep = "/"))
actives[as.Date(actives$EndDate) < Sys.Date(), "Status"] <- 0
save(actives, file = paste(dir, "active_strategies.rda", sep = "/"))
actives <- actives[actives$Timeframe == "hourly" && actives$Status == 1, ]
load(paste(dir, "strategies.rda", sep = "/"))
strategies <- strategies[strategies$StgID == actives$StgID, ]

valid_tickers <- unique(actives$Share)
n <- length(valid_tickers)
tickers_dir <- "data/tickers/"
for(i in 1:n) {
  load(paste(tickers_dir, vavalid_tickers[i], ".rda"))
  share <- to.hourly(data)
  actives_shares <- actives %>%
    filter(Share == valid_tickers[i])
  
  m <- nrow(actives_shares)
  for (j in 1:m) {
    Stg = strategies[strategies$StgID = actives_shares[j, "StgID"], "Stg"]
    res <- strategies_buyer(Stg = Stg, share = share, ReEnterType = actives_shares[j, "ReEnterType"], ReEnterAmm = actives_shares[j, "ReEnterAmm"])
    if(res$Buy) {
      #TODO add email service
      load("data/active_signals.rda")
      newBuySignals <- data.frame(time = format(Sys.time(), tz = "Asia/Tehran"), ActivationID = actives$ActivationID, StgID = actives$StgID, User = actives$User, Share = valid_tickers[i])
      BuySignals <- rbind(BuySignals, newBuySignals)
      save(BuySignals, file = "data/active_signals.rda")
    }
  }
}









