load("/home/ubuntu/Billionaire_API/data/strategies.rda")
load("/home/ubuntu/Billionaire_API/data/active_strategies.rda")

requireNamespace("jsonlite")
library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(dplyr)
library(MASS)

strategies_buyer <- function(Stg, share, ReEnterType = 0, ReEnterAmm = 0) {
  
  # get the Strategy
  x <- as.character(Stg)
  Stg <- jsonlite::fromJSON(x)
  if(Stg$BUY$Status == "Set"){
    EnRuls <- Stg$BUY$Enter$Rules
    EnRels <- Stg$BUY$Enter$Rels
    ExRuls <- Stg$BUY$Exit$Rules
    ExRels <- Stg$BUY$Exit$Rels
    StpLst <- Stg$BUY$Exit$StopLost
    TkPrft <- Stg$BUY$Exit$TakeProfit
    n <- length(EnRuls)
    for (i in 1:n) {
      m <- length(EnRuls[[i]]$Indicator)
      qqq <-"Ind_1"
      for (j in 1:m) {
        Ind <- EnRuls[[i]]$Indicator[[j]]$Indicator
        l <- length(EnRuls[[i]]$Indicator[[j]]$Parameters)
        #TODO fix this
        indslag <- EnRuls[[i]]$Indicator[[j]]$lag
        if(is.null(indslag)) indslag <- 0
        qq <- ""
        if(l > 0){
          for (t in 1:l) {
            qq <- paste(qq,EnRuls[[i]]$Indicator[[j]]$Parameters[[t]][1,2],sep = ",")
          }
        }
        k <- which(Indo[,21] == Ind)
        if(indslag > 0){
          b <- paste("Ind_",j," <- Lag(Indis(bb = share,FUN = Indo[k,1]",qq, ")[,Indo[k,22]],",indslag,")", sep = "")
        } else {
          b <- paste("Ind_",j," <- Indis(bb = share,FUN = Indo[k,1]",qq, ")[,Indo[k,22]]", sep = "")
        }
        eval(parse(text = b))
      }
      m <- m - 1
      if(EnRuls[[i]]$Math[[m]] == "cross<"){
        for (s in 1:m) {
          qqq <- paste(qqq,EnRuls[[i]]$Math[[s]],"Ind_",s+1,sep = "")
        }
        a1 <- gsub("cross<",">=",qqq)
        b <- paste("c1 <- ",a1,sep = "")
        eval(parse(text = b))
        a2 <- gsub("cross<","<",qqq)
        b <- paste("c2 <- ",a2,sep = "")
        eval(parse(text = b))
        c2 <- lag(c2,1)
        b <- paste("rull_",i," <- (c1 & c2)",sep = "")
        eval(parse(text = b))
      }else if(EnRuls[[i]]$Math[[m]] == "cross>"){
        for (s in 1:m) {
          qqq <- paste(qqq,EnRuls[[i]]$Math[[s]],"Ind_",s+1,sep = "")
        }
        a1 <- gsub("cross>","<=",qqq)
        b <- paste("c1 <- ",a1,sep = "")
        eval(parse(text = b))
        a2 <- gsub("cross>",">",qqq)
        b <- paste("c2 <- ",a2,sep = "")
        eval(parse(text = b))
        c2 <- lag(c2,1)
        b <- paste("rull_",i," <- (c1 & c2)",sep = "")
        eval(parse(text = b))
      }else{
        for (s in 1:m) {
          qqq <- paste(qqq,EnRuls[[i]]$Math[[s]],"Ind_",s+1,sep = "")
        }
        b <- paste("rull_",i," <- (",qqq,")",sep = "")
        eval(parse(text = b))
      }
    }
    q <- "rull_1"
    if(n > 1){
      n <- n - 1
      for (s in 1:n) {
        q <- paste(q,EnRels[[s]],"rull_",s+1,sep = "")
      }
      q <- gsub("OR", " | ", q)
      q <- gsub("ADD", " & ", q)
    }
    b <- paste("BUY_Enter <- (",q,")",sep = "")
    eval(parse(text = b))
  }
  if(as.logical(BUY_Enter[nrow(BUY_Enter), 1])){
    P <- share[nrow(share), "Close"]
    #ReEnter in a position
    if(ReEnterType =="Percentage_Above" | ReEnterType =="Percentage_Below" | ReEnterType =="PriceTick_Above" | ReEnterType =="PriceTick_Below"){
      B <- switch (ReEnterType,
        Percentage_Above = ReEnter <- floor(P * ((100 + ReEnterAmm)/100)),
        Percentage_Below = ReEnter <- floor(P * ((100 - ReEnterAmm)/100)),
        PriceTick_Above = ReEnter <- floor(P + ReEnterAmm),
        PriceTick_Below = ReEnter <- floor(P - ReEnterAmm)
      )
    }
    if(is.null(ExRuls)) {
      ExRuleCon = FALSE
    } else {
      ExRuleCon = TRUE
    }
    if(is.null(StpLst)){
      Stp <- as.numeric(pri * 0)
    }else {
      if(StpLst[1,1] == "Percent"){
        Stp <- as.numeric(floor(P * ((100 - as.numeric(StpLst[1,2]))/100)))
      }else if(StpLst[1,1] == "PriceTick"){
        Stp <- as.numeric(P - as.numeric(StpLst[1,2]))
      } else {
        Stp <- 0
      }
    }
    if(is.null(TkPrft)){
      Prf <- as.numeric(P * 1000)
    }else {
      if(TkPrft[1,1] == "Percent"){
        Prf <- as.numeric(floor(P * ((100 + as.numeric(TkPrft[1,2]))/100)))
      }else if(TkPrft[1,1] == "PriceTick"){
        Prf <- as.numeric(P + as.numeric(TkPrft[1,2]))
      } else {
        Prf <- as.numeric(P * 1000)
      }
    }
    res <- data.frame(Buy = TRUE, Price = P, ReEnter = ReEnter, ExRuleCon = ExRuleCon, TakeProfit = Prf, StopLoss = Stp)
    return(res)
  } else {
    res <- data.frame(Buy = FALSE, Price = 0, ReEnter = 0, ExRuleCon = 0, TakeProfit = 0, StopLoss = 0)
  }
}