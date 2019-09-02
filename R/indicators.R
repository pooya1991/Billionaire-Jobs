library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(dplyr)
library(MASS)
load("data/Indo.rda")
time <- "time"
weekday <- "weekday"
boyh <- "both"
RealTime <- function(x, t) {
  switch (x,
          time = result <- strftime(t,"%H:%M:%OS"),
          weekday = result <- strftime(t,"%A"),
          both = result <- strftime(t,"%H:%M:%OS %A")
  )
  result
}
Time <- function(vaght = "", rooz = "",HLC){
  result <- paste(vaght,rooz,sep = "")
  return(result)
}
Low <- "Low"
High <- "High"
Open <- "Open"
Close <- "Close"
HL <- "HL"
HLC <- "HLC"
HLCC <- "HLCC"
Highest <- function(OHLC,Interval,COLUMN){
  switch (COLUMN,
          Close = result <- runMax(OHLC[,4],Interval),
          Open = result <- runMax(OHLC[,1], Interval),
          High = result <- runMax(OHLC[,2], Interval),
          Low = result <- runMax(OHLC[,3], Interval),
          HL = result <- runMax((OHLC[,2] + OHLC[,3])/2, Interval),
          HLC = result <- runMax((OHLC[,2] + OHLC[,3] + OHLC[,4])/3, Interval),
          HLCC = result <- runMax((OHLC[,2] + OHLC[,3]+ 2*OHLC[,4])/4, Interval)
  )
  result
}
Lowest <- function(OHLC,Interval,COLUMN){
  switch (COLUMN,
          Close = result <- runMin(OHLC[,4],Interval),
          Open = result <- runMin(OHLC[,1], Interval),
          High = result <- runMin(OHLC[,2], Interval),
          Low = result <- runMin(OHLC[,3], Interval),
          HL = result <- runMin((OHLC[,2] + OHLC[,3])/2, Interval),
          HLC = result <- runMin((OHLC[,2] + OHLC[,3] + OHLC[,4])/3, Interval),
          HLCC = result <- runMin((OHLC[,2] + OHLC[,3]+ 2*OHLC[,4])/4, Interval)
  )
  result
}
# Ichimoku Indicator Function
ichimoku <- function(HLC, nFast=9, nMed=26, nSlow=52) {
  turningLine <- (runMax(Hi(HLC), nFast)+runMin(Lo(HLC), nFast))/2
  baseLine <- (runMax(Hi(HLC), nMed)+runMin(Lo(HLC), nMed))/2
  spanA <- lag((turningLine+baseLine)/2, nMed)
  spanB <- lag((runMax(Hi(HLC), nSlow)+runMin(Lo(HLC), nSlow))/2, nMed)
  plotSpan <- lag(Cl(HLC), nMed) #for plotting the original Ichimoku only
  laggingSpan <- lag(Cl(HLC), nMed)
  lagSpanA <- lag(spanA, nMed)
  lagSpanB <- lag(spanB, nMed)
  out <- cbind(turnLine=turningLine, baseLine=baseLine, spanA=spanA, spanB=spanB, plotSpan=plotSpan, laggingSpan=laggingSpan, lagSpanA, lagSpanB)
  colnames(out) <- c("turnLine", "baseLine", "spanA", "spanB", "plotLagSpan", "laggingSpan", "lagSpanA","lagSpanB")
  return (out)
}
# Indicators function
Indis <- function(bb,FUN,n,m,p,q){
  switch(FUN,
         ClosePrice = result <- bb[,4],
         HighPrice = result <- bb[,2],
         LowPrice = result <- bb[,3],
         OpenPrice = result <- bb[,1],
         Volume = result <- bb[,5],
         ADX = result <- ADX(bb[,c(2,3,4)],n),
         Aroon = result <- aroon(bb[,c(2,3)],n),
         aroon = result <- aroon(bb[,c(2,3)],n),
         ATR = result <- ATR(bb[,c(2,3,4)],n),
         BBands = result <- BBands(bb[,c(2,3,4)],n = n,sd = m,maType = "SMA"),
         CCI = result <- CCI(bb[,c(2,3,4)],n = n,maType = "SMA",m),
         chaikinAD = result <- chaikinAD(bb[,c(2,3,4)],bb[,5]),
         chaikinVolatility = result <- chaikinVolatility(bb[,c(2,3)],n),
         CLV = result <- CLV(bb[,c(2,3,4)]),
         CMF = result <- CMF(bb[,c(2,3,4)],bb[,5],n),
         CMO = result <- CMO(bb[,4],n),
         DonchianChannel = result <- DonchianChannel(bb[,c(2,3)],n),
         DPO = result <- DPO(bb[,4],n,shift = m,maType = "SMA"),
         DVI = result <- DVI(bb[,4],n),
         EMV = result <- EMV(bb[,c(2,3)],bb[,5],n,maType = "SMA"),
         Highest = result <- Highest(OHLC = bb[,c(1:4)],Interval = m,COLUMN = n),
         ichimoku = result <- ichimoku(HLC = bb[,c(2,3,4)],nFast = n,nMed = m,nSlow = p),
         KST = result <- KST(bb[,4],n = c(n,n,n,floor((3 * n) / 2)),nROC = c(n, n + floor(n/2),2 * n, 2*n + floor(n/2)),nSig = m,maType = "SMA"),
         Lowest = result <- Lowest(OHLC = bb[,c(1:4)],Interval = m,COLUMN = n),
         lags = result <- lag(bb[,m],n),
         MACD = result <- MACD(bb[,4],n,m,p,"SMA"),
         MFI = result <- MFI(bb[,c(2,3,4)],bb[,5],n),
         momentum = result <- momentum(bb[,4],n),
         OBV = result <- OBV(bb[,4],bb[,5]),
         Pbands = result <- PBands(bb[,4],n,sd = m,maType = "SMA"),
         RealTime = result <- RealTime(x = n, t = index(bb)),
         ROC = result <- ROC(bb[,4],n),
         rollSFM = result <- rollSFM(bb[,4],n),
         RSI = result <- RSI(bb[,4],n,"SMA"),
         SAR = result <- SAR(bb[,c(2,3)],accel = c(n,m)),
         stoch = result <- stoch(bb[,c(2,3,4)],nFastK = n,nFastD = m,nSlowD = p,maType = "SMA"),
         SMI = result <- SMI(HLC,n = n,nFast = m,nSlow = p,nSig = q),
         TDI = result <- TDI(bb[,4],n,m),
         Time = result <- Time(vaght = n, rooz = m, HLC = bb),
         TRIX = result <- TRIX(bb[,4],n,m,"SMA"),
         ultimateOscillator = result <- ultimateOscillator(bb[,c(2,3,4)],n = c(5, 10, 20), wts = c(4, 2, 1)),
         VHF = result <- VHF(bb[,4],n),
         volatility = result <- volatility(bb[,c(7,2,3,4)],n),
         williamsAD = result <- williamsAD(bb[,c(2,3,4)]),
         WPR = result <- WPR(bb[,c(2,3,4)],n),
         ZigZag <- result <- ZigZag(bb[,c(2,3)],n),
         SMA = result <- SMA(bb[,4],n),
         EMA = result <- EMA(bb[,4],n),
         DEMA = result <- DEMA(bb[,4],n,m),
         WMA = result <- WMA(bb[,4],n),
         EVWMA = result <- EVWMA(bb[,4],n),
         ZLEMA = result <- ZLEMA(bb[,4],n),
         VWAP = result <- VWAP(bb[,4],bb[,5],n),
         VMA = result <- VMA(bb[,4],n),
         HMA = result <- HMA(bb[,4],n),
         ALMA = result <- ALMA(bb[,4],n,m,p)
  )
  result
}