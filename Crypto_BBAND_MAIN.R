source("Crypto_BBAND_FUN.R")
### get Time Sequences for Algo
tmz = getTMZ(TF=5,tmDIFF = tmDIFF)
# ***********************************************************************************************
# THE FOLLOWING LINE ENSURES THAT THE ALGO START TIMES ARE GREATER THAN THE CURRENT TIME
# **** IF THE "SCAN" BLOCK STOP/FAILS - RE-RUN LINES: 11-33 *****
tmz <- tmz[tmz>(Sys.time()-hours(tmDIFF))]
# ***********************************************************************************************
#                               ALGO START
# ***********************************************************************************************
SLEEEP(1,tmDIFF = tmDIFF)
SCAN <- pblapply(as.list(2:length(tmz)), function(xx){
  source("Crypto_BBAND_FUN.R")
  COIN = c("ETH","BTC") # <- coins you want to trade go here
  tmp = lapply(as.list(COIN), function(COIN)
  {
  # get new data
  dta2 = get_historicals_crypto(COIN = COIN,interval = "5minute",
                                span="week",bounds = "24_7")
  # rbind latest quote
  dta2 = get_latest_crypto_quote(COIN=COIN,dta2 = dta2,interval = "5minute")
  # calculate Bollinger Bands
  crypto = get_crypto_bands(dta2 = dta2)
  # send orders
  RH_orders(crypto = crypto,ticker=COIN,USD=500)
  # **********************************************************************************************
  # optional
  print(tail(crypto))
  })
  # **********************************************************************************************
  # Sleep until the next bar
  SLEEEP(xx,tmDIFF = tmDIFF)
})


# Use the following lines to extract the returns of this strategy for any Crypto on RH
# bt = backtest(COIN="ETH",interval = "5minute",span = "week",bounds = "24_7")
# charts.PerformanceSummary(bt,geometric = FALSE)
