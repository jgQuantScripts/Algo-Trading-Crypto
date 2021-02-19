require("RobinHood");require("lubridate");require("quantmod");require("PerformanceAnalytics")
require("httr");require("rvest");require("pbapply")
PASS <- new.env()
assign("username","****",envir = PASS)
assign("password","****",envir = PASS)

# time difference between current time zone and UTC-time
tmDIFF = round(as.numeric(difftime(Sys.time(),
                                   lubridate::force_tz(with_tz(Sys.time(),tz="UTC")),
                                   units = "hours")),0)
# interval = '15second'|'5minute'|'10minute'|'hour'|'day'|'week'
# span     =  'hour'|'day'|'week'|'month'|'3month'|'year'|'5year'
# bounds   =  '24_7'|'extended'|'regular'|'trading'
# 'Regular' is 6 hours a day  |'trading' is 9 hours a day 
# 'extended' is 16 hours a day|'24_7' is 24 hours a day.
# ***********************************************************
#                   get OHLC data: CRYPTO
# ***********************************************************
get_historicals_crypto = function(COIN,interval,span,bounds){
RH = RobinHood(username = PASS$username, password = PASS$password)
# URL and token
url = paste0("https://nummus.robinhood.com/currency_pairs/?symbols=",COIN)
token <- paste("Bearer", RH$tokens.access_token)

# GET call
reqID <- GET(url,
           add_headers("Accept" = "application/json",
                       "Content-Type" = "application/json",
                       "Authorization" = token))

# Format return
reqID <- mod_json(reqID, "fromJSON")
reqID <- as.data.frame(reqID$results)
# extract Crypto ID
cryptoID = reqID[which(reqID$asset_currency$code == COIN),"id"]
# url to get data
url = paste0("https://api.robinhood.com/marketdata/forex/historicals/",cryptoID,
             "/?symbol=",COIN,"&interval=",interval,"&span=",span,"&bounds=",bounds)
token <- paste("Bearer", RH$tokens.access_token)

# GET call
dta <- GET(url,
           add_headers("Accept" = "application/json",
                       "Content-Type" = "application/json",
                       "Authorization" = token
           ))

# Format return
dta2 <- mod_json(dta, "fromJSON")
dta2 <- as.data.frame(dta2$data_points)

dta2$begins_at <- as.POSIXct(as.character(dta2$begins_at), 
                             format="%Y-%m-%dT%H:%M:%SZ",TZ="UTC")
dta2 = dta2[,c("begins_at","open_price",
               "high_price","low_price",
               "close_price")]
logout(RH)
dta2
}
# rbind with latest data
get_latest_crypto_quote = function(COIN,dta2,interval)
{
  RH = RobinHood(username = PASS$username, password = PASS$password)
  rn = get_quote_crypto(RH,COIN)
  toRbind = rn[,c("open_price","high_price","low_price","mark_price")]
  tm = as.POSIXct(last(dta2$begins_at),format="%Y-%m-%d %H:%M:%S")
  tm = get_next_bar(tm,interval)
  toRbind = cbind(tm,toRbind)
  colnames(toRbind) = c("begins_at","open_price","high_price",
                        "low_price","close_price")
  logout(RH)
  rbind(dta2,toRbind)
}
# ***********************************************************
#                   BBand Strategy
# ***********************************************************
get_crypto_bands = function(dta2){
colnames(dta2)[2:5] = c("Open","High","Low","Close")
crypto= xts(dta2[,2:5], order.by = dta2$begins_at)
crypto = reclass(apply(crypto,2,as.numeric),match.to = crypto)
#chartSeries(OHLC(crypto))
#write.zoo(crypto,"~/Desktop/ETH.csv")
Bands =  BBands(HLC(crypto),n = 20,sd = 2)

crypto = cbind(crypto,Bands)
crypto = na.omit(crypto)
crypto = crypto[,-ncol(crypto)]
crypto$sig = NA

# Flat where Close crossed the mavg
crypto$sig[c(FALSE, diff(sign(Cl(crypto) - crypto$mavg), na.pad=FALSE) != 0)] <- 0
#crypto$sig[Cl(crypto) > crypto$up] <- -1 # short when Close is above up
crypto$sig[Cl(crypto) < crypto$dn] <- 1 # long when Close is below dn

# Fill in the signal for other times
crypto$sig <- na.locf(crypto$sig) 
# Now Lag your signal to reflect that you can't trade on the same bar that 
# your signal fires
#crypto$sig <- Lag(crypto$sig)

crypto
}

get_next_bar = function(tm,interval)
{
  if(interval == '15second'){tm = seconds(15) + tm}
  if(interval == '5minute' ){tm = minutes(5) + tm}
  if(interval == '10minute'){tm = minutes(10) + tm}
  if(interval == 'hour'    ){tm = hours(1) + tm}
  if(interval == 'day'     ){tm = days(1) + tm}
  if(interval == 'week'    ){tm = days(7) + tm}
  tm
}
backtest = function(COIN, interval, span,bounds)
{
  # get new data
  dta2 = get_historicals_crypto(COIN = COIN,interval = interval,
                                span=span,bounds = bounds)
  tm = as.POSIXct(last(dta2$begins_at),format="%Y-%m-%d %H:%M:%S")
  # rbind latest quote
  dta2 = get_latest_crypto_quote(COIN=COIN,dta2 = dta2,tm=get_next_bar(tm,interval))
  # calculate Bollinger Bands
  crypto = get_crypto_bands(dta2 = dta2)
  # extract returns
  ROC(Cl(crypto)) * Lag(crypto$sig)
}
# ***********************************************************
#                    Orders
# ***********************************************************
# Send orders determined by signal
RH_orders = function(crypto,ticker,USD)
{
  tmp = last(tail(crypto),3)
  if((as.numeric(tmp$sig[1])==0) & (as.numeric(tmp$sig[2])==1))
  {
    openOrder(tmp=tmp,ticker=ticker,USD=USD)
  }
  if((as.numeric(tmp$sig[1])==1) & (as.numeric(tmp$sig[2])==0))
  {
    closeOrder(tmp=tmp,ticker=ticker)
  }
  if((as.numeric(tmp$sig[1])==0) & (as.numeric(tmp$sig[2])==0))
  {
    cat("\nNothing to Buy/Close")
  }
}
# function to Open orders
openOrder = function(tmp,ticker,USD)
{
    RH = RobinHood(username = PASS$username, password = PASS$password)
    # recent_orders = get_order_history_crypto(RH)
    # recent_orders = subset(recent_orders, recent_orders$symbol == paste0(ticker,"-USD"))
    # recent_orders = recent_orders[1,]
    pos = get_positions_crypto(RH)
    if(!(ticker %in% pos$symbol))
    {
    cat("\nOpening Position...")
    RobinHood::place_order_crypto(RH,symbol = ticker,
                                  type = "limit", # or "market" 
                                  time_in_force = "gtc", # or gtc | ioc | opg
                                  price = as.character(round(as.numeric(tmp$Close[3]),2)),
                                  quantity = as.character(round(USD/as.numeric(tmp$Close[3]),6)),
                                  side = "buy")
    }
    logout(RH)
}
# function to close orders
closeOrder = function(tmp,ticker)
{
   
    RH = RobinHood(username = PASS$username, password = PASS$password)
    recent_orders = get_order_history_crypto(RH)
    recent_orders = subset(recent_orders, recent_orders$symbol == paste0(ticker,"-USD"))
    recent_orders = recent_orders[1,]
    pos = get_positions_crypto(RH)
    if(length(which(pos$symbol == ticker)) != 0)
    {
      cat("\nClosing Position...")
      RobinHood::place_order_crypto(RH,symbol = ticker,
                                  type = "market", # or "market" 
                                  time_in_force = "gtc", # or gtc
                                  price = as.character(round(as.numeric(tmp$Close[3]),2)),
                                  quantity = as.character(recent_orders$cumulative_quantity),
                                  side = "sell")
    }else{
      cat("\n")
    }
    logout(RH)
}
## Create sequence of times
getTMZ = function(TF,tmDIFF)
{
    # Start/End Times
    START <- as.POSIXct(round_date(Sys.time(), paste0(TF," minutes")))
    START  <- START - hours(tmDIFF)   
    END <- START + hours(24)
    # the following line will determine the start times for the algo
    tmz <- seq(START,END, by=paste0("",TF," min"))
    tmz
}
### SLEEP UNTIL MARKET OPENS 
SLEEEP = function(xx,tmDIFF){
  ttt <- tmz[xx] - (Sys.time() - hours(tmDIFF))
  HMS <- attr(ttt,"units")
  tt <- as.numeric(ttt)
  if(HMS == "hours")
  {
    print(paste0("Will now sleep for: ",tt , " hours"));cat("\n")
    print(paste0("STARTING AT: ",tmz[xx]));cat("\n")
    Sys.sleep(tt*60*60)
  }
  if(HMS == "mins")
  {
    print(paste0("Will now sleep for: ",tt , " minutes"));cat("\n")
    print(paste0("STARTING AT: ",tmz[xx]));cat("\n")
    Sys.sleep(tt*60)
  }
  if(HMS == "secs")
  {
    print(paste0("Will now sleep for: ",tt , " seconds"));cat("\n")
    print(paste0("STARTING AT: ",tmz[xx]));cat("\n")
    Sys.sleep(tt)
  }  
}
