#####################################################
#
#     BASIC FUNCTIONS TO INTERACT WITH FTX API
#     https://docs.ftx.com/#authentication
#     https://blog.ftx.com/blog/api-authentication/
#
#####################################################

# AUTHENTICATION
# FTX-KEY: Your API key
# FTX-TS: Number of milliseconds since Unix epoch
# FTX-SIGN: SHA256 HMAC of the following four strings, using your API secret, as a hex string: # nolint
#  - Request timestamp (e.g. 1528394229375)
#  - HTTP method in uppercase (e.g. GET or POST)
#  - Request path, including leading slash and any URL parameters but not including the hostname (e.g. /account)
#  - (POST only) Request body (JSON-encoded)
# FTX-SUBACCOUNT (optional): URI-encoded name of the subaccount to use. Omit if not using subaccounts.

library(lubridate) # date time functions
library(digest)    # SHA hashing functions
library(httr)      # rest API call functions returns JSON
library(jsonlite)  # converting JSON objects to R objects
library(dplyr)     # useful "case-when" statements and general data frame manipulation
library(quantmod)  # for the xts finance & time series functions

options(scipen = 99999)

# setwd("C:/Users/kurtc/Google Drive/Crypto")

# Read in the public and secret keys
keys <- if (file.exists("keys.csv")) {
  read.csv("keys.csv", stringsAsFactors = F)
} else "No keyfile found"

checkFTXts <- function() {
  # Get the current FTX Timestamp. Can be used to compare with system timestamp
  ftxts <- GET("https://otc.ftx.com/api/time") %>% cleanUp() # nolint
  ftxts <- ymd_hms(ftxts[[1]]) # nolint
  ftxts <- as.numeric(ftxts)
  return(ftxts)
}

cleanUp <- function(flattenThis, ret = "list") {
  # Given the JSON from the GET call
  # Turn into a list as default, or data frame if "df" specified
  resultText <- content(flattenThis, type = "text", encoding = "UTF-8") # nolint
  resultFlatten <- fromJSON(resultText, flatten = TRUE)
  if (ret == "df")
     resultFlatten <- as.data.frame(resultFlatten, stringsAsFactors = F)

  return(resultFlatten)
}

historicalPrices <- function(marketName = "ETH-PERP",
                             start = "2021-01-01",
                             end = "2021-12-11",
                             resolution = "day") {
  # Function to get historical prices using the FTX API.
  # Returns a xts data frame object
  # Path to get historical prices of ETH-PERP future
  # GET /markets/{market_name}/candles?resolution={resolution}&start_time={start_time}&end_time={end_time}
  # resolution = window length in seconds. options: 15, 60, 300, 900, 3600, 14400, 86400 (day),
  # or any multiple of 86400 up to 30*86400
  # start & end time are optional

  thisResolution <- case_when (
    resolution == "15 seconds" ~ 15,
    resolution == "minute" ~ 60,
    resolution == "5 minutes" ~ 300,
    resolution == "15 minutes" ~ 900,
    resolution == "1 hour" ~ 3600,
    resolution == "4 hours" ~ 14400,
    resolution == "day" ~ 86400,
    TRUE ~ 86400
  )

  # Input is a date, add the time for the call to the API
  startTime <- as.numeric(ymd_hms(paste0(start, "22:00:00"))) # nolint
  endTime <- as.numeric(ymd_hms(paste0(end, "22:00:00")))
  path <- paste0("https://ftx.com/api/markets/",
                 marketName,
                 "/candles?resolution=",
                 thisResolution,
                 "&start_time=",
                 startTime,
                 "&end_time=",
                 endTime)
  rawResult <- cleanUp(GET(path), ret = "df")

  # Convert to xts and return
  rawResult$success <- NULL
  colnames(rawResult) <- c("startTime", "time", "open", "high", "low", "close", "volume")

  return(rawResult)
}


getMarkets <- function() {
  # Get all current FTX markets
  endPoint <- "https://ftx.com/api/markets"
  rawResult <- GET(endPoint)
  return(cleanUp(rawResult, "df"))
}

getAccount <- function(keys) {
  # Supply the public and secret keys for the account in a dataframe
  # Reproduce the FTX example in Python shown here: https://blog.ftx.com/blog/api-authentication/
  endPoint <- "https://ftx.com/api/account"
  ts <- floor(as.numeric(now(tzone = "UTC")) * 1000)  # time since epoch in milliseconds # nolint

  # Build the signature payload
  signature_payload <- paste0(ts, "GET/api/account")

  # Apply SHA 256 to payload using secret key
  # Returns encrypted signature as hex
  # Read only public key "BGTgjHW9cVpDWhHwtFj8MIy77PnjaH1gTz4FX8rF"
  # Read only secret key "sPJvyqPayC6hnezcXgJ9GT30IS4XXSsDlRivA_IP"
  signature <- hmac(key = keys$secret,
                    object = signature_payload,
                    algo = "sha256")

  # Make the API call
  rawResult <- GET(endPoint, add_headers(
    "FTX-KEY" = keys$public,
    "FTX-SIGN" = signature,
    "FTX-TS" = as.character(ts)
  ))
  
  # Tidy up and return as a list
  return(cleanUp(rawResult, ret = "list"))
}

accountPositions <- function(accountInfoRaw) {
  # From the raw list returned by the FTX API in the getAccount function
  # return all the current open positions
  
  return(accountInfoRaw$result$positions %>% filter(size > 0))
}

placeOrder <- function(market = "", side = "buy",
                       price = 0, type = "limit", size = 0,
                       reduceOnly = FALSE, ioc = FALSE, postOnly = FALSE,
                       clientId = "NULL", rejectOnPriceBand = FALSE) {

  # https://docs.ftx.com/#place-order
  # build the body of the POST request
  prBody <- paste0('{"market": "', market,
                   '", "side": "', side,
                   '", "price": ', price,
                   ', "type": "', type,
                   '", "size": ', size,
                   '}')
  #"\", \"reduceOnly\": ", reduceOnly,
  #", \"ioc\": ", ioc,
  #", \"postOnly\": ", postOnly,
  #", \"clientId\": ", clientId,
  #", \"rejectOnPriceBand\": ", rejectOnPriceBand,

  # Requires authentication
  endPoint <- "https://ftx.com/api/orders"
  ts <- floor(as.numeric(now(tzone = "UTC")) * 1000)  # time since epoch in milliseconds # nolint

  # Build the signature payload
  signature_payload <- paste0(ts, "POST/api/orders", prBody)

  # Send the order
  signature <- hmac(keys$secret,
                    object = signature_payload,
                    algo = "sha256")

  prResponse <- POST(url = endPoint, add_headers(
    "FTX-KEY" = keys$public,
    "FTX-SIGN" = signature,
    "FTX-TS" = as.character(ts)
  ), body = prBody, content_type_json())

  return(prResponse)
}

deleteOrder <- function(orderId = 96057318782) {

  # Not currently working

  # https://docs.ftx.com/#place-order
  # build the body of the POST request
  # drBody <- paste0('{', orderId,'}')
  #"\", \"reduceOnly\": ", reduceOnly,
  #", \"ioc\": ", ioc,
  #", \"postOnly\": ", postOnly,
  #", \"clientId\": ", clientId,
  #", \"rejectOnPriceBand\": ", rejectOnPriceBand,

  # Requires authentication
  endPoint <- paste0("https://ftx.com/api/orders/", orderId)
  ts <- floor(as.numeric(now(tzone = "UTC")) * 1000)  # time since epoch in milliseconds # nolint

  # Build the signature payload
  signature_payload <- paste0(ts, "DELETE/api/orders/", orderId)

  # Delete the order
  signature <- hmac(key = keys$secret,
                    object = signature_payload,
                    algo = "sha256")

  drResponse <- DELETE(url = endPoint, add_headers(
    "FTX-KEY" = keys$public,
    "FTX-SIGN" = signature,
    "FTX-TS" = as.character(ts)
  ), body = as.character(orderId), content_type_json())

  return(prResponse)
}


getOpenOrders <- function(market = "FTM-PERP") {

  endPoint <- paste0("https://ftx.com/api/orders?market=", market)
  ts <- floor(as.numeric(now(tzone = "UTC")) * 1000)  # time since epoch in milliseconds

  # Build the signature payload
  signature_payload <- paste0(ts, "GET/api/orders?market=", market)

  # Apply SHA 256 to payload using secret key
  # Returns encrypted signature as hex
  # Read only public key "BGTgjHW9cVpDWhHwtFj8MIy77PnjaH1gTz4FX8rF"
  # Read only secret key "sPJvyqPayC6hnezcXgJ9GT30IS4XXSsDlRivA_IP"
  signature <- hmac(key = keys$secret,
                    object = signature_payload,
                    algo = "sha256")

  # Make the API call
  rawResult <- GET(endPoint, add_headers(
    "FTX-KEY" = keys$public,
    "FTX-SIGN" = signature,
    "FTX-TS" = as.character(ts)
  ))

  # Tidy up and return as a list
  return(cleanUp(rawResult, ret="list"))
}

createSubAccount <- function(nickname = "sub2") {
  prBody <- paste0('{"nickname": "', nickname, '"}')

  endPoint <- "https://ftx.com/api/subaccounts"
  ts <- floor(as.numeric(now(tzone = "UTC")) * 1000)  # time since epoch in milliseconds

  # Build the signature payload
  signature_payload <- paste0(ts, "POST/api/subaccounts", prBody)

  # Send the order
  signature <- hmac(key = keys$secret,
                    object = signature_payload,
                    algo = "sha256")

  prResponse <- POST(url = endPoint, add_headers(
    "FTX-KEY" = keys$public,
    "FTX-SIGN" = signature,
    "FTX-TS" = as.character(ts)
  ), body = prBody, content_type_json())

  return(prResponse)
}

deleteSubAccount <- function(nickname = "sub2") {
  prBody <- paste0('{"nickname": "', nickname, '"}')

  endPoint <- "https://ftx.com/api/subaccounts"
  ts <- floor(as.numeric(now(tzone = "UTC")) * 1000)  # time since epoch in milliseconds

  # Build the signature payload
  signature_payload <- paste0(ts, "DELETE/api/subaccounts", prBody)

  # Send the order
  signature <- hmac(key = keys$secret,
                    object = signature_payload,
                    algo = "sha256")

  prResponse <- DELETE(url = endPoint, add_headers(
    "FTX-KEY" = keys$public,
    "FTX-SIGN" = signature,
    "FTX-TS" = as.character(ts)
  ), body = prBody, content_type_json())

  return(prResponse)
}

##########################################################################
#                                                                        #
#  Functions related to trading rules                                    #
#                                                                        #
##########################################################################

pxVsSMA <- function(market = "ETH-PERP", trendPeriod = 40) {
  # Return TRUE if last price is > Simple Moving Average given in trendPeriod
  # Otherwise returns FALSE.
  # Also returns the market name
  endDate <- today()  # Use current date & time to test for stop loss
  startDate <- endDate - days(trendPeriod + 1)
  px <- historicalPrices(marketName = market, start = startDate,
                         end = endDate, resolution = "day")
  if (nrow(px) >= trendPeriod) {
    # Have enough rows to calculate SMA
    px$SMA <- SMA(px[, "close"], n = trendPeriod)
    res <- data.frame(PxAboveSMA = last(px$close) > last(px$SMA),
                      Market = market, stringsAsFactors = F)
  } else {
    res <- data.frame(PxAboveSMA = "Data",
                                   Market = market, stringsAsFactors = F)
  }

  print(paste(endDate, market))
  return(res)
}

anyStopsTriggered <- function(positions, trendPeriod = 40) {
  # positions a list of position names ("future") and direction i.e buy/sell ("side")
  # trendPeriod is number of days to test in SMA

  # Go through each position & test if above / below SMA
  results <- lapply(positions$future, pxVsSMA, trendPeriod = trendPeriod)

  # Turn list of results into a data frame
  resultsDF <- data.frame(matrix(unlist(results),
                          nrow=length(results), byrow=T),
                   stringsAsFactors = F)
  names(resultsDF) <- c("PxAboveSMA", "MarketName")
  resultsDF$PxAboveSMA <- as.logical(resultsDF$PxAboveSMA)

  # Join the side
  resultsDF <- left_join(positions, resultsDF, by = c("future" = "MarketName"))

  # Test to see if stop loss is triggered
  resultsDF$StopTriggered <- case_when(
                               resultsDF$PxAboveSMA & resultsDF$side == "buy" ~ FALSE,
                               ! resultsDF$PxAboveSMA & resultsDF$side == "sell" ~ FALSE,
                               TRUE ~ TRUE
  )
  return(resultsDF)
}

accountStops <- function(keys, trendPeriod = 40) {
  return(getAccount(keys = keys) %>%
         accountPositions() %>%
         anyStopsTriggered(trendPeriod = trendPeriod))
}

