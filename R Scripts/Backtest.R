#################################################################
#                                                               #
#       BACKTESTING CRYPTO RETURNS USING FTX PERP PRICES        #
#                                                               #
#################################################################

library(lubridate) # date time functions used when preparing API calls
library(digest) # SHA hashing functions used when preparing API calls
library(httr) # rest API call functions returns JSON
library(jsonlite) # converting JSON objects to R objects
library(dplyr) # useful "case-when" statements and general data frame manipulation
library(tidyr) # for pivoting of dataframes
library(quantmod) # for the TTR, xts finance & time series functions
library(PerformanceAnalytics) # for the backtesting
library(ggplot2) # Visualise the analysis
library(mailR) # To send an email

options(scipen = 99999)

# setwd("C:/Users/kurtc/Google Drive/Crypto")

#################################################################
#                GET HISTORICAL PRICES & VOLUME                 #
#################################################################

cleanUp <- function(flattenThis, ret = "list") {
  # Given the JSON from the GET call
  # Turn into a list as default, or data frame if "df" specified
  resultText <- content(flattenThis, type = "text", encoding = "UTF-8")
  resultFlatten <- fromJSON(resultText, flatten = TRUE)
  if (ret == "df") resultFlatten <- as.data.frame(resultFlatten, stringsAsFactors = F)

  return(resultFlatten)
}

historicalPrices <- function(marketName = "ETH-PERP",
                             start = "2021-01-01 22:00:00",
                             end = "2021-12-11 22:00:00",
                             resolution = "day") {
  # Function to get historical prices using the FTX API.
  # Returns a xts data frame object
  # Path to get historical prices of ETH-PERP future
  # GET /markets/{market_name}/candles?resolution={resolution}&start_time={start_time}&end_time={end_time}
  # resolution = window length in seconds. options: 15, 60, 300, 900, 3600, 14400, 86400 (day),
  # or any multiple of 86400 up to 30*86400
  # start & end time are optional

  thisResolution <- case_when(
    resolution == "day" ~ 86400,
    resolution == "hour" ~ 3600,
    resolution == "4hour" ~ 14400,
    TRUE ~ 86400
  )

  startTime <- as.numeric(ymd_hms(start))
  endTime <- as.numeric(ymd_hms(end))
  path <- paste0("https://ftx.com/api/markets/", marketName, "/candles?resolution=", thisResolution, "&start_time=", startTime, "&end_time=", endTime)
  rawResult <- cleanUp(GET(path), ret = "df")

  # Convert to xts and return
  rawResult$success <- NULL
  colnames(rawResult) <- c("startTime", "time", "open", "high", "low", "close", "volume")
  # row.names(rawResult) <- rawResult$startTime
  return(rawResult)
}

getMarkets <- function() {
  # Get all the PERP market names listed on FTX
  path <- "https://ftx.com/api/markets"
  final <- GET(path) %>% cleanUp(ret = "df")
  final <- final[grep("PERP", final$result.name), ] # Extract only rows which have "PERP" in their name
  return(final)
}

#################################################################
#                    APPLY TRADING RULES                        #
#################################################################

# Breakout trading rule, mark where breakouts have occurred
breakoutType <- function(priceSeries, trendPeriod = 16) {
  # Given a vector of closing prices & trendperiod, will return a vector of the
  # same length indicating at each point if a breakout occured.
  # "High", "Low" or NA

  dataPoints <- length(priceSeries)
  type <- FALSE
  # Test have enough data in the price series to test against period specified
  if (dataPoints > trendPeriod) {
    startPoint <- dataPoints - trendPeriod
    if (priceSeries[dataPoints] > seriesHi(priceSeries[startPoint:(dataPoints - 1)])) {
      type <- "High"
    } else if (priceSeries[dataPoints] < seriesLo(priceSeries[startPoint:(dataPoints - 1)])) {
      type <- "Low"
    }
  }
  return(type)
}

markBreakouts <- function(closePx, trendPeriod = 16) {
  # Given a vector of closing prices, return an equal length vector
  # which identifies if the row is a breakout over the period
  # supplied in the trendPeriod argument.
  # Returning "High", "Low", or NA = none
  highLow <- sapply(1:length(closePx), function(row) breakoutType(closePx[1:row], trendPeriod = trendPeriod))
  return(highLow)
}

stopLossTrigger <- function(pxSeries, trendPeriod = 16) {
  # Identify a period which would have triggered a stop loss, any one of:
  #   1) Has the breakOut signal flipped?
  #   2) ClosePx < trendPeriod SMA
  #   3) Return > 2 standard deviations of the return vs previous trendPeriod
  #   4) Last bar True Range > 2 * ATR of the previous trendPeriod
  #
  # Inputs are
  #    - price series containing closing prices, % return between periods
  #      and breakout signals
  #    - breakout testing for
  #    - trend period to test over

  # 1) Has the breakOut signal flipped?
  testBreakOuts <- data.frame(previous = lag(pxSeries$breakOut), current = pxSeries$breakOut, stringsAsFactors = F)
  pxSeries$stopSignalFlipped <- case_when(
    # case_when used since can be vectorised, in place of if else
    (testBreakOuts$previous == "Low" & testBreakOuts$current == "High") |
      (testBreakOuts$previous == "High" & testBreakOuts$current == "Low") ~ TRUE, # Test for a flip
    TRUE ~ FALSE # Otherwise return FALSE for all other combinations
  )

  # 2) ClosePx < trendPeriod SMA

  pxSeries$stopSMALong <- pxSeries$close < pxSeries$SMA # Long stop signal triggered
  pxSeries$stopSMAShort <- pxSeries$close > pxSeries$SMA # Short stop signal triggered

  # 3) The current Return > 2 SD of returns prior over 'trendPeriod'
  pxSeries$stopReturnLong <- case_when(
    # If long, test for negative return which > 2 * 'trendPeriod' SD of the return
    (pxSeries$Return1 < 0) & (abs(pxSeries$Return1) / pxSeries$Sd1 > 2) ~ TRUE,
    TRUE ~ FALSE
  )

  pxSeries$stopReturnShort <- case_when(
    # If short, test for positive return which > 2 * 'trendPeriod' SD of the return
    (pxSeries$Return1 > 0) & (abs(pxSeries$Return1) / pxSeries$Sd1 > 2) ~ TRUE,
    TRUE ~ FALSE
  )

  # 4) Period ATR > 2 Std dev of ATR over previous "trendPriod"
  pxSeries$stopATRLong <- case_when(
    # If long, test for negative return and 2 * SD of ATR
    (pxSeries$Return1 < 0) & (pxSeries$TR / pxSeries$ATRLag > 2) ~ TRUE,
    TRUE ~ FALSE
  )

  pxSeries$stopATRShort <- case_when(
    # If short, test for positive return and 2 * SD of ATR
    (pxSeries$Return1 > 0) & (pxSeries$TR / pxSeries$ATRLag > 2) ~ TRUE,
    TRUE ~ FALSE
  )

  pxSeries$StopLong <- pxSeries$stopSMALong #| pxSeries$stopReturnLong | pxSeries$stopATRLong # Combine all the Long Stop Signals
  pxSeries$StopLong[is.na(pxSeries$StopLong)] <- FALSE # Mark NAs as FALSE

  pxSeries$StopShort <- pxSeries$stopSMAShort #| pxSeries$stopReturnShort | pxSeries$stopATRShort # Combine all the Long Stop Signals
  pxSeries$StopShort[is.na(pxSeries$StopShort)] <- FALSE # Mark NAs as FALSE

  ######### TESTS ###########

  # Average True Range

  # VWAP

  return(pxSeries)
}

addIndicatorsDF <- function(pxSeries, trendPeriod = 16) {
  # Given a regular data frame containing "Date", "Open", "High", "Low", "Close", "Volume"
  # Add various additional indicators to use for back testing

  # Add breakouts
  pxSeries$breakOut <- markBreakouts(pxSeries$close, trendPeriod = trendPeriod)

  # Add Simple Moving Average from TTR
  pxSeries$SMA <- SMA(pxSeries[, "close"], n = trendPeriod)

  # Distance of the current price to the moving average
  pxSeries$distanceMA <- abs(pxSeries$close - pxSeries$SMA)

  # Add VWAP from TTR
  pxSeries$VWAP <- VWAP(price = pxSeries[, "close"], volume = pxSeries[, "volume"], n = trendPeriod)

  # Add Signal to Noise Ratio (SNR) from TTR package
  # Using as.numeric to convert a single column to a vector - function calls often return a list
  pxSeries$SNR <- SNR(pxSeries[, c("high", "low", "close")], n = trendPeriod)

  # Delta of the volume over the trend period
  pxSeries$VolumeDelta <- as.numeric(Delt(pxSeries[, "volume"], k = trendPeriod))

  # Add Average True Range & Std Dev of the ATR over the trend period
  tr <- ATR(pxSeries[, c("high", "low", "close")], n = trendPeriod)
  pxSeries$TR <- tr[, "tr"] # Extract the True Range component
  pxSeries$ATR <- tr[, "atr"] # Extract the Average TR component
  # For Std Dev, shift column down using 'lag', so current ATR can be compared to previous SD (not including current observation)
  pxSeries$ATRLag <- lag(pxSeries$ATR)
  pxSeries$SdATR <- runSD(pxSeries[, "ATRLag"], n = trendPeriod)

  # Calculate different price returns over various periods
  pxSeries$Return1 <- as.numeric(Delt(pxSeries[, "close"], k = 1))
  pxSeries$Return1Lag <- lag(pxSeries$Return1)
  pxSeries$Return2 <- as.numeric(Delt(pxSeries[, "close"], k = 2))
  pxSeries$Return3 <- as.numeric(Delt(pxSeries[, "close"], k = 3))
  pxSeries$Return4 <- as.numeric(Delt(pxSeries[, "close"], k = 4))
  pxSeries$Return5 <- as.numeric(Delt(pxSeries[, "close"], k = 5))
  pxSeries$Return6 <- as.numeric(Delt(pxSeries[, "close"], k = 6))
  pxSeries$Return7 <- as.numeric(Delt(pxSeries[, "close"], k = 7))

  # Standard Deviation of the returns

  # Shift column down using 'lag', so current return can be compared to previous SD (not including current observation)
  pxSeries$Sd1 <- runSD(lag(pxSeries[, "Return1"]), n = trendPeriod)
  pxSeries$Sd2 <- runSD(pxSeries[, "Return2"], n = trendPeriod)
  pxSeries$Sd3 <- runSD(pxSeries[, "Return3"], n = trendPeriod)
  pxSeries$Sd4 <- runSD(pxSeries[, "Return4"], n = trendPeriod)
  pxSeries$Sd5 <- runSD(pxSeries[, "Return5"], n = trendPeriod)
  pxSeries$Sd6 <- runSD(pxSeries[, "Return6"], n = trendPeriod)
  pxSeries$Sd7 <- runSD(pxSeries[, "Return7"], n = trendPeriod)

  return(pxSeries)
}

addIndicatorsXTS <- function(pxSeries, trendPeriod = 16) {
  # Applies indicators to an XTS dataframe

  # Add Signal to Noise Ratio (SNR) from TTR package
  # Using as.numeric to convert a single column to a vector - function calls often return a list
  pxSeries$SNR <- SNR(pxSeries[, c("high", "low", "close")], n = trendPeriod)

  # Add Simple Moving Average from TTR
  # pxSeries$SMA <- SMA(pxSeries[,"close"], n = trendPeriod)

  # Delta of the volume over the trend period
  pxSeries$VolumeDelta <- as.numeric(Delt(pxSeries[, "volume"], k = trendPeriod))

  return(pxSeries)
}

#################################################################
#                   TOOLS TO VISUALISE &                        #
#                   CALCULATE THE RETURNS                       #
#################################################################

onlyBreakouts <- function(mkts, start = "2021-08-01 22:00:00", end = now(), resolution = "day", trendPeriod = 40) {

  # Given a dataframe with a list of markets, return a dataframe with all the markets with a
  #  high or low breakout. Include all historical prices, so they can be used to add indicators
  # Skips markets which do not have enough historical data and returns these as a character vector of names

  resultDF <- data.frame()
  noData <- ""
  dayInterval <- floor(as.numeric(ymd_hms(end) - ymd_hms(start)))

  for (thisMktName in mkts$result.name) {
    # Get the mkt
    print(thisMktName)
    px <- historicalPrices(marketName = thisMktName, start = start, end = end, resolution = resolution)

    # Check if number of rows greater than number of days between dates specified
    if (nrow(px) < dayInterval) {
      # Not enough rows, add the name to list of 'fails' and go to start of loop
      noData <- c(noData, thisMktName)
      next
    }

    # Add indicators, including breakout
    pxSeries <- addIndicatorsDF(pxSeries = px, trendPeriod = trendPeriod) %>% stopLossTrigger(trendPeriod = trendPeriod)

    # Enough rows so test if breakout occurred
    if (last(pxSeries$breakOut) == "High" | last(pxSeries$breakOut) == "Low") {
      pxSeries$market <- thisMktName # Identify which market
      resultDF <- rbind(resultDF, pxSeries) # Bind it to the larger dataframe
    }
  }

  return(list(results = resultDF, noData = noData))
}

breakOutReturn <- function(pxSeries, type = "Low") {
  # Given pxSeries containing
  # startTime - timestamp of the bar breakout observation
  # close - closing price of the row observation
  # breakOut - signal ("High" or "Low") of the row observation
  # StopLong - signal (TRUE or FALSE) if a long stop been triggered
  # StopShort - signal (TRUE or FALSE) if a short stop has been triggered
  # Type - Specify what type of return the function should calculate for
  #
  # Returns a dataframe on each row
  # startTime - timestamp of the breakout
  # breakOut type
  # open - closing price of the bar at point of breakout
  # close - closing price of bar at point of stop
  # closeTime - timestamp of the bar where stop observed
  pctRet <- data.frame(stringsAsFactors = F)
  lookForThisColumn <- ifelse(type == "Low", "StopShort", "StopLong")

  r <- 1
  while (r < nrow(pxSeries)) {
    # Go through each row, look for a low breakout
    if (pxSeries[r, "breakOut"] == type) {
      # Found one so calculate the return, log the date/time and opening price
      thisRet <- data.frame(
        openTime = pxSeries[r, "startTime"],
        open = pxSeries[r, "close"],
        distanceMA = pxSeries[r, "distanceMA"], stringsAsFactors = F
      )
      while (
        pxSeries[r, lookForThisColumn] != TRUE & r < nrow(pxSeries)) {
        r <- r + 1
      }
      # Ends when a stop signal is found or end of the series reached
      # log the closing price
      thisRet <- cbind(thisRet, close = pxSeries[r, "close"], closeTime = pxSeries[r, "startTime"])

      # log this occurance
      pctRet <- rbind(pctRet, thisRet)
    }

    # Look for the next low breakout
    r <- r + 1
  }

  return(pctRet)
}

# px <- historicalPrices(marketName = "RUNE-PERP", start = "2021-01-01 01:00:00", end = now(), resolution = "day")
# pxSeries <- addIndicatorsDF(px, trendPeriod = 40) %>% stopLossTrigger(trendPeriod = 40)

# Plot coloured points where breakouts have occured
# p <- ggplot(pxSeries, aes(x=startTime, y=close, colour = breakOut, size = Return1)) + geom_point() +
#      theme(axis.text.x = element_text(angle = 90, hjust = 0.1, vjust = 0.5, size = 7)) +
#      scale_x_discrete(breaks = pxSeries$startTime[c(TRUE, rep(FALSE, nrow(pxSeries) / 40))])

# Plot vertical lines where Short Stop
# ss <- p + geom_vline(xintercept = pxSeries[pxSeries$StopShort, "startTime"])

# Plot vertical lines where Long Stop
# ls <- p + geom_vline(xintercept = pxSeries[pxSeries$StopLong, "startTime"])

# Quantmod plots
# px <- pxSeries
# row.names(px) <- date(px$startTime)
# chartSeries(px[, c("open", "high", "low", "close")])
# addSMA(n=16)
# stops <- px[!is.na(px$StopLong), c("open", "high", "low", "close")]
# addLines(v=stops)

# Get the position sizes based on USD 60 exposure and distance from MA as volatility sizing
# by_market <- resultDF %>% group_by(market) %>% select(distanceMA, breakOut) %>% slice_tail() %>% mutate(stake = 60 / (distanceMA))

# shortPct <- pxSeries %>% select(startTime, close, breakOut, StopShort, distanceMA) %>%
#  breakOutReturn(type = "Low") %>%
#  mutate(pctReturn = (open - close) * 100 / open,
#        stake = 60 / distanceMA,
#        notional = open * stake,
#        pnl = pctReturn * notional / 100,
#        type = "Low")

# longPct <- pxSeries %>% select(startTime, close, breakOut, StopLong, distanceMA) %>%
#  breakOutReturn(type = "High") %>%
#  mutate(pctReturn = (close - open) * 100 / open,
#         stake = 60 / distanceMA,
#         notional = open * stake,
#         pnl = pctReturn * notional / 100,
#         type = "High")

# pnl <- rbind(longPct, shortPct)

# sum(pnl[,"pnl"])

############### SAVE A FILE OF BREAKOUTS AND SEND AS AN EMAIL ATTACHMENT ########################

yesterday <- ymd_hms(paste(today(), "22:00:00"))
fromDate <- yesterday - days(x = 150)
mkts <- getMarkets()

# read.csv("C:/Users/KC Framework Laptop/My Drive/Crypto/FTX_Markets_202205.csv",
#  stringsAsFactors = F
# ) # Available markets

# Get breakouts & add all necessary indicators
res <- onlyBreakouts(mkts,
  start = fromDate,
  end = yesterday,
  trendPeriod = 40
)

# Add a condition when no breakouts detected
resultDF <- res$result # Get the dataframe part of the result
fileAttach <- resultDF %>%
  group_by(market) %>% # Summarise breakouts
  select(distanceMA, breakOut) %>%
  slice_tail() %>%
  mutate(stake = 60 / (distanceMA))

# Save as a file to be attached to the email
write.csv(fileAttach,
  paste0("Breakouts/", Sys.Date(), " DailyBreakouts.csv"),
  row.names = F
)
