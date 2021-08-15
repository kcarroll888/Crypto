#################################################
#
#     Weekly Trend Following Model for Crypto
#
#     Find breakouts assessed over the period
#     of weeks, from the identified universe
#
#      1. Get necessary closing prices
#      2. For each token calculate
#           - if there's breakout
#           - average true range
#           - where the stop loss should be
#
#################################################

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)

setwd("C:/Users/kurtc/Google Drive/Crypto")
options(scipen = 999)

# Function which accesses the endpoint at CoinGecko and returns a data frame
getAPI <- function(url) {
  # Call the API endpoint
  rawResult <- GET(url = url)
  resultText <- content(rawResult, type = "text", encoding = "UTF-8")
  # Extract the content out of the JSON format
  resultFlatten <- fromJSON(resultText, flatten = TRUE)
  # Turn into data frame
  resultDF <- as.data.frame(resultFlatten, stringsAsFactors = F)
  return(resultDF)
}

tidyClose <- function(closePx, retType="close") {
  # closePx data frame the return from coingecko "marketchart" endpoint
  # tidy this up
  
  # Make UNIX time human readable
  closePx$Date <- as.POSIXct(closePx$prices.1/1000, origin="1970-01-01", tz="GMT")
  
  # Remove duplicate timestamp columns from the result
  closePx[,c("market_caps.1","total_volumes.1")] <- NULL
  names(closePx) <- c("Unix_Time","Close_Price","Market_Cap","Total_Volume","Date")
  return(closePx)
}

closeUrl <- function(tokenName = "ethereum", numberWeeks = 5) {
  # build the url based on the coingecko token name and number of weeks data required
  # daily data returned only if request > 90 days, otherwise hours/minutes.
  #  cannot specify weekly data
  lookBack <- ifelse(numberWeeks < 18, 91, numberWeeks * 7)
  paste0("https://api.coingecko.com/api/v3/coins/", tokenName,
         "/market_chart?vs_currency=USD&days=", lookBack)
}

weeks <- function(df, weekDay = 7){
  # convert the daily dataframe (df) into a weekly dataframe
  # based on weekDay, where 7=Sunday, 1=Monday
  weekStart <- floor_date(df$Date, unit = "week", week_start = weekDay)
  ethWeekly <- df %>% filter(round_date(Date, unit = "day") == weekStart)
}

breakOutType <- function(prices, trendPeriod = 40){
  # Function to calculate what type of breakout, if any
  #  given a vector of prices, compares the last value to the 
  #  previous observations given in the trend period
  
  if(length(prices) <= trendPeriod){
    # test that have enough rows, if not return "data"
    return("data")
    } else {
      # have enough rows to calculate a breakout
      periodStart <- length(prices) - trendPeriod - 1
      periodEnd <- length(prices) - 1
      lastWeekPrice <- tail(prices, 1)
      if(max(prices[periodStart:periodEnd]) < lastWeekPrice){
        HighLow <- "High"
        } else if(min(prices[periodStart:periodEnd]) > lastWeekPrice){
            HighLow <- "Low"
        } else HighLow <- NA
      return(HighLow)
    }
}

testForBreakOut <- function(id, trendPeriod = 40, weekDay = 7){
  # Function to test for breakouts and calculates volatility
  # Accesses the coingecko endpoint, cleans up and calculates
  
  # id : coingecko token id
  # trendPeriod : weekly period over which to test for breakouts
  # weekDay : specifies starting day of the week. 7 = Sunday, 1 = Monday

  
  # Build the url for the API call
  print(id)
  urlEndpoint <- closeUrl(tokenName = id, numberWeeks = round(trendPeriod * 1.5))
  
  # Get the closing prices, tidy up and convert to weekly period
  tokenPxWeeks <- getAPI(urlEndpoint)
  
  # Test the API call has returned an error
  if(names(tokenPxWeeks)[1] == "error") {
    # Error generated, print it out and exit
    print(tokenPxWeeks)
    return("check data returned")
  }
  tokenPxWeeks <- tokenPxWeeks %>%
                    tidyClose() %>%
                      weeks(weekDay = weekDay)
  
  # Add in volatility calculation
  if(length(tokenPxWeeks$Close_Price) < trendPeriod){
    # test that have enough rows, if not set volatility column to "data"
    tokenPxWeeks$Volatility <- rep("data", length(tokenPxWeeks$Close_Price))
  } else {
    tokenPxWeeks$Volatility <- tokenPxWeeks$Close_Price %>%
      TTR::volatility(n=trendPeriod, calc = "close", N = 52)
  }
  
  
  # Add in if breakout has occured
  isBreakout <- tokenPxWeeks$Close_Price %>% breakOutType(trendPeriod = trendPeriod)
  
  # Return the last row of the dataframe and the type of breakout
  lastRow <- tail(tokenPxWeeks, 1) %>% select(Date, Close_Price, Volatility, Total_Volume, Market_Cap)
  return(cbind(id, isBreakout, lastRow))
}

findBreakpoints <- function(listToBreakup, intervalsOf = 50){
  # Given a sequence (listToBreakup), return a list cut into equal lengths
  #  determined by intervalsOf
  if(length(listToBreakup) < 50) {
    # Can be done in a single list
    retList <- listToBreakup
  } else {
    
    # Otherwise calculate where the 
    len <- length(listToBreakup)
    numbBreaks <- ifelse(len %% 50 > 0, floor(len / intervalsOf) + 1, floor(len / intervalsOf))
    cutPoints <- split(x= 1:len, cut(x = 1:len, breaks = numbBreaks))
    retList <- sapply(cutPoints, function(x) listToBreakup[x])
  }

  return(retList)
}

updateMktCaps <- function(fileName = "ERC20 Token Sectors.csv"){
  # Read in the manually classified ERC20 Token Sector csv
  #  append coingecko market cap info from their API
  #  save.
  #
  # Used by rotation and trend following models so should be run first 
  #  if the ERC20 Sector sheet has been updated
  
  coinUniverse <- read.csv(fileName, stringsAsFactors = F)
  
  # Get all non blank coin gecko names
  cgIds <- coinUniverse$CoinGecko.id[coinUniverse$CoinGecko.id != ""]
  
  # Build a url to request all their market caps
  # https://api.coingecko.com/api/v3/coins/markets?vs_currency=USD&ids=ethereum%2C%20numeraire%2C%20bzx-protocol&order=market_cap_desc&per_page=250&page=1&sparkline=false&price_change_percentage=7d
  # split the coin gecko id's into lists of maximum 50 length
  tokenList <- findBreakpoints(listToBreakup = cgIds, intervalsOf = 50)
  listIndx <- 1
  urlList <- ""
  repeat {
    urls <- sapply(tokenList[[listIndx]], function(x) paste0("https://api.coingecko.com/api/v3/coins/markets?vs_currency=USD&ids=",
                                                    paste0(x, collapse = "%2C%20"),
                                                    "&order=market_cap_desc&per_page=250&page=1&sparkline=false&price_change_percentage=7d"))
    urlList <- c(urlList, urls)
    listIndx <- listIndx + 1
    if(listIndx > length(tokenList)) {break}
  }
 
                  
  # Call the API for each list of upto 50 tokens at a time
  finalMktCaps <- data.frame(stringsAsFactors = F)
  indx <- 1
  repeat {
    mktCaps <- getAPI(urlList[[indx]])
    indx <- indx + 1
    finalMktCaps <- rbind(finalMktCaps, select(mktCaps,
                                               "id",
                                               "symbol",
                                               "name",
                                               "market_cap",))
    print(indx)
    if(indx > length(urlList)) {break}
    if(indx %% 50 == 0){
      print("Pausing 60 seconds to comply with coingecko API protocol")
      Sys.sleep(60)
    }
  }
  # mktCaps <- lapply(urlList, getAPI)
  
  # Convert the multiple lists into a single dataframe
  # do.call method prefered since preserves column names
  # finalMktCaps <- do.call(rbind.data.frame, mktCaps)
  
  # Tidy up the dataframe
  final <- left_join(coinUniverse, select(finalMktCaps, id, market_cap), by = c("CoinGecko.id" = "id")) %>%
              arrange(desc(Market_Cap))
  
  final <- final %>% select(id = "CoinGecko.id",
                            Symbol="Symbol",
                            Name = "Name",
                            Sector = "Sector.Classification",
                            Market_Cap = "market_cap",
                            Description = "Note") %>%
                      arrange(desc(Market_Cap))
  
  write.csv(final, "ERC20_Sectors_Mkt_Caps.csv", row.names = F)
  return(final)
}

weeklyRun <- function(dateSaved=Sys.Date(), trendPeriod = 40, weekDay = 7){
  # Function to read in token list
  # Get the data from the coin gekco api
  # Using the manually categorised token sectors, get weekly returns and
  #  add them as a column in the dataframe
  
  # dateSaved : parameter allows to specify which date is saved in the filename
  #             otherwise date function called is used
  # trendPeriod : how many weeks to assess for a breakout
  
  tokenUniverse <- read.csv("ERC20_Sectors_Mkt_Caps.csv", stringsAsFactors = F)
  # tokenUniverse <- read.csv("Test.csv", stringsAsFactors = F)
  tokenUniverse <- tokenUniverse[tokenUniverse$id != "",]
  
  breakOutList <- findBreakpoints(tokenUniverse$id, intervalsOf = 50)
  
  bOuts <- data.frame(stringsAsFactors = F)
  indx <- 1
  repeat {
    brkOuts <- lapply(breakOutList[[indx]], function(nmes) testForBreakOut(nmes, trendPeriod = trendPeriod, weekDay = weekDay))
    brkOuts <- do.call(rbind.data.frame, brkOuts)
    indx <- indx + 1
    bOuts <- rbind(bOuts, brkOuts)
    if(indx > length(breakOutList)) {break}
    print("Pausing 60 seconds to comply with coingecko API protocol")
    Sys.sleep(60)
  }
  
  write.csv(bOuts, paste0("Breakouts/", dateSaved, " ", trendPeriod, " Week Breakouts.csv"), row.names = F)
  return(bOuts)
}

