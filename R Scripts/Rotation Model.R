########## Rotation Model for Tokens ###########
#
# Closing prices from Coingecko API
#
# 1. Get necessary closing prices
# 2. Compare the closing prices
# 3. Output the relative strengths
#
################################################

# API has limit of 100 calls per minute

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)

setwd("C:/Users/Kurt/Google Drive/Crypto")

# Function which accesses the endpoint at CoinGecko and returns a dataframe
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
  # closePx dataframe the return from coingecko "marketchart" endpoint
  # tidy this up
  
  # Make UNIX time human readable
  closePx$Date <- as.POSIXct(closePx$prices.1/1000, origin="1970-01-01", tz="GMT")
  
  # Remove duplicate timestamp columns from the result
  closePx[,c("market_caps.1","total_volumes.1")] <- NULL
  names(closePx) <- c("Unix_Time","Close_Price","Market_Cap","Total_Volume","Date")
  return(closePx)
}

smoothedReturns <- function(coinName = "ethereum", numberWeeks = 5, dayOfWeek = 7){
  # coinName:    coingecko coin name
  # numberWeeks: look back period for the moving average
  # dayOfWeek:   which day of the week is the weekly return calculated between. 7=Sunday, 1=Monday etc
  
  # Get the closing price from coinGecko API
  #  so build the url. Default to price vs USD
  
  # fromDate <- as.numeric(as.POSIXct(input$dateRange[1], tz = "GMT", origin="1970-01-01"))
  # toDate <- as.numeric(as.POSIXct(input$dateRange[2], tz = "GMT", origin="1970-01-01"))
  # To formulate an X week moving average, need N = 7 * (X+1) = days of data. With N > 90
  #   since coinGecko API only returns daily data if request > 90 days
  #   so will request 100 days makes data easier to work with
  # n <- ifelse((7 * (numberWeeks + 1)) > 90, 7 * (numberWeeks + 1), 91)
  # toDate <- as.numeric(as.POSIXct(Sys.Date(), tz = "GMT", origin="1970-01-01"))
  # fromDate <- as.numeric(as.POSIXct(Sys.Date() - n, tz = "GMT", origin="1970-01-01"))
  
  # Make sure to only get daily data
  print(coinName)
  lookBack <- ifelse(numberWeeks < 18, 91, numberWeeks * 7)
  
  closeUrl <- paste0("https://api.coingecko.com/api/v3/coins/", coinName,
                     "/market_chart?vs_currency=USD&days=", lookBack)
  
  # call the API & tidy up the returned dataframe
  coinData <- getAPI(closeUrl)
  coinData <- tidyClose(coinData)
  
  # pull out desired weekday 7=Sunday
  weekStart <- floor_date(coinData$Date, "week", week_start = dayOfWeek)
  coinDataWeekStart <- coinData %>% filter(Date == weekStart)
  
  # work out smoothed average for the weeks return
  coinDataWeekStart <- coinDataWeekStart %>%
                          mutate(Prev_Close = lag(Close_Price),
                                 Week_Return = (Close_Price - Prev_Close) * 100 / Prev_Close)

  return(mean(tail(coinDataWeekStart$Week_Return, numberWeeks)))
}

getCoinUniverse <- function(fileName = "ERC20_Sectors_Mkt_Caps.csv", source="api") {
  
  if(source=="api"){
      # Get the data from the coin gekco api
      # Using the manually categorised token sectors, get weekly returns and
      #  add them as a column in the dataframe
      
      coinUniverse <- read.csv("ERC20_Sectors_Mkt_Caps.csv", stringsAsFactors = F)
      
      # Read the closing data for each coin within each secor & get 
      #  each coins smoothed weekly return
      
      #  remove the lines which do not have a coin gecko id
      coinUniverse <- coinUniverse[coinUniverse$id != "",]
      coinReturns1 <- sapply(coinUniverse$id[1:50], smoothedReturns)
      #  need to pause for 1 min as throttling applies
      print("Pausing for 60 seconds")
      Sys.sleep(60)
      coinReturns2 <- sapply(coinUniverse$id[51:97], smoothedReturns)
      
      # Add the returns into the main data
      coinUniverse <- coinUniverse %>% mutate(Returns = c(coinReturns1, coinReturns2))
      
      # Save the returns so can be retrieved from memory
      write.csv(coinUniverse, paste0("Token Returns/Token Returns ",Sys.Date(),".csv"), row.names = F)
      
      } else {
        coinUniverse <- read.csv(paste0("Token Returns/",fileName), stringsAsFactors = F)
        }
  
  return(coinUniverse)
  
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
  
  url <- paste0("https://api.coingecko.com/api/v3/coins/markets?vs_currency=USD&ids=",
                paste0(cgIds[1:50], collapse = "%2C%20"),
                "&order=market_cap_desc&per_page=250&page=1&sparkline=false&price_change_percentage=7d")
  
  url2 <- paste0("https://api.coingecko.com/api/v3/coins/markets?vs_currency=USD&ids=",
                 paste0(cgIds[51:97], collapse = "%2C%20"),
                 "&order=market_cap_desc&per_page=250&page=1&sparkline=false&price_change_percentage=7d")
  
  mktCaps <- rbind(getAPI(url), getAPI(url2))
  
  final <- left_join(coinUniverse, select(mktCaps, id, market_cap), by = c("CoinGecko.id" = "id"))
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

# Compare the returns, rank the sectors
coinUniverse <- getCoinUniverse(source = "api")
rank <- coinUniverse %>% group_by(Sector) %>%
                  summarise(Avg_Sector_Return = mean(Returns, na.rm = T),
                            Sector_Number_Tokens = n()) %>%
                  arrange(desc(Avg_Sector_Return))

