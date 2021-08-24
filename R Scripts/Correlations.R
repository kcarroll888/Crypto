#################################################
#
# Get correlation of coins between two dates
#  default = correlation 5 weeks from current date
#
#################################################

library(httr)
library(jsonlite)
library(dplyr)

setwd("C:/Users/kurtc/Google Drive/Crypto")
options(scipen = 999)

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

closeUrl <- function(tokenName = "ethereum", numberWeeks = 5) {
  # build the url based on the coingecko token name and number of weeks data required
  # daily data returned only if request > 90 days, otherwise hours/minutes.
  #  cannot specify weekly data
  lookBack <- ifelse(numberWeeks < 18, 91, numberWeeks * 7)
  paste0("https://api.coingecko.com/api/v3/coins/", tokenName,
         "/market_chart?vs_currency=USD&days=", lookBack)
}

ratioPair <- function(coin1 = "ethereum", coin2 = "yearn-finance", nWeeks = 5){
  tidyCoin1 <- closeUrl(coin1, numberWeeks = nWeeks) %>% getAPI %>% tidyClose
  tidyCoin2 <- closeUrl(coin2, numberWeeks = nWeeks) %>% getAPI %>% tidyClose
  return(tidyCoin1$Close_Price / tidyCoin2$Close_Price)
}

coinList <- c("yearn-finance","ethereum","numeraire","curve-dao-token","fantom","uniswap","wrapped-bitcoin",
              "celsius-degree-token","compound-governance-token","nexo","maker","aave","balancer","bancor",
              "ftx-token","serum","matic-network", "solana", "binancecoin", "sushi")

# Bulid URL endpoint call for each coin
urlList <- lapply(coinList, closeUrl)

# Call coinGecko API for each endpoint
dfList <- lapply(urlList, getAPI)

# Tidy up the response, remove duplicate columns and add column names
cleanList <- lapply(dfList, tidyClose)

# Make sure columns are all the same size otherwise sapply call will return a list
# and need a dataframe
ncols <- min(sapply(cleanList, nrow))
fullDF <- sapply(1:length(cleanList), function(x) cbind(cleanList[[x]]$Close_Price[1:ncols]))

colnames(fullDF) <- coinList

corMatrix <- cor(fullDF)
