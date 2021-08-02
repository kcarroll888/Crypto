# Get market cap from coingecko

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

# Clean up the market data
tidyMktCap <- function(closePx) {
  
  # Remove duplicate timestamp columns from the result
  closePx[,c("market_caps.1","total_volumes.1")] <- NULL
  names(closePx) <- c("Unix_Time","Close_Price","Market_Cap","Total_Volume","Date")
  return(closePx)
}

update_mkt_caps <- function(fileName = "ERC20 Token Sectors.csv"){
  # Read in the manually classified ERC20 Token Sector csv
  #  append coingecko market cap info from their API
  #  save
  
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
