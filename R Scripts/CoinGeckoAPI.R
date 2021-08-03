## Accessing historical Token data from CoinGecko website
## using a shiny interface

library(httr)
library(jsonlite)
library(lubridate)
library(shiny)
library(ggplot2)
library(scales)

setwd("C:/Users/kurtc/Google Drive/Crypto")
options(stringsAsFactors = FALSE, scipen = 9999)

# URL to API
# url  <- "https://api.coingecko.com/api/v3"

# Endpoints
# allCoins <- "/coins/list"  # All coins
# eth <- "/coins/ethereum/ohlc?vs_currency=USD&days=1"  # Ether

## Call to the beta OHLC endpoint returns
## 
## 1 - 2 days: 30 minutes
## 3 - 30 days: 4 hours
## 31 and before: 4 days
# fullPath <- "https://api.coingecko.com/api/v3/coins/eth/ohlc?vs_currency=USD&days=3"

# Function to GET the call and return a dataframe
getAPI <- function(url) {
  rawResult <- GET(url = url)
  resultText <- content(rawResult, type = "text", encoding = "UTF-8")
  resultFlatten <- fromJSON(resultText, flatten = TRUE)
  resultDF <- as.data.frame(resultFlatten, stringsAsFactors = F)
  return(resultDF)
}

# Function to access the closing px GET endpoint
# Get historical market data include price, market cap, and 24h volume within a range of timestamp 
# (granularity auto). Minutely data will be used for duration within 1 day, Hourly data will be used
# for duration between 1 day and 90 days, Daily data will be used for duration above 90 days
# 
# Endpoint consumes and returns UNIX time. Url should contain COIN and unix FROM and TO numbers

getClose <- function(url) {
  # Make UNIX time human readable
  close <- getAPI(url)
  close$Date <- as.POSIXct(close$prices.1/1000, origin="1970-01-01", tz="GMT")
  
  # Remove duplicate timestamp columns from the result
  close[,c("market_caps.1","total_volumes.1")] <- NULL
  names(close) <- c("Unix_Time","Close_Price","Market_Cap","Total_Volume","Date")
  return(close)
}

# Either pre-load coinsList object or populate below by uncommenting

coinsList <- c("ethereum", "polymath-network", "uniswap", "celsius-degree-token", "wbtc", "quant-network", "yearn-finance",
              "binancecoin","theta-token","banana-token","celsius-degree-token","hedgetrade","yearn-finance",
              "numeraire","ampleforth","dai", "bzx-protocol", "band-protocol", "tellor", "chainlink","ocean-protocol", "okb",
              "fantom","curve-dao-token","compound-governance-token","nexo","maker","aave","balancer","bancor", "cream-2")

####### Shiny Front End to Play with API #########

ui <- fluidPage(
  titlePanel("Accessing CoinGecko API"),
  # sidebar with dropdown to choose which coin to access
  sidebarPanel(
    selectInput("coin","Select Coin", choices = coinsList),
    selectInput("ccy", "Select currency", choices = c("USD","EUR","GBP","DKK")),
    dateRangeInput("dateRange", "Enter date range"),
    actionButton("getData","Get Data")
  ),
  
  mainPanel(
    plotOutput("coinChart")
  )
)

server <- function(input, output){
  
  # Update if get data button pressed
  updateChart <- eventReactive(input$getData, {
    
    # convert the dates and times to unix format
    fromDate <- as.numeric(as.POSIXct(input$dateRange[1], tz = "GMT", origin="1970-01-01"))
    toDate <- as.numeric(as.POSIXct(input$dateRange[2], tz = "GMT", origin="1970-01-01"))
    
    # build the url from the inputs
    closeUrl <- paste0("https://api.coingecko.com/api/v3/coins/", input$coin,
                       "/market_chart/range?vs_currency=", input$ccy,
                       "&from=", fromDate,
                       "&to=", toDate)
    # call the API
    coinData <- getClose(closeUrl)
  })
  
  breakString <- eventReactive(input$getData, {
    # figure out the chart axis date & time breaks to use
    if(input$dateRange[2] - input$dateRange[1] < 2){
      # Less than a day so API returns minute data
      breakString <- "15 min"
      
    } else if(input$dateRange[2] - input$dateRange[1] < 8){
      # between 1 day and 90 days so hourly
      breakString <- "2 hours"
      
    } else if(input$dateRange[2] - input$dateRange[1] < 91){
      # more than 8 days still returns hours but looks messy
      breakString <- "1 days"
      
    } else breakString <- "1 week"
  })
  
  # Plot the chart
  output$coinChart <- renderPlot(
    ggplot(updateChart(), aes(x = Date, y = Close_Price)) +
      geom_line() +
      scale_x_datetime(breaks = breaks_width(breakString())) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  )
  
  
  
}

shinyApp(ui = ui, server = server)

##################################################

# Test it out on ethereum
# coin <- "ethereum"
# CCY <- "USD"
# fromDate <- as.numeric(as.POSIXct("2020-08-20 00:00:01", tz = "GMT", origin="1970-01-01"))
# toDate <- as.numeric(as.POSIXct("2020-08-28 22:00:01", tz = "GMT", origin="1970-01-01"))

# build url
# closeUrl <- paste0("https://api.coingecko.com/api/v3/coins/", coin,
#                   "/market_chart/range?vs_currency=", CCY,
#                   "&from=", fromDate,
#                   "&to=", toDate)

# get closing prices
# eth <- getClose(closeUrl)


# Get all coins
# allCoins <- getAPI(url = paste0(url, allCoins))


# Get ether prices
# rawResult <- getAPI(url = paste0(url, eth))

# Convert from JSON to data frame
# result_text <- content(rawResult, type = "text")
# result_flatten <- fromJSON(result_text, flatten = TRUE)
# resultDF <- as.data.frame(result_flatten, stringsAsFactors = F)
# names(resultDF) <- c("Time", "Open", "High", "Low", "Close")

# Convert to UNIX time ie secs from "1970-01-01" and UTC
# resultDF$DateTime <- as.POSIXct(resultDF$Time/1000, origin = "1970-01-01", tz = "UTC")
