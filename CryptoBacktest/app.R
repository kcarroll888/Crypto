
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate) # date time functions used when preparing API calls
library(digest)    # SHA hashing functions used when preparing API calls
library(httr)      # rest API call functions returns JSON
library(jsonlite)  # converting JSON objects to R objects
library(dplyr)     # useful "case-when" statements and general data frame manipulation
library(tidyr)     # for pivoting of dataframes
library(quantmod)  # for the TTR, xts finance & time series functions
library(ggplot2)   # Visualise the analysis


mkts <- read.csv("C:/Users/KC Framework Laptop/My Drive/Crypto/FTX_Markets.csv", stringsAsFactors = F)

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Crypto Trend Following Backtest"),

    # Three columns: Inputs, Chart, Chart Selector
    fluidRow(
      # Which period to analyse
      column(width = 2,
             dateRangeInput(inputId = "dateRange", label = "Select the Start and End Dates",
                            format = "yyyy-mm-dd", start = "2021-05-01", end = Sys.Date()),
            
             selectInput(inputId = "resolution", label = "Select time interval",
                         choices = c("15 seconds", "minute", "5 minutes", "15 minutes", "1 hour", "4 hours", "day"),
                         selected = "day"),
            
             numericInput(inputId = "trendPeriod", label = "Select Trend Period",
                          value = 40),
             
             selectInput(inputId = "mktName", label = "Select Crypto Perpetual",
                         choices = mkts$result.name, selected = "ETH-PERP"),
            
             actionButton(inputId = "evaluate", label = "Go!", class = "btn-primary btn-lg")
             ),

      # Show a plot of the breakouts
      column(width = 9,
             plotOutput("chartPlot")),
        
      # Select which breakout plot to show
      column(width = 1,
             selectInput(inputId = "chartChoice", label = "Chart Selector",
                         choices = c("Only Breakouts",
                                     "Breakouts & Long Stops",
                                     "Breakouts & Short Stops"),
                         selected = "Only Breakouts"))
             ),
    
    # Three columns on a new row: PnL analysis, Table of PnL, Table selector
      sidebarLayout(
        sidebarPanel(
          # pnl summary
          h5("Strategy PnL Summary"),
          tableOutput("pnlSummary"),
          
          h5("Buy & Hold PnL Summary"),
          tableOutput("buyHold")
        ),
        
        mainPanel(
          # pnl table
          dataTableOutput("pnlTable")
        )
    )
)

# Define server logic
server <- function(input, output) {
  
  ################## REGULAR FUNCTIONS ###########################
  
  cleanUp <- function(flattenThis, ret = "list"){
    # Given the JSON from the GET call
    # Turn into a list as default, or data frame if "df" specified
    resultText <- content(flattenThis, type = "text", encoding = "UTF-8")
    resultFlatten <- fromJSON(resultText, flatten = TRUE)
    if (ret == "df") resultFlatten <- as.data.frame(resultFlatten, stringsAsFactors = F)
    
    return(resultFlatten)
  }
  
  historicalPrices <- function(marketName = "ETH-PERP",
                               start = "2021-01-01",
                               end = "2021-12-11",
                               resolution = "day"){
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
    startTime <- as.numeric(ymd_hms(paste0(start, "22:00:00")))
    endTime <- as.numeric(ymd_hms(paste0(end, "22:00:00")))
    path <- paste0("https://ftx.com/api/markets/",marketName,"/candles?resolution=",thisResolution,"&start_time=",startTime,"&end_time=", endTime)
    rawResult <- cleanUp(GET(path), ret = "df")
    
    # Convert to xts and return
    rawResult$success <- NULL
    colnames(rawResult) <- c("startTime", "time", "open", "high", "low", "close", "volume")
    #row.names(rawResult) <- rawResult$startTime
    return(rawResult)
  }
  
  
  # Breakout trading rule, mark where breakouts have occurred
  breakoutType <- function(priceSeries, trendPeriod = 16){
    # Given a vector of closing prices & trendperiod, will return a vector of the 
    # same length indicating at each point if a breakout occured.
    # "High", "Low" or NA
    
    dataPoints <- length(priceSeries)
    type <- FALSE
    # Test have enough data in the price series to test against period specified
    if(dataPoints > trendPeriod){
      startPoint <- dataPoints - trendPeriod
      if(priceSeries[dataPoints] > seriesHi(priceSeries[startPoint:(dataPoints - 1)])){
        type <- "High"
      } else if(priceSeries[dataPoints] < seriesLo(priceSeries[startPoint:(dataPoints - 1)])){
        type <- "Low"
      }
    }
    return(type)
  }
  
  markBreakouts <- function(closePx, trendPeriod = 16){
    # Given a vector of closing prices, return an equal length vector
    # which identifies if the row is a breakout over the period 
    # supplied in the trendPeriod argument.
    # Returning "High", "Low", or NA = none
    highLow <- sapply(1:length(closePx), function(row) breakoutType(closePx[1:row], trendPeriod = trendPeriod))  
    return(highLow)
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
    while(r < nrow(pxSeries)) {
      # Go through each row, look for a low breakout
      if(pxSeries[r, "breakOut"] == type) {
        # Found one so calculate the return, log the date/time and opening price
        thisRet <- data.frame(openTime = pxSeries[r, "startTime"],
                              open = pxSeries[r, "close"],
                              distanceMA = pxSeries[r, "distanceMA"], stringsAsFactors = F)
        while(
          pxSeries[r, lookForThisColumn] != TRUE & r < nrow(pxSeries)){
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
  
  stopLossTrigger <- function(pxSeries, trendPeriod = 16){
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
    
    pxSeries$stopSMALong <- pxSeries$close < pxSeries$SMA  # Long stop signal triggered
    pxSeries$stopSMAShort <- pxSeries$close > pxSeries$SMA  # Short stop signal triggered
    
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
    pxSeries$StopLong[is.na(pxSeries$StopLong)] <- FALSE                # Mark NAs as FALSE
    
    pxSeries$StopShort <- pxSeries$stopSMAShort #| pxSeries$stopReturnShort | pxSeries$stopATRShort # Combine all the Long Stop Signals
    pxSeries$StopShort[is.na(pxSeries$StopShort)] <- FALSE                 # Mark NAs as FALSE
    
    return(pxSeries)
  }
  
  addIndicatorsDF <- function(pxSeries, trendPeriod = 16){
    # Given a regular data frame containing "Date", "Open", "High", "Low", "Close", "Volume"
    # Add various additional indicators to use for back testing
    
    # Add breakouts
    pxSeries$breakOut <- markBreakouts(pxSeries$close, trendPeriod = trendPeriod)
    
    # Add Simple Moving Average from TTR
    pxSeries$SMA <- SMA(pxSeries[,"close"], n = trendPeriod)
    
    # Distance of the current price to the moving average
    pxSeries$distanceMA <- abs(pxSeries$close - pxSeries$SMA)
    
    # Add VWAP from TTR
    pxSeries$VWAP <- VWAP(price = pxSeries[, "close"], volume = pxSeries[, "volume"], n = trendPeriod)
    
    # Add Signal to Noise Ratio (SNR) from TTR package
    # Using as.numeric to convert a single column to a vector - function calls often return a list
    pxSeries$SNR <- SNR(pxSeries[,c("high", "low", "close")], n = trendPeriod)
    
    # Delta of the volume over the trend period
    pxSeries$VolumeDelta <- as.numeric(Delt(pxSeries[, "volume"], k = trendPeriod))
    
    # Add Average True Range & Std Dev of the ATR over the trend period
    tr <- ATR(pxSeries[, c("high", "low", "close")], n = trendPeriod)
    pxSeries$TR <- tr[, "tr"]  # Extract the True Range component
    pxSeries$ATR <- tr[, "atr"]  # Extract the Average TR component
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
  
  pnlSummary <- function(pxSeries){
    # Define empty dataframes which will be returned in the event that there are no Long / Short breakouts to calculate
    longPct <- data.frame(openTime = NA, open = NA, distanceMA = NA, close = NA, closeTime = NA,
                          pctReturn = NA, stake = NA, notional = NA, pnl = NA, type = NA, stringsAsFactors = F)
    shortPct <- longPct
    
    # Calculate short return
    shortPct <- pxSeries %>% select(startTime, close, breakOut, StopShort, distanceMA) %>%
      breakOutReturn(type = "Low")
    
    # Test if there were any short values returned
    if(length(shortPct) >0){
      shortPct <- shortPct %>% mutate(pctReturn = (open - close) * 100 / open,
             stake = 60 / distanceMA,
             notional = open * stake,
             pnl = pctReturn * notional / 100,
             type = "Low")
    }

    # Calculate long return
    longPct <- pxSeries %>% select(startTime, close, breakOut, StopLong, distanceMA) %>%
      breakOutReturn(type = "High") 
    
    # Test if there were any short values returned
    if(length(longPct) > 0){
      longPct <- longPct %>% mutate(pctReturn = (close - open) * 100 / open,
             stake = 60 / distanceMA, 
             notional = open * stake,
             pnl = pctReturn * notional / 100,
             type = "High")
    }

    
    return(rbind(longPct, shortPct))
  }
  
  ################## SHINY EVENTS ###########################
  
  rval_pxSeries <- eventReactive(input$evaluate, {
    # When the Go! button pressed evalute the inputs given
    # Get historical data
    px <- historicalPrices(marketName = input$mktName,
                           start = input$dateRange[1],
                           end = input$dateRange[2],
                           resolution = input$resolution)
    
    # Add the indicatiors
    pxSeries <- addIndicatorsDF(px, trendPeriod = input$trendPeriod) %>% stopLossTrigger(trendPeriod = input$trendPeriod)
  })
  
  output$chartPlot <- renderPlot({
    # Generate chart requested
    pxSeries <- rval_pxSeries()
    
    p <- ggplot(pxSeries, aes(x=startTime, y=close, colour = breakOut, size = Return1)) + geom_point() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.1, vjust = 0.5, size = 7)) +
      scale_x_discrete(breaks = pxSeries$startTime[c(TRUE, rep(FALSE, nrow(pxSeries) / 40))])
      
    # Unable to use case_when here as creates a "ggplot" object rather than a list << DEBUG >>
    if(input$chartChoice == "Only Breakouts") {
      p
      } else if(input$chartChoice == "Breakouts & Long Stops"){
        p + geom_vline(xintercept = pxSeries[pxSeries$StopLong, "startTime"])
        } else {
          p + geom_vline(xintercept = pxSeries[pxSeries$StopShort, "startTime"])
          }
    })
  
  output$pnlSummary <- renderTable({
    # Data table of the strategy pnl
    pxSeries <- rval_pxSeries()
    pnlSummary(pxSeries) %>% group_by(type) %>% summarise(NumbObs = n(), AvgPct = sum(pnl) * 100 / sum(notional), VolAdjPnL=sum(pnl))
  })
  
  output$buyHold <- renderTable({
    # Data table of buying & holding
    pxSeries <- rval_pxSeries()
    data.frame(BuyHoldPct = (last(pxSeries$close) - first(pxSeries$close)) * 100 / first(pxSeries$close))
  })
  
  output$pnlTable <- renderDataTable({
    # Data table of the back test PnL during period selected
    pxSeries <- rval_pxSeries()
    pnlSummary(pxSeries)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
