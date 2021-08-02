############################################################
#
# Read ether blockchain information from Etherscan API
#
############################################################

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)

myAPI <- "BA1WVFPYQ1VDMTN2Q4P55D9G2TYUY3DQSI"
divisor <- 10^18 # Etherscan returns the tokens / ether balances in 10 ^ 18. Need to divide by this amount
eth_yfi <- "0x2fdbadf3c4d5a8666bc06645b8358ab803996e28"
yfi <- "0x0bc529c00c6401aef6d220be8c6ea1667f6ad93e"
weth <- "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2"
myMetamask <- "0x750cd0a706e152b2e8a4497e4658cc9a6105d1ae"

getEthAPI <- function(url, apiKey){
  # Given a url access the Etherscan API
  rawResult <- GET(url = paste0(url,apiKey))
  resultText <- content(rawResult, type = "text", encoding = "UTF-8")
  # Extract the content out of the JSON format
  resultFlatten <- fromJSON(resultText, flatten = TRUE)
  # Turn into data frame
  resultDF <- as.data.frame(resultFlatten, stringsAsFactors = F)
  return(as.numeric(resultDF$result) / divisor)
}

ethScanUrl <- function(callType = "ethBalance", accountAddr = "", tokenAddr = ""){
  url <- case_when(
    callType == "ethBalance" ~ paste0("https://api.etherscan.io/api?module=account&action=balance&address=",
                                   accountAddr,
                                   "&tag=latest&apikey="),
    callType == "tokenBalance" ~ paste0("https://api.etherscan.io/api?module=account&action=tokenbalance&contractaddress=",
                                        tokenAddr,
                                        "&address=", accountAddr,
                                        "&tag=latest&apikey=")
  )
  return(url)
}
