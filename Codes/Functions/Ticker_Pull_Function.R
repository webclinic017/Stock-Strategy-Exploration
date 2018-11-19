Ticker_Pull_Function = function(Location){
  # ############ Sample Data #################
  # Location = "C://Users//aayorde//desktop//"
  # ##########################################
  
  ## Loading Required Packages
  require(quantmod)
  require(tidyverse)
  require(lubridate)
  
  ## Defining Historical Data Location
  Historical_File = paste0(Location,"NASDAQ Historical.RDATA")
  load(Historical_File)
  
  ## Defining Ticker List And Start Date
  Tickers = unique(Combined_Results$Stock)
  From = max(Combined_Results$Date) + 1
  
  ## Looping Through and Adding New Data
  p = progress_estimated(n = length(Tickers),min_time = 3)
  Dump = list()
  for(i in 1:length(Tickers)){
    p$pause(0.1)$tick()$print()
    ticker = as.character(Tickers[i])
    stockData = try(getSymbols(
      ticker,
      from = From,
      src = "yahoo",
      auto.assign = FALSE) %>% 
        as.data.frame() %>%
        mutate(Date = ymd(rownames(.)))) 
    if("try-error" %in% class(stockData)){
      Dump[[i]] = stockData
    }else{
      colnames(stockData) = c("Open","High","Low","Close","Volume","Adjusted","Date")
      stockData$Stock = ticker
      Dump[[i]] = stockData
    }
  }
  ## Simplifying List to a Data.Frame
  list.condition <- sapply(Dump, function(x) class(x) == "data.frame")
  output.list  <- Dump[list.condition]
  New_Results = plyr::ldply(output.list,data.frame)
  
  ## Appending New Data and Saving Results
  Combined_Results = bind_rows(Combined_Results,New_Results)
  save(Combined_Results,
       file = Historical_File)
}