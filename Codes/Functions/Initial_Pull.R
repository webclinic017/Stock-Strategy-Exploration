Initial_Pull = function() {
  NASDAQ_Stocks = read.csv(paste0(Project_Folder, "/Data/NASDAQ.csv")) %>%
    mutate(LastSale = as.numeric(as.character(LastSale)),
           IPOyear = as.numeric(as.character(IPOyear))) %>%
    filter(!is.na(LastSale),
           !is.na(IPOyear),
           LastSale >= 15,
           IPOyear <= as.numeric(year(Sys.Date()) - 5))
  AMEX_Stocks = read.csv(paste0(Project_Folder, "/Data/AMEX.csv")) %>%
    mutate(LastSale = as.numeric(as.character(LastSale)),
           IPOyear = as.numeric(as.character(IPOyear))) %>%
    filter(!is.na(LastSale),
           !is.na(IPOyear),
           LastSale >= 15,
           IPOyear <= as.numeric(year(Sys.Date()) - 5))
  NYSE_Stocks = read.csv(paste0(Project_Folder, "/Data/NYSE.csv")) %>%
    mutate(LastSale = as.numeric(as.character(LastSale)),
           IPOyear = as.numeric(as.character(IPOyear))) %>%
    filter(!is.na(LastSale),
           !is.na(IPOyear),
           LastSale >= 15,
           IPOyear <= as.numeric(year(Sys.Date()) - 5))
  ETFS = read.csv(paste0(Project_Folder,"/Data/ETFList.csv")) %>%
    mutate(IPOyear = as.numeric(year(Sys.Date()) - 5)) %>%
    filter(LastSale >= 15)
  
  Market_Tickers = data.frame(
    Symbol = c("^GSPC", "^IXIC", "^DJI", "^VIX", "^VXN", "MFST","DIS"),
    Name = c(
      "S&P 500",
      "NASDAQ",
      "Dow Jones",
      "S&P Volatility",
      "NASDAQ Volatility",
      "Microsoft",
      "Disney"
    ),
    IPOyear = as.numeric(year(Sys.Date()) - 5),
    LastSale = 1000
  )
  
  Total_Stocks = bind_rows(Market_Tickers, NASDAQ_Stocks, NYSE_Stocks, AMEX_Stocks, ETFS)
 
  for (j in 1:2){
    Dump = list()
    p = progress_estimated(n = nrow(Total_Stocks), min_time = 3)
    for (i in 1:nrow(Total_Stocks)) {
      p$pause(0.1)$tick()$print()
      ticker = as.character(Total_Stocks$Symbol[i])
      
      if(j == 1){
      stockData = try(getSymbols(ticker,
                                 src = "yahoo",
                                 from = ifelse(j == 1,
                                               Sys.Date() - 10),
                                 auto.assign = FALSE) %>%
                        as.data.frame() %>%
                        mutate(Date = ymd(rownames(.))))
      }else{
        
        stockData = try(getSymbols(ticker,
                                   src = "yahoo",
                                   auto.assign = FALSE) %>%
                          as.data.frame() %>%
                          mutate(Date = ymd(rownames(.))))
      }
      if ("try-error" %in% class(stockData)) {
        Dump[[i]] = stockData
      } else{
        colnames(stockData) = c("Open",
                                "High",
                                "Low",
                                "Close",
                                "Volume",
                                "Adjusted",
                                "Date")
        stockData$Stock = ticker
        Dump[[i]] = stockData
      }
    }
    list.condition <-
      sapply(Dump, function(x)
        class(x) == "data.frame")
    output.list  <- Dump[list.condition]
    Combined_Results = plyr::ldply(output.list, data.frame)
    
    CHECK = Combined_Results %>%
      group_by(Stock) %>%
      na.locf() %>%
      na.omit() %>%
      summarise(O = sum(Open == 0),
                H = sum(High == 0),
                L = sum(Low == 0),
                C = sum(Close == 0),
                V = sum(Volume < 100),
                V_AVG = median(Volume),
                C_AVG = median(Close),
                DV = V_AVG * C_AVG) %>%
      filter(O == 0,
             H == 0,
             L == 0,
             C == 0,
             V == 0,
             V_AVG >= 400000,
             DV >= 20000000)
    Total_Stocks = Total_Stocks %>%
      filter(Symbol %in% c(CHECK$Stock,"^VIX","^VXN"))
  }
  
  save(Combined_Results,
       file = paste0(Project_Folder, "/Data//NASDAQ Historical.RDATA"))
}