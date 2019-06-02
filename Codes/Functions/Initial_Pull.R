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
  
  Total_Stocks = bind_rows(Market_Tickers, NASDAQ_Stocks, NYSE_Stocks, AMEX_Stocks, ETFS) %>%
    filter(
      !Symbol %in% c(
        "AGFSW",
        "ARII",
        "CORI",
        "DWCH",
        "TACOW",
        "ECYT",
        "ESRX",
        "JTPY",
        "LINDW",
        "OXBRW",
        "PBSK",
        "RSYS",
        "SODA",
        "SLNOW",
        "SONC",
        "BJZ",
        "BPK",
        "BLH",
        "CTT",
        "ECOM",
        "ECC",
        "ETX",
        "EEQ",
        "FEI",
        "FSIC",
        "GFA",
        "HTGX",
        "LHO",
        "LKM",
        "NAP",
        "SEP",
        "EIA",
        "MAB",
        "MIW",
        "EMI",
        "NYH",
        "CTT",
        "ECOM",
        "ECC",
        "ETX",
        "FEI",
        "ARDM",
        "ATHN",
        "GOV",
        "GNBC",
        "HDP",
        "KANG",
        "INTX",
        "IPAS",
        "JASNW",
        "LOXO",
        "TSRO",
        "AHL",
        "CVRR",
        "DM",
        "FCB",
        "KORS",
        "NFX",
        "PAH",
        "SN",
        "VLP",
        "MMV",
        "EMJ",
        "EVJ",
        "EIO",
        "EVO",
        "EIP",
        "EVP",
        "CTT",
        "ECOM",
        "ECC",
        "ETX",
        "FEI",
        "BHBK",
        "BLMT",
        "IDTI",
        "WTW",
        "DSW",
        "ELLI",
        "SSWN",
        "VZA",
        "ORM",
        "NXTM",
        "ECC",
        "ETX",
        "FBR"
      )
    )
  Dump = list()
  
  p = progress_estimated(n = nrow(Total_Stocks), min_time = 3)
  for (i in 1:nrow(Total_Stocks)) {
    p$pause(0.1)$tick()$print()
    ticker = as.character(Total_Stocks$Symbol[i])
    
    
    stockData = try(getSymbols(ticker,
                               src = "yahoo",
                               auto.assign = FALSE) %>%
                      as.data.frame() %>%
                      mutate(Date = ymd(rownames(.))))
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
  
  Combined_Results = Combined_Results %>%
    group_by(Stock) %>%
    na.locf() %>%
    ungroup()
  
  save(Combined_Results,
       file = paste0(Project_Folder, "/Data//NASDAQ Historical.RDATA"))
}