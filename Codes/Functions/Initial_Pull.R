Initial_Pull = function(Cap = "Small",PAPER = T) {
  ## Updating Stock List
  Auto_Stocks = stockSymbols() %>%
    filter(!is.na(Industry),
           !is.na(Sector),
           !is.na(MarketCap))
  
  ## Setting API Keys
  if(PAPER){
    KEYS = read.csv(paste0(Project_Folder,"/Data/Keys/Paper API.txt"))
    Sys.setenv('APCA-API-KEY-ID' = KEYS$Key.ID)
    Sys.setenv('APCA-API-SECRET-KEY' = KEYS$Secret.Key)
  }else{
    KEYS = read.csv(paste0(Project_Folder,"/Data/Keys/Live API.txt"))
    Sys.setenv('APCA-API-KEY-ID' = as.character(KEYS$Key.ID))
    Sys.setenv('APCA-API-SECRET-KEY' = as.character(KEYS$Secret.Key))
  }
  
  ## Pulling Available
  Alpaca_Stocks = get_assets() %>%
    filter(status == "active",
           tradable)
  
  Auto_Stocks = Auto_Stocks %>%
    mutate(Multiplier = str_extract(MarketCap,"\\w$"),
           Value = str_extract(MarketCap,"\\d+")) %>%
    filter(str_detect(Multiplier,"M|B")) %>%
    mutate(New_Cap = case_when(
      Multiplier == "M" ~ as.numeric(Value) * 1e6,
      Multiplier == "B" ~ as.numeric(Value) * 1e9
    )) %>%
    filter(New_Cap > 500e6,
           Symbol %in% Alpaca_Stocks$symbol) %>%
    mutate(Cap_Type = case_when(
      New_Cap > 300e9 ~ "Mega",
      New_Cap > 10e9 ~ "Large",
      New_Cap > 2e9 ~ "Mid",
      New_Cap > 500e6 ~ "Small"
    )) %>%
    select(-c(IPOyear,Exchange,Multiplier,Value)) %>%
    filter(!str_detect(str_trim(Symbol),
                       paste0("^ARII$|^ATHN$|^BHBK$|^BLMT$|^ECYT$|^ESRX$|^GNBC$|^HDP$|^KANG$|^IDTI$",
                              "|^LOXO$|^NXTM$|^PBSK$|^SODA$|^SONC$|^TSRO$|^NAVG$|^ULTI$|^WTW$|^AHL$",
                              "|^BJZ$|^BPK$|^DM$|^DSW$|^ECC$|^ETX$|^ELLI$|^FCB$|^FBR$|^HTGX$|^LHO$",
                              "|^KORS$|^MSF$|^NFX$|^SSWN$|^SEP$|^TLP$|^VLP$|^VZA$|^WGP$|^ORM$|^DEACU$",
                              "|^SVA$|^UBS$|^CRVS$|^HEXO$|^ALRM$|^KTB$")))
  
  ## Filtering to Specific 
  if(Cap != "All"){
      Auto_Stocks = Auto_Stocks %>%
        filter(Cap_Type == Cap)
  }
  
  save(Auto_Stocks,
       file = paste0(Project_Folder,"/Data/Stock_META.RDATA"))
  
  Market_Tickers = data.frame(
    Symbol = c("^GSPC", "^IXIC", "^DJI", "^VIX", "^VXN"),
    Name = c(
      "S&P 500",
      "NASDAQ",
      "Dow Jones",
      "S&P Volatility",
      "NASDAQ Volatility"
    )
  )
  
  ## Combining & Removing Dead Stocks
  Total_Stocks = bind_rows(Market_Tickers, Auto_Stocks)
    
 
   for (j in 1:2){
    Dump = list()
    p = progress_estimated(n = nrow(Total_Stocks), min_time = 3)
    for (i in 1:nrow(Total_Stocks)) {
      p$pause(0.1)$tick()$print()
      ticker = as.character(Total_Stocks$Symbol[i])
      
      if(j == 1){
        stockData = try(getSymbols(ticker,
                                   src = "yahoo",
                                   from = Sys.Date() - 257,
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
      }else{
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
    ## Consolidating to Data Frame
    list.condition <-
      sapply(Dump, function(x)
        class(x) == "data.frame")
    output.list  <- Dump[list.condition]
    Combined_Results = plyr::ldply(output.list, data.frame)
    
    ## Performing Liquidity Checks
    if(j == 1){
      CHECK = Combined_Results %>%
        group_by(Stock) %>%
        na.locf() %>%
        na.omit() %>%
        summarise(O = sum(Open == 0),
                  H = sum(High == 0),
                  L = sum(Low == 0),
                  C = sum(Close == 0),
                  V_AVG = median(Volume),
                  C_AVG = median(Close) - 2*mad(Close),
                  DV = V_AVG * C_AVG) %>%
        filter(O == 0,
               H == 0,
               L == 0,
               C == 0,
               C_AVG > 5,
               DV >= 2.5e6)
      
      ## Reducing to Highly Liquid Stocks
      Total_Stocks = Total_Stocks %>%
        filter(Symbol %in% c(CHECK$Stock,"^VIX","^VXN"))
    }
   }
  
  ## Saving Historical Market Data
    save(Combined_Results,
         file = paste0(Project_Folder, "/Data//NASDAQ Historical.RDATA"))
}