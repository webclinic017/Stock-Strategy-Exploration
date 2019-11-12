Initial_Pull = function() {
  ## Updating Stock List
  Auto_Stocks = stockSymbols() %>%
    filter(!is.na(Industry),
           !is.na(Sector),
           !is.na(MarketCap))
  
  Auto_Stocks = Auto_Stocks %>%
    mutate(Multiplier = str_extract(MarketCap,"\\w$"),
           Value = str_extract(MarketCap,"\\d+")) %>%
    filter(str_detect(Multiplier,"M|B")) %>%
    mutate(New_Cap = case_when(
      Multiplier == "M" ~ as.numeric(Value) * 1e6,
      Multiplier == "B" ~ as.numeric(Value) * 1e9
    )) %>%
    filter(New_Cap > 300e6) %>%
    mutate(Cap_Type = case_when(
      New_Cap > 300e9 ~ "Mega",
      New_Cap > 10e9 ~ "Large",
      New_Cap > 2e9 ~ "Mid",
      New_Cap > 300e6 ~ "Small"
    )) %>%
    select(-c(IPOyear,Exchange,Multiplier,Value))
  
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
  Total_Stocks = bind_rows(Market_Tickers, Auto_Stocks) %>%
    filter(!str_detect(str_trim(Symbol),
                       paste0("^ARII$|^ATHN$|^BHBK$|^BLMT$|^ECYT$|^ESRX$|^GNBC$|^HDP$|^KANG$|^IDTI$",
                       "|^LOXO$|^NXTM$|^PBSK$|^SODA$|^SONC$|^TSRO$|^NAVG$|^ULTI$|^WTW$|^AHL$",
                       "|^BJZ$|^BPK$|^DM$|^DSW$|^ECC$|^ETX$|^ELLI$|^FCB$|^FBR$|^HTGX$|^LHO$",
                       "|^KORS$|^MSF$|^NFX$|^SSWN$|^SEP$|^TLP$|^VLP$|^VZA$|^WGP$|^ORM$|^DEACU$",
                       "|^SVA$|^UBS$")))
 
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
    
    ## Performing Liquidity Checks
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
    
    ## Reducing to Highly Liquid Stocks
    Total_Stocks = Total_Stocks %>%
      filter(Symbol %in% c(CHECK$Stock,"^VIX","^VXN"))
  }
  
  save(Combined_Results,
       file = paste0(Project_Folder, "/Data//NASDAQ Historical.RDATA"))
}