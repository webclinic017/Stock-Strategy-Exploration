Initial_Pull = function(Cap = "All",
                        Source = "Y",
                        Max_Single_Investment,
                        Min_Single_Investment,
                        Required_Packages,
                        Debug_Save = F) {
 
   if(!Source %in% c("Y","A")){
    stop("Source must be either 'Y' (Yahoo) or 'A' (Alpaca) in Initial_Pull function.")
  }
  
  require(doParallel)
  require(iterators)
  require(doSNOW)
  
  ## Spinning down any left open clusters
  on.exit(installr::kill_all_Rscript_s())
  
  if(Debug_Save){
    save(list = methods::formalArgs(Initial_Pull),
         file = str_c(Project_Folder,"/Debug/Initial_Pull.RDATA"))
    load(file = str_c(Project_Folder,"/Debug/Initial_Pull.RDATA"))
  }
  
  ## Updating Stock List
  Auto_Stocks = stockSymbols() %>%
    filter(!is.na(Industry),
           !is.na(Sector),
           !is.na(MarketCap),
           LastSale <= Max_Single_Investment,
           LastSale >= Min_Single_Investment)
  
  ## Setting API Keys
  KEYS = read.csv(paste0(Project_Folder,"/Data/Keys/Paper API.txt"))
  Sys.setenv('APCA-PAPER-API-KEY-ID' = KEYS$Key.ID)
  Sys.setenv('APCA-PAPER-API-SECRET-KEY' = KEYS$Secret.Key)
  KEYS = read.csv(paste0(Project_Folder,"/Data/Keys/Live API.txt"))
  Sys.setenv('APCA-LIVE-API-KEY-ID' = as.character(KEYS$Key.ID))
  Sys.setenv('APCA-LIVE-API-SECRET-KEY' = as.character(KEYS$Secret.Key))
  
  ## Pulling Available
  Alpaca_Stocks = get_assets() %>%
    filter(status == "active",
           tradable,
           marginable,
           shortable,
           easy_to_borrow)
  
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
    select(-c(IPOyear,Exchange,Multiplier,Value)) 
  
  ## Filtering to Specific 
  if(all(Cap != "All")){
      Auto_Stocks = Auto_Stocks %>%
        filter(Cap_Type %in% Cap)
  }
  
  ## Removing Duplicate Tickers
  Dups = duplicated(Auto_Stocks$Symbol)
  Auto_Stocks = Auto_Stocks[!Dups,]
  
  DCFs = pbmapply(DCF_Update,
                  Auto_Stocks$Symbol,
                  SIMPLIFY = T)
  DCFs[is.na(DCFs)] = F
  Auto_Stocks = Auto_Stocks[Auto_Stocks$Symbol %in% names(DCFs),]
  
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
    
  ## Creating Clusters
  c1 = makeCluster(detectCores())
  registerDoSNOW(c1)
  
  Dump = list()
  
  p <- progress_estimated(nrow(Total_Stocks))
  progress <- function(n) p$tick()$print()
  opts <- list(progress = progress)
  
  tickers = Total_Stocks$Symbol
  
  Dump = foreach(i = tickers,
                 .errorhandling = "stop",
                 .inorder = F,
                 .options.snow = opts,
                 .packages = Required_Packages) %dopar%{
                   
                   ticker = i
                   
                   if(Source == "Y"){
                     stockData = try(getSymbols(ticker,
                                                src = "yahoo",
                                                from = Sys.Date() - 365*3,
                                                auto.assign = FALSE) %>%
                                       as.data.frame() %>%
                                       mutate(Date = ymd(rownames(.))) %>%
                                       select(-6))
                   }
                   if(Source == "A"){
                     stockData = try(get_bars(ticker,
                                              from = Sys.Date() - 365*3) %>%
                                       select("Open" = o,
                                              "High" = h,
                                              "Low" = l,
                                              "Close" = c,
                                              "Volume" = v,
                                              "Date" = d) %>%
                                       mutate(Date = ymd(Date)),
                                     silent = T)
                   }
                   if ("try-error" %in% class(stockData)) {
                     stockData
                   }else{
                     colnames(stockData) = c("Open",
                                             "High",
                                             "Low",
                                             "Close",
                                             "Volume",
                                             "Date")
                     stockData$Stock = ticker
                     stockData
                   }
                 }
  
  
  ## Consolidating to Data Frame
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
  
  Combined_Results = Combined_Results %>%
    filter(Stock %in% Total_Stocks$Symbol)
  
  ## Saving Historical Market Data
  save(Combined_Results,
       file = paste0(Project_Folder, "/Data//NASDAQ Historical.RDATA"))
}
