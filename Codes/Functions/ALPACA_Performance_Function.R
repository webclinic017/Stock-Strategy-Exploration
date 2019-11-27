ALPACA_Performance_Function = function(TODAY,
                                       RESULT,
                                       Auto_Stocks,
                                       Project_Folder,
                                       Max_Holding = 0.10,
                                       PAPER = T){
  ## Setting API Keys
  if(PAPER){
    KEYS = read.csv(paste0(Project_Folder,"/Data/Keys/Paper API.txt"))
    Sys.setenv('APCA-API-KEY-ID' = KEYS$Key.ID)
    Sys.setenv('APCA-API-SECRET-KEY' = KEYS$Secret.Key)
    Report_CSV = paste0(Project_Folder,"data/Decison Tracking/Paper Choices.csv")
  }else{
    KEYS = read.csv(paste0(Project_Folder,"/Data/Keys/Live API.txt"))
    Sys.setenv('APCA-API-KEY-ID' = as.character(KEYS$Key.ID))
    Sys.setenv('APCA-API-SECRET-KEY' = as.character(KEYS$Secret.Key))
    Report_CSV = paste0(Project_Folder,"data/Decison Tracking/Live Choices.csv")
  }
  
  ## Diversification
  Diversification = function(RESULT){ 
    if(!"try-error" %in% class(Sector_Ind_DF)){
      ## Sector & Industry Filter
      TMP = RESULT %>%
        filter(!Industry %in% Sector_Ind_DF$Industry,
               !Sector %in% Sector_Ind_DF$Sector)
      
      if(nrow(TMP) != 0){
        return(TMP)
      }
       
      ## Sector Filter
      TMP = RESULT %>%
        filter(!Sector %in% Sector_Ind_DF$Sector)
      
      if(nrow(TMP) != 0){
        return(TMP)
      }
      
      ## Sector or Industry Filter
      TMP = RESULT %>%
        filter(!Industry %in% Sector_Ind_DF$Industry |
                 !Sector %in% Sector_Ind_DF$Sector)
      return(TMP)
    }
    return(RESULT)
  }
  
  ## Checking Account Status
  (ACCT_Status = get_account(live = !PAPER))
  
  ## Pulling Holinds
  Current_Holdings = get_positions(live = !PAPER)
  
  ## Appending Sector / Industry Info
  Sector_Ind_DF = try(Current_Holdings %>%
                        left_join(Auto_Stocks,by = c("symbol" = "Symbol")) %>%
                        select(Sector,Industry),
                      silent = T)
  
  ## Pulling Existing Orders
  Current_Orders = try(get_orders(status = 'all',live = !PAPER) %>%
                         filter(status == "new"),
                       silent = T)
  
  ## Pulling Filled Sell Orders During Wash Sale Window
  Wash_Sale_Record = try(get_orders(status = 'closed',
                                    from = Sys.Date() - 30,
                                    live = !PAPER) %>%
                           filter(status == "filled",
                                  type == "stop_limit" | type == "stop" | type == "market") %>%
                           mutate(filled_at = ymd_hms(filled_at)),
                         silent = T)
  
  ## Updating Capital 
  Investment_Value = as.numeric(ACCT_Status$portfolio_value)
  Buying_Power = as.numeric(ACCT_Status$cash)
  
  ## Checking 30 Day Wash Rule
  if(!"try-error" %in% class(Wash_Sale_Record)){
    RESULT = RESULT %>%
      filter(!Stock %in% Wash_Sale_Record$symbol)
  }
  
  ## Removes Any Outside of Price Range
  RESULT = RESULT %>%
    BUY_POS_FILTER() %>%
    filter(Close < Investment_Value*Max_Holding,
           !Stock %in% toupper(Current_Holdings$symbol)) %>%
    select(Sector,Industry,Decider,everything())
  
  ## Keeping Best Outlook Within Industry and Sector
  RESULT = RESULT %>%
    group_by(Sector,Industry) %>%
    filter(Decider == max(Decider)) %>%
    ungroup() %>%
    distinct()
  
  ## Prioritizing Sector & Industry Diversification ##
  if(!"try-error" %in% class(Sector_Ind_DF)){
    RESULT = Diversification(RESULT)
  }
  
  ## Defining Purchase Numbers
  K = 0
  Remaining_Money = Buying_Power
  Number = numeric(length = nrow(RESULT))
  while(K < nrow(RESULT)){
    K = K + 1
    counter = 0
    Price = RESULT$Close[K]
    while(Price < Remaining_Money & (counter+1)*Price < Investment_Value*Max_Holding){
      counter = counter + 1
      Remaining_Money = Remaining_Money - Price
    }
    Number[K] = counter
  }
  Numbers = Number[which(Number > 0)]
  RESULT = RESULT[which(Number > 0),]
  
  ## Skipping if Buying Power Too Low to Invest (0 RESULT Options)
  if(nrow(RESULT) > 0){
    for(STOCK in 1:nrow(RESULT)){
      submit_order(ticker = RESULT$Stock[STOCK],
                   qty = as.character(Numbers[STOCK]),
                   side = "buy",
                   type = "limit",
                   time_in_force = "day",
                   live = !PAPER,
                   extended_hours = T,
                   limit_price = as.character(RESULT$Close[STOCK]),
                   client_order_id = str_c("T",
                                           round(RESULT$Expected_Return[STOCK],4),
                                           "S",
                                           round(RESULT$Stop_Loss[STOCK],4),
                                           "D",
                                           Sys.Date()))
      Report_Out = data.frame(Time = Sys.time(),
                              Stock = RESULT$Stock[STOCK],
                              Qty = as.character(Numbers[STOCK]),
                              Side = "Buy",
                              Type = "Limit",
                              Price = as.character(RESULT$Close[STOCK]),
                              Reason = "Probability")
      try(write_csv(x = Report_Out,
                    path = Report_CSV,
                    append = T))
    }
  } 
  
  
  Ticker_List = c(Current_Holdings$symbol)
  if(!is_empty(Ticker_List)){
    ## Updating Holdings
    Current_Holdings = get_positions(live = !PAPER)
    Current_Orders = try(get_orders(status = 'all',live = !PAPER) %>%
                           filter(status == "new"))
    Filled_Orders = try(get_orders(status = 'all',
                                   from = Sys.Date() - 365,
                                   live = !PAPER) %>%
                          filter(status == "filled",
                                 type == "limit" | type == 'stop_limit') %>%
                          mutate(filled_at = ymd_hms(filled_at)))
    
    ## Stop Loss and Market Sell Rules
    for(STOCK in Ticker_List){
      ## Pulling Recent Close Price / Relevant Data
      Current_Info = get_bars(ticker = STOCK,
                              limit = 1)
      Buy_Price = as.numeric(Current_Holdings$avg_entry_price[Current_Holdings$symbol == STOCK])
      Quantity = as.numeric(Current_Holdings$qty[Current_Holdings$symbol == STOCK])
      Pcent_Gain = (as.numeric(Current_Info$c)-Buy_Price)/Buy_Price
      Loss_Order = Current_Orders[Current_Orders$symbol == STOCK,]
      
      ## Pulling Profit Target From Client Order ID
      Buy_Order = Filled_Orders %>%
        filter(symbol == STOCK,
               side == "buy") %>%
        filter(filled_at == max(filled_at))
      Target_Pcent_Gain = as.numeric(str_extract(Buy_Order$client_order_id,"(?<=T).+(?=S)"))
      if(is_empty(Target_Pcent_Gain)){
        Target_Pcent_Gain = -1
      }
      
      ## Pulling Current Technical Indicators
      FinViz_Metrics = FinViz_Meta_Data(data.frame(Stock = STOCK))
      
      
      ## Calculating Stop Loss
      if(nrow(Loss_Order) == 0){
        ## Determining Stop Loss
        Stop_Loss = max(c(
          Buy_Price - 2*head(TODAY$ATR[TODAY$Stock == STOCK & 
                                         TODAY$Date == max(TODAY$Date)],1),
          as.numeric(Current_Info$c) - 2*head(TODAY$ATR[TODAY$Stock == STOCK & 
                                                          TODAY$Date == max(TODAY$Date)],1)
        ))
        
        ## Placing Stop Loss Order
        submit_order(ticker = STOCK,
                     qty = as.character(Quantity),
                     side = "sell",
                     type = "stop_limit",
                     time_in_force = "gtc",
                     stop_price = as.character(Stop_Loss),
                     limit_price = as.character(Stop_Loss),
                     live = !PAPER)
        Report_Out = data.frame(Time = Sys.time(),
                                Stock = STOCK,
                                Qty = Quantity,
                                Side = "sell",
                                Type = "stop",
                                Price = Stop_Loss,
                                Reason = "Initial Stop Loss")
        try(write_csv(x = Report_Out,
                      path = Report_CSV,
                      append = T))
      }else{
        ## Pulling Current Stop Loss
        Current_Stop_Loss = as.numeric(Loss_Order$stop_price)
        
        ## Bumping Loss Order Up If Target Met
        if(Pcent_Gain >= Target_Pcent_Gain){
          Target_Stop = Buy_Price*Target_Pcent_Gain + Buy_Price
        }else{
          Target_Stop = 0
        }
        
        ## Determining New Stop Loss
        Stop_Loss = max(c(
          as.numeric(Current_Info$c) - 2*head(TODAY$ATR[TODAY$Stock == STOCK & 
                                                          TODAY$Date == max(TODAY$Date)],1),
          Target_Stop))
        
        ## Updating Stop Loss if Higher
        if(Stop_Loss > Current_Stop_Loss){
          
          ## Updating Exisiting Order
          patch_order(order_id = Loss_Order$id,
                      qty = as.character(Quantity),
                      time_in_force = "gtc",
                      stop_price = as.character(Stop_Loss),
                      limit_price = as.character(Stop_Loss),
                      live = !PAPER)
          
          ## Wrtining Update To Tracking File
          Report_Out = data.frame(Time = Sys.time(),
                                  Stock = STOCK,
                                  Qty = Quantity,
                                  Side = "sell",
                                  Type = "stop",
                                  Price = Stop_Loss,
                                  Reason = "Stop Loss Update")
          
          ## Appending To Decison Log
          try(write_csv(x = Report_Out,
                        path = Report_CSV,
                        append = T))
        }
      }
    }
  } 
}
