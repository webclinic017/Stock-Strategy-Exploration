ALPACA_Performance_Function = function(ID_DF,
                                       RESULT,
                                       Auto_Stocks,
                                       Project_Folder,
                                       Max_Holding = 0.10,
                                       Max_Loss = 0.05,
                                       Target = 0.40,
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
      ## Running Ordered Diversification Checks
      TMP = RESULT %>%
        filter(!Sector %in% Sector_Ind_DF$Sector)
      
      if(nrow(TMP) == 0){
        TMP = RESULT %>%
          filter(!Industry %in% Sector_Ind_DF$Industry,
                 !Sector %in% Sector_Ind_DF$Sector)
      }else{
        return(TMP)
      }
      
      if(nrow(TMP) == 0){
        TMP = RESULT %>%
          filter(!Industry %in% Sector_Ind_DF$Industry |
                   !Sector %in% Sector_Ind_DF$Sector)
      }else{
        return(TMP)
      }
      
      if(nrow(TMP) != 0){
        return(TMP)
      }
    }
    return(RESULT)
  }
  
  ## Checking Account Status
  ACCT_Status = get_account(live = !PAPER)
  
  ## Pulling Holinds
  Current_Holdings = get_positions(live = !PAPER)
  
  ## Appending Sector / Industry Info
  Sector_Ind_DF = try(Current_Holdings %>%
    left_join(Auto_Stocks,by = c("symbol" = "Symbol")) %>%
    select(Sector,Industry))
  
  ## Pulling Existing Orders
  Current_Orders = try(get_orders(status = 'all',live = !PAPER) %>%
    filter(status == "new"))
  
  ## Pulling Filled Sell Orders During Wash Sale Window
  Wash_Sale_Record = try(get_orders(status = 'closed',
                                    from = Sys.Date() - 30,
                                    live = !PAPER) %>%
                           filter(status == "filled",
                                  type == "stop_limit" | type == "stop" | type == "market") %>%
                           mutate(filled_at = ymd_hms(filled_at)))
  
  ## Updating Capital 
  Investment_Value = as.numeric(ACCT_Status$portfolio_value)
  Buying_Power = as.numeric(ACCT_Status$buying_power)
  
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
  
  ## Prioritizing Sector & Industry Diversification
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
                   limit_price = as.character(RESULT$Close[STOCK]))
      Report_Out = data.frame(Time = Sys.time(),
                              Stock = RESULT$Stock[STOCK],
                              Qty = as.character(Numbers[STOCK]),
                              Side = "Buy",
                              Type = "Limit",
                              Price = as.character(RESULT$Close[STOCK]),
                              Reason = "Probability")
      write_csv(x = Report_Out,
                path = Report_CSV,
                append = T)
    }
  } 
  
  
  Ticker_List = c(Current_Holdings$symbol)
  if(!is_empty(Ticker_List)){
    ## Updating Holdings
    Sys.sleep(10)
    Current_Holdings = get_positions(live = !PAPER)
    Current_Orders = try(get_orders(status = 'all',live = !PAPER) %>%
                           filter(status == "new"))
    Filled_Orders = try(get_orders(status = 'all',
                                   live = !PAPER) %>%
                          filter(status == "filled",
                                 type == "limit") %>%
                          mutate(filled_at = ymd_hms(filled_at)))
    
    ## Stop Loss and Market Sell Rules
    for(STOCK in Ticker_List){
      ## Pulling Recent Close Price / Relevant Data
      Current_Info = get_bars(ticker = STOCK,
                              limit = 1)[[STOCK]]
      Buy_Price = as.numeric(Current_Holdings$avg_entry_price[Current_Holdings$symbol == STOCK])
      Quantity = as.numeric(Current_Holdings$qty[Current_Holdings$symbol == STOCK])
      Pcent_Gain = (as.numeric(Current_Info$c)-Buy_Price)/Buy_Price
      Loss_Order = Current_Orders[Current_Orders$symbol == STOCK,]
      Market_Sell = F
      
      
      ## Calculating Stop Loss
      if(!Market_Sell){
        if(nrow(Loss_Order) == 0){
          ## Determining Stop Loss
          Stop_Loss = max(c(
            Buy_Price*(1-Max_Loss),
            Buy_Price - 2*head(ID_DF$ATR[ID_DF$Stock == STOCK & 
                                                 ID_DF$Date == max(ID_DF$Date)],1),
            as.numeric(Current_Info$c)*(1-Max_Loss),
            as.numeric(Current_Info$c) - 2*head(ID_DF$ATR[ID_DF$Stock == STOCK & 
                                                                  ID_DF$Date == max(ID_DF$Date)],1)
            ))
          
          ## Placing Stop Loss Order
          submit_order(ticker = STOCK,
                       qty = as.character(Quantity),
                       side = "sell",
                       type = "stop",
                       time_in_force = "gtc",
                       stop_price = as.character(Stop_Loss),
                       live = !PAPER)
          Report_Out = data.frame(Time = Sys.time(),
                                  Stock = STOCK,
                                  Qty = Quantity,
                                  Side = "sell",
                                  Type = "stop",
                                  Price = Stop_Loss,
                                  Reason = "Initial Stop Loss")
          write_csv(x = Report_Out,
                    path = Report_CSV,
                    append = T)
        }else{
          ## Pulling Current Stop Loss
          Current_Stop_Loss = as.numeric(Loss_Order$stop_price)
          
          ## Determining New Stop Loss
          Stop_Loss = max(c(
            as.numeric(Current_Info$c)*(1-Max_Loss),
            as.numeric(Current_Info$c) - 2*head(ID_DF$ATR[ID_DF$Stock == STOCK & 
                                                                  ID_DF$Date == max(ID_DF$Date)],1)))
          
          ## Updating Stop Loss if Higher
          if(Stop_Loss > Current_Stop_Loss){
            ## Canceling Exisiting Order
            cancel_order(ticker = STOCK,
                         order_id = Loss_Order$id,
                         live = !PAPER)
            Sys.sleep(10)
            submit_order(ticker = STOCK,
                         qty = as.character(Quantity),
                         side = "sell",
                         type = "stop",
                         time_in_force = "gtc",
                         stop_price = as.character(Stop_Loss),
                         live = !PAPER)
            Report_Out = data.frame(Time = Sys.time(),
                                    Stock = STOCK,
                                    Qty = Quantity,
                                    Side = "sell",
                                    Type = "stop",
                                    Price = Stop_Loss,
                                    Reason = "Stop Loss Update")
            write_csv(x = Report_Out,
                      path = Report_CSV,
                      append = T)
          }
        }
      }
    }
  }
  ## Rebalancing Well Performing Stocks
  ## Updating Holdings
  Sys.sleep(10)
  ## Checking Account Status
  ACCT_Status = get_account(live = !PAPER)
  ## Updating Capital 
  Investment_Value = as.numeric(ACCT_Status$portfolio_value)
  Buying_Power = as.numeric(ACCT_Status$buying_power)
  
  Current_Holdings = get_positions(live = !PAPER) %>%
    filter(unrealized_plpc > 0) %>%
    arrange(desc(unrealized_plpc)) %>%
    filter(as.numeric(market_value) + as.numeric(current_price) < Investment_Value*Max_Holding) %>%
    filter(as.numeric(current_price) < Buying_Power)
  
  ## Purchasing More Stocks if Money Allows
  ## Defining Purchase Numbers
  K = 0
  Remaining_Money = Buying_Power
  Number = numeric(length = nrow(Current_Holdings))
  while(K < nrow(Current_Holdings)){
    K = K + 1
    counter = 0
    Price = as.numeric(Current_Holdings$current_price[K])
    while(Price < Remaining_Money & (counter+1)*Price < Investment_Value*Max_Holding){
      counter = counter + 1
      Remaining_Money = Remaining_Money - Price
    }
    Number[K] = counter
  }
  Numbers = Number[which(Number > 0)]
  Rebalance = Current_Holdings[which(Number > 0),]
  
  ## Skipping if no stocks meet criteria
  if(nrow(Rebalance) > 0){
    for(STOCK in 1:nrow(Rebalance)){
      submit_order(ticker = Rebalance$symbol[STOCK],
                   qty = as.character(Numbers[STOCK]),
                   side = "buy",
                   type = "limit",
                   time_in_force = "day",
                   live = !PAPER,
                   limit_price = as.character(Rebalance$current_price[STOCK]))
      Report_Out = data.frame(Time = Sys.time(),
                              Stock = Rebalance$symbol[STOCK],
                              Qty = as.character(Numbers[STOCK]),
                              Side = "Buy",
                              Type = "Limit",
                              Price = as.character(Rebalance$current_price[STOCK]),
                              Reason = "Rebalance Strong Performers")
      write_csv(x = Report_Out,
                path = Report_CSV,
                append = T)
    }
  } 
  
}
