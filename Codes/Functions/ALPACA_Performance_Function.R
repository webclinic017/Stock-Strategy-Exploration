ALPACA_Performance_Function = function(TODAY,
                                       RESULT,
                                       Auto_Stocks,
                                       Project_Folder,
                                       Max_Holding = 0.10,
                                       PAPER = T,
                                       Rebalance = F){
  
  ########################## Setting API Keys ##########################
  KEYS = read.csv(paste0(Project_Folder,"/Data/Keys/Paper API.txt"))
  Sys.setenv('APCA-PAPER-API-KEY-ID' = KEYS$Key.ID)
  Sys.setenv('APCA-PAPER-API-SECRET-KEY' = KEYS$Secret.Key)
  KEYS = read.csv(paste0(Project_Folder,"/Data/Keys/Live API.txt"))
  Sys.setenv('APCA-LIVE-API-KEY-ID' = as.character(KEYS$Key.ID))
  Sys.setenv('APCA-LIVE-API-SECRET-KEY' = as.character(KEYS$Secret.Key))
  if(PAPER){
    Report_CSV = paste0(Project_Folder,"/data/Decison Tracking/Paper Choices.csv")
  }else{
    Report_CSV = paste0(Project_Folder,"/data/Decison Tracking/Live Choices.csv")
  }
  
  
  ########################## Getting Current Account Information ##########################
  ACCT_Status = get_account(live = !PAPER)
  Current_Holdings = try(get_positions(live = !PAPER) %>%
                           filter(side == "long"))
  Current_Orders = try(get_orders(status = 'all',live = !PAPER) %>%
                         filter(status == "new"),
                       silent = T)
 
  Investment_Value = as.numeric(ACCT_Status$portfolio_value)
  Buying_Power = as.numeric(ACCT_Status$cash)
  
  
  ########################## Investment Choice Reduction ########################## 
  
  LONG = RESULT$LONG
  
  ## Appending Sector / Industry Info
  Sector_Ind_DF = try(Current_Holdings %>%
                        left_join(Auto_Stocks,by = c("symbol" = "Symbol")) %>%
                        select(Sector,Industry),
                      silent = T)
  
  ## Checking 30 Day Wash Rule
  Wash_Sale_Record = try(get_orders(status = 'closed',
                                    from = Sys.Date() - 30,
                                    live = !PAPER) %>%
                           filter(status == "filled",
                                  type == "stop_limit" | type == "stop" | type == "market") %>%
                           mutate(filled_at = ymd_hms(filled_at)),
                         silent = T)
  if(!"try-error" %in% class(Wash_Sale_Record)){
    LONG = LONG %>%
      filter(!Stock %in% Wash_Sale_Record$symbol)
  }
  
  ## Removes Any Outside of Price Range
  LONG = LONG %>%
    filter(Close < Investment_Value*Max_Holding,
           !Stock %in% toupper(Current_Holdings$symbol)) %>%
    select(Sector,Industry,Decider,everything())
  
  ## Keeping Best Outlook Within Industry and Sector
  LONG = LONG %>%
    group_by(Sector,Industry) %>%
    filter(Decider == max(Decider)) %>%
    ungroup() %>%
    distinct()
  
  ## Prioritizing Sector & Industry Diversification ##
  if(!"try-error" %in% class(Sector_Ind_DF)){
    LONG = Diversification(LONG,Sector_Ind_DF)
  }
  
  
  ########################## Initial Purchase Orders ########################## 
  ## Defining Purchase Numbers
  K = 0
  Remaining_Money = Buying_Power
  Number = numeric(length = nrow(LONG))
  while(K < nrow(LONG)){
    K = K + 1
    counter = 0
    Price = LONG$Close[K]
    while(Price < Remaining_Money & (counter+1)*Price < Investment_Value*Max_Holding){
      counter = counter + 1
      Remaining_Money = Remaining_Money - Price
    }
    Number[K] = counter
  }
  Numbers = Number[which(Number > 0)]
  LONG = LONG[which(Number > 0),]
  
  ## Skipping if Buying Power Too Low to Invest (0 LONG Options)
  if(nrow(LONG) > 0){
    for(STOCK in 1:nrow(LONG)){
      
      ## Placing Limit Order (Extended Hours Enabled)
      submit_order(ticker = LONG$Stock[STOCK],
                   qty = as.character(Numbers[STOCK]),
                   side = "buy",
                   type = "limit",
                   time_in_force = "day",
                   live = !PAPER,
                   extended_hours = T,
                   limit_price = as.character(LONG$Close[STOCK]))
      
      ## Recording Decison in Log
      Report_Out = data.frame(Time = Sys.time(),
                              Stock = LONG$Stock[STOCK],
                              Qty = as.character(Numbers[STOCK]),
                              Side = "buy",
                              Type = "limit",
                              Price = as.character(LONG$Close[STOCK]),
                              Reason = "Probability")
      try(write_csv(x = Report_Out,
                    path = Report_CSV,
                    append = T))
    }
  } 
  
  
  
  ########################## Current Holding Order Updates ########################## 
  Ticker_List = c(Current_Holdings$symbol)
  
  if(!is_empty(Ticker_List)){
    ## Stop Loss and Market Sell Rules
    for(STOCK in Ticker_List){
      print(str_c("Running Long Logic On ", STOCK))  
      
        ##########################  Current Asset Information  ########################## 
        Current_Info = get_bars(ticker = STOCK,
                                limit = 1)[[STOCK]]
        Current_Forecast = RESULT$TOTAL %>%
          filter(Stock == STOCK) %>%
          head(1)
        Buy_Price = as.numeric(Current_Holdings$avg_entry_price[Current_Holdings$symbol == STOCK])
        Quantity = as.numeric(Current_Holdings$qty[Current_Holdings$symbol == STOCK])
        Pcent_Gain = (as.numeric(Current_Info$close)-Buy_Price)/Buy_Price
        
        ## Pulling Existing Loss_Order
        Loss_Order = try(
          get_orders(
            ticker = STOCK,
            status = 'open',
            live = !PAPER
          ) %>%
            filter(side == "sell") %>%
            filter(created_at == max(created_at)) %>%
            head(1),
          silent = T
        )
        
        
        
        ########################## Stop Loss Calculations ########################## 
        if(any(nrow(Loss_Order) == 0,"try-error" %in% class(Loss_Order))){
          print(str_c(STOCK," Setting Initial Stop Loss"))
          ## Initial Stop Loss Based On ATR
          Stop_Loss = max(c(
            Buy_Price - 2*head(TODAY$ATR[TODAY$Stock == STOCK & 
                                           TODAY$Date == max(TODAY$Date)],1),
            as.numeric(Current_Info$close) - 2*head(TODAY$ATR[TODAY$Stock == STOCK & 
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
          
          ## Recording In Decison Log
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
          
          if(Pcent_Gain > 0 & 
             any(ifelse(is_empty(Current_Forecast$Expected_Return_Long),
                    1,
                    Current_Forecast$Expected_Return_Long + Current_Forecast$Expected_Return_Short)/2 < 1,
                 ifelse(is_empty(Current_Forecast$Expected_Return_Short),
                        1,
                        Current_Forecast$Expected_Return_Short) < 1)
             ){
            print(str_c(STOCK," Profit Protection Sharpe Ratio < 1"))
            ## Canceling Existing Order
            cancel_order(ticker_id = STOCK,
                         live = !PAPER)
            Sys.sleep(3)
            AlpacaforR::submit_order(ticker = STOCK,
                                     qty = as.character(Quantity),
                                     side = "sell",
                                     type = "market",
                                     live = !PAPER,
                                     time_in_force = "gtc")
            
            
            ## Recording In Decison Log
            Report_Out = data.frame(Time = Sys.time(),
                                    Stock = STOCK,
                                    Qty = Quantity,
                                    Side = "sell",
                                    Type = "market",
                                    Price = Current_Info$close,
                                    Reason = "Sharpe Ratio < 1 Sell To Protect Profit")
            
            try(write_csv(x = Report_Out,
                          path = Report_CSV,
                          append = T))
            
          }else{
            
            Current_Stop_Loss = as.numeric(Loss_Order$stop_price)
            
            ## Determining New Stop Loss
            Stop_Loss = round(as.numeric(Current_Info$close) - 2*head(TODAY$ATR[TODAY$Stock == STOCK & 
                                                                                  TODAY$Date == max(TODAY$Date)],1),2)
            
            ## Updating Stop Loss if Higher
            if(all(Stop_Loss > Current_Stop_Loss,!is_empty(Stop_Loss))){
              print(str_c(STOCK," Updating Stop Loss"))
              ## Canceling Existing Order
              cancel_order(ticker_id = STOCK,
                           live = !PAPER)
              Sys.sleep(3)
              AlpacaforR::submit_order(ticker = STOCK,
                                       qty = as.character(Quantity),
                                       side = "sell",
                                       type = "stop_limit",
                                       limit_price = as.character((Stop_Loss)),
                                       stop_price = as.character((Stop_Loss)),
                                       live = !PAPER,
                                       time_in_force = "gtc")
              
              ## Defining Reason For Decison Log
              Reason = "Stop Loss Update"
              
              ## Writing Update To Log File
              Report_Out = data.frame(Time = Sys.time(),
                                      Stock = STOCK,
                                      Qty = Quantity,
                                      Side = "sell",
                                      Type = "stop",
                                      Price = Stop_Loss,
                                      Reason = Reason)
              
              ## Appending To Decison Log
              try(write_csv(x = Report_Out,
                            path = Report_CSV,
                            append = T))
            }
          }
        }
        
        ########################## Rebalancing Check ########################## 
        if(Rebalance){
          Market_Value = as.numeric(Current_Holdings$market_value[Current_Holdings$symbol == STOCK])
          if(Market_Value > Investment_Value*Max_Holding){
            Max_qty = floor(Investment_Value*Max_Holding/Current_Info$close)
            if(Max_qty > 0){
              print(str_c(STOCK," Rebalancing"))
              
              ## Determining New Spread
              Keep = Max_qty
              Sell = Quantity - Keep
              
              ## Pulling Existing Loss_Order
              Loss_Order = try(
                get_orders(
                  ticker = STOCK,
                  status = 'open',
                  live = !PAPER
                ) %>%
                  filter(side == "sell") %>%
                  filter(created_at == max(created_at)) %>%
                  head(1)
              )
              if(nrow(Loss_Order) > 0){
                ## Canceling Existing Order
                cancel_order(ticker_id = STOCK,
                             live = !PAPER)
                Sys.sleep(3)
              }
              AlpacaforR::submit_order(ticker = STOCK,
                                       qty = as.character(Keep),
                                       side = "sell",
                                       type = "stop_limit",
                                       limit_price = as.character(Loss_Order$limit_price),
                                       stop_price = as.character(Loss_Order$stop_price),
                                       live = !PAPER,
                                       time_in_force = "gtc")
              Sys.sleep(3)
              
              ## Placing Market Sell Order
              submit_order(ticker = STOCK,
                           qty = as.character(Sell),
                           side = "sell",
                           type = "market",
                           time_in_force = "gtc",
                           live = !PAPER)
              
              ## Writing Update To Log File
              Report_Out = data.frame(Time = Sys.time(),
                                      Stock = STOCK,
                                      Qty = Sell,
                                      Side = "sell",
                                      Type = "market",
                                      Price = Current_Info$close,
                                      Reason = "Rebalance Reduction")
              
              ## Appending To Decison Log
              try(write_csv(x = Report_Out,
                            path = Report_CSV,
                            append = T))
              
              
              
            }
          }
        }
    }
  }
  
  if(PAPER){
    ##########################  Shorting Option Reduction ########################## 
    ACCT_Status = get_account(live = !PAPER)
    Current_Holdings = try(get_positions(live = !PAPER) %>%
                             filter(side == "short"))
    Current_Orders = try(get_orders(status = 'all',live = !PAPER) %>%
                           filter(status == "new"),
                         silent = T)
    
    Investment_Value = as.numeric(ACCT_Status$portfolio_value)
    Buying_Power = as.numeric(ACCT_Status$regt_buying_power)
    
    ########################## Investment Choice Reduction ########################## 
    
    SHORT = RESULT$SHORT %>%
      mutate(Decider = -Decider)
    
    ## Appending Sector / Industry Info
    Sector_Ind_DF = try(Current_Holdings %>%
                          left_join(Auto_Stocks,by = c("symbol" = "Symbol")) %>%
                          select(Sector,Industry),
                        silent = T)
    
    ## Checking 30 Day Wash Rule
    Wash_Sale_Record = try(get_orders(status = 'closed',
                                      from = Sys.Date() - 30,
                                      live = !PAPER) %>%
                             filter(status == "filled",
                                    type == "stop_limit" | type == "stop" | type == "market") %>%
                             mutate(filled_at = ymd_hms(filled_at)),
                           silent = T)
    if(!"try-error" %in% class(Wash_Sale_Record)){
      SHORT = SHORT %>%
        filter(!Stock %in% Wash_Sale_Record$symbol)
    }
    
    
    
    ## Removes Any Outside of Price Range
    SHORT = SHORT %>%
      filter(Close < Investment_Value*Max_Holding,
             !Stock %in% toupper(Current_Holdings$symbol)) %>%
      select(Sector,Industry,Decider,everything())
    
    ## Keeping Best Outlook Within Industry and Sector
    SHORT = SHORT %>%
      group_by(Sector,Industry) %>%
      filter(Decider == max(Decider)) %>%
      ungroup() %>%
      distinct()
    
    ## Prioritizing Sector & Industry Diversification ##
    if(!"try-error" %in% class(Sector_Ind_DF)){
      SHORT = Diversification(SHORT,Sector_Ind_DF)
    }
    
    
    ########################## Initial Purchase Orders ########################## 
    ## Defining Purchase Numbers
    K = 0
    Remaining_Money = Buying_Power
    Number = numeric(length = nrow(SHORT))
    while(K < nrow(SHORT)){
      K = K + 1
      counter = 0
      Price = SHORT$Close[K]
      while(Price < Remaining_Money & (counter+1)*Price < Investment_Value*Max_Holding){
        counter = counter + 1
        Remaining_Money = Remaining_Money - Price
      }
      Number[K] = counter
    }
    Numbers = Number[which(Number > 0)]
    SHORT = SHORT[which(Number > 0),]
    
    ## Skipping if Buying Power Too Low to Invest (0 SHORT Options)
    if(nrow(SHORT) > 0){
      for(STOCK in 1:nrow(SHORT)){
        
        ## Placing Limit Order (Extended Hours Enabled)
        submit_order(ticker = SHORT$Stock[STOCK],
                     qty = as.character(Numbers[STOCK]),
                     side = "sell",
                     type = "limit",
                     time_in_force = "day",
                     live = !PAPER,
                     extended_hours = T,
                     limit_price = as.character(SHORT$Close[STOCK]))
        
        ## Recording Decison in Log
        Report_Out = data.frame(Time = Sys.time(),
                                Stock = SHORT$Stock[STOCK],
                                Qty = as.character(Numbers[STOCK]),
                                Side = "sell",
                                Type = "limit",
                                Price = as.character(SHORT$Close[STOCK]),
                                Reason = "Short Sell Probability")
        try(write_csv(x = Report_Out,
                      path = Report_CSV,
                      append = T))
      }
    } 
    
    ########################## Current Holding Order Updates ########################## 
    Ticker_List = c(Current_Holdings$symbol)
    
    if(!is_empty(Ticker_List)){
      
      ## Stop Loss and Market Sell Rules
      for(STOCK in Ticker_List){
        
        print(str_c("Running Short Logic On ", STOCK)) 
        ##########################  Current Asset Information  ########################## 
        Current_Info = get_bars(ticker = STOCK,
                                limit = 1)[[STOCK]]
        Buy_Price = as.numeric(Current_Holdings$avg_entry_price[Current_Holdings$symbol == STOCK])
        Quantity = abs(as.numeric(Current_Holdings$qty[Current_Holdings$symbol == STOCK]))
        Pcent_Gain = -(as.numeric(Current_Info$close)-Buy_Price)/Buy_Price
        
        ## Pulling Existing Loss_Order
        Loss_Order = try(
          get_orders(
            ticker = STOCK,
            status = 'open',
            live = !PAPER
          ) %>%
            filter(side == "buy") %>%
            filter(created_at == max(created_at)) %>%
            head(1)
        )
  
        
        
        ########################## Stop Loss Calculations ########################## 
        if(nrow(Loss_Order) == 0){
          print(str_c(STOCK," Setting Initial Buy Stop"))
          ## Initial Stop Loss Based On ATR
          Stop_Loss = max(c(
            Buy_Price + 2*head(TODAY$ATR[TODAY$Stock == STOCK & 
                                           TODAY$Date == max(TODAY$Date)],1),
            as.numeric(Current_Info$c) + 2*head(TODAY$ATR[TODAY$Stock == STOCK & 
                                                            TODAY$Date == max(TODAY$Date)],1)
          ))
          
          ## Placing Stop Loss Order
          submit_order(ticker = STOCK,
                       qty = as.character(Quantity),
                       side = "buy",
                       type = "stop",
                       time_in_force = "gtc",
                       stop_price = as.character(Stop_Loss),
                       live = !PAPER)
          
          ## Recording In Decison Log
          Report_Out = data.frame(Time = Sys.time(),
                                  Stock = STOCK,
                                  Qty = Quantity,
                                  Side = "buy",
                                  Type = "stop",
                                  Price = Stop_Loss,
                                  Reason = "Initial Short Stop Loss")
          try(write_csv(x = Report_Out,
                        path = Report_CSV,
                        append = T))
        }else{
          Current_Stop_Loss = as.numeric(Loss_Order$stop_price)
          
          
          ## Determining New Stop Loss
          Stop_Loss = as.numeric(Current_Info$close) + 2*head(TODAY$ATR[TODAY$Stock == STOCK & 
                                                            TODAY$Date == max(TODAY$Date)],1)
          
          if(is_empty(Stop_Loss)){
            print(str_c(STOCK," Buying No Longer Easy to Borrow If Positive Return"))
            if(Pcent_Gain > 0){
              ## Canceling Existing Order
              cancel_order(ticker_id = STOCK,
                           live = !PAPER)
              Sys.sleep(3)
              AlpacaforR::submit_order(ticker = STOCK,
                                       qty = as.character(Quantity),
                                       side = "buy",
                                       type = "market",
                                       stop_price = as.character(Stop_Loss),
                                       live = !PAPER,
                                       time_in_force = "gtc")
            }
          }else if(Stop_Loss < Current_Stop_Loss){
            print(str_c(STOCK," Updating Buy Stop"))
            ## Canceling Existing Order
            cancel_order(ticker_id = STOCK,
                         live = !PAPER)
            Sys.sleep(3)
            AlpacaforR::submit_order(ticker = STOCK,
                                     qty = as.character(Quantity),
                                     side = "buy",
                                     type = "stop",
                                     stop_price = as.character(Stop_Loss),
                                     live = !PAPER,
                                     time_in_force = "gtc")
            
            
            ## Defining Reason For Decison Log
            Reason = "Short Stop Loss Update"
            
            ## Writing Update To Log File
            Report_Out = data.frame(Time = Sys.time(),
                                    Stock = STOCK,
                                    Qty = Quantity,
                                    Side = "buy",
                                    Type = "stop",
                                    Price = Stop_Loss,
                                    Reason = Reason)
            
            ## Appending To Decison Log
            try(write_csv(x = Report_Out,
                          path = Report_CSV,
                          append = T))
          }
        }
        ########################## Rebalancing Check ########################## 
        if(Rebalance){
          Market_Value = as.numeric(Current_Holdings$market_value[Current_Holdings$symbol == STOCK])
          if(Market_Value > Investment_Value*Max_Holding){
            Max_qty = floor(Investment_Value*Max_Holding/Current_Info$close)
            if(Max_qty > 0){
              print(str_c(STOCK," Rebalancing Market Sells"))
              
              ## Determining New Spread
              Keep = Max_qty
              Sell = Quantity - Keep
              
              ## Pulling Existing Loss_Order
              Loss_Order = try(
                get_orders(
                  ticker = STOCK,
                  status = 'open',
                  live = !PAPER
                ) %>%
                  filter(side == "buy") %>%
                  filter(created_at == max(created_at)) %>%
                  head(1)
              )
              
              ## Canceling Existing Order
              cancel_order(ticker_id = STOCK,
                           live = !PAPER)
              Sys.sleep(3)
              AlpacaforR::submit_order(ticker = STOCK,
                                       qty = as.character(Keep),
                                       side = "buy",
                                       type = "stop",
                                       stop_price = as.character(Loss_Order$stop_price),
                                       live = !PAPER,
                                       time_in_force = "gtc")
              Sys.sleep(3)
              
              ## Placing Market Sell Order
              submit_order(ticker = STOCK,
                           qty = as.character(Sell),
                           side = "buy",
                           type = "market",
                           time_in_force = "gtc",
                           live = !PAPER)
              
              ## Writing Update To Log File
              Report_Out = data.frame(Time = Sys.time(),
                                      Stock = STOCK,
                                      Qty = Sell,
                                      Side = "buy",
                                      Type = "market",
                                      Price = Current_Info$close,
                                      Reason = "Short Rebalance Reduction")
              
              ## Appending To Decison Log
              try(write_csv(x = Report_Out,
                            path = Report_CSV,
                            append = T))
              
              
              
            }
          }
        } # End Rebalance
      } # End Stock Loop
    } 
  } # End Short Sell Portion
} # End Funtion
