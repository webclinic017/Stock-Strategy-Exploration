ALPACA_Performance_Function = function(TODAY,
                                       RESULT,
                                       Auto_Stocks,
                                       Project_Folder,
                                       Max_Holding = 0.10,
                                       PAPER = T){
  
  ########################## Setting API Keys ##########################
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
  
  
  ########################## Getting Current Account Information ##########################
  ACCT_Status = get_account(live = !PAPER)
  Current_Holdings = get_positions(live = !PAPER)
  Current_Orders = try(get_orders(status = 'all',live = !PAPER) %>%
                         filter(status == "new"),
                       silent = T)
 
  Investment_Value = as.numeric(ACCT_Status$portfolio_value)
  Buying_Power = as.numeric(ACCT_Status$cash)
  ## Determining Rebalance (1st Trading Day Of Month)
  Start = get_calendar(str_c(year(now()),
                             "-",
                             month(now()),
                             "-01")) %>%
    head(1)
  Rebalance = as_date(now()) == Start$date[1]
  
  
  ########################## Investment Choice Reduction ########################## 
  
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
  
  
  ########################## Initial Purchase Orders ########################## 
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
      
      ## Placing Limit Order (Extended Hours Enabled)
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
      
      ## Recording Decison in Log
      Report_Out = data.frame(Time = Sys.time(),
                              Stock = RESULT$Stock[STOCK],
                              Qty = as.character(Numbers[STOCK]),
                              Side = "buy",
                              Type = "limit",
                              Price = as.character(RESULT$Close[STOCK]),
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
      
      ##########################  Current Asset Information  ########################## 
      Current_Info = get_bars(ticker = STOCK,
                              limit = 1)
      Buy_Price = as.numeric(Current_Holdings$avg_entry_price[Current_Holdings$symbol == STOCK])
      Quantity = as.numeric(Current_Holdings$qty[Current_Holdings$symbol == STOCK])
      Pcent_Gain = (as.numeric(Current_Info$c)-Buy_Price)/Buy_Price
     
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
      
      ## Pulling Profit Target From Client Order ID
      Buy_Order = try(
        get_orders(
          ticker = STOCK,
          status = 'closed',
          live = !PAPER
        ) %>%
          filter(status == 'filled',
                 side == "buy") %>%
          filter(filled_at == max(filled_at)) %>%
          head(1)
      )
      Target_Pcent_Gain = as.numeric(str_extract(Buy_Order$client_order_id,"(?<=T).+(?=S)"))
      if(is_empty(Target_Pcent_Gain)){
        Target_Pcent_Gain = -1
      }
      
      
      ########################## Stop Loss Calculations ########################## 
      if(nrow(Loss_Order) == 0){
        
        ## Initial Stop Loss Based On ATR
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
        Current_Stop_Loss = as.numeric(Loss_Order$stop_price)
        
        ## Bumping Loss Order Up If Profit Target Met
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
          
          ## Defining Reason For Decison Log
          if(Stop_Loss == Target_Stop){
            Reason = "Profit Target Met"
          }else{
            Reason = "Stop Loss Update"
          }
          
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
      ########################## Rebalancing Check ########################## 
      if(Rebalance){
        Market_Value = as.numeric(Current_Holdings$market_value[Current_Holdings$symbol == STOCK])
        if(Market_Value > Investment_Value*Max_Holding){
          Max_qty = floor(Investment_Value*Max_Holding/Current_Info$c)
          if(Max_qty > 0){
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
            
            ## Updating Exisiting Order
            patch_order(order_id = Loss_Order$id,
                        qty = as.character(Keep),
                        time_in_force = "gtc",
                        live = !PAPER)
            
            Sys.sleep(10)
            
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
                                    Price = Current_Info$c,
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
}
