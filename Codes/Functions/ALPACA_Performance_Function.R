ALPACA_Performance_Function = function(PR_Stage_R3,
                                       RESULT,
                                       FUTURES,
                                       SHORTS,
                                       Auto_Stocks,
                                       Project_Folder,
                                       Max_Holding = 0.10,
                                       Projection = 15,
                                       Max_Loss = 0.05,
                                       PAPAER = T){
  ## Setting API Keys
  if(PAPAER){
    KEYS = read.csv(paste0(Project_Folder,"/Data/Abram Paper API.txt"))
    Sys.setenv('APCA-API-KEY-ID' = KEYS$Key.ID)
    Sys.setenv('APCA-API-SECRET-KEY' = KEYS$Secret.Key)
  }else{
    KEYS = read.csv(paste0(Project_Folder,"/Data/Abram Live API.txt"))
    Sys.setenv('APCA-API-KEY-ID' = KEYS$Key.ID)
    Sys.setenv('APCA-API-SECRET-KEY' = KEYS$Secret.Key)
  }
  
  ## Checking Account Status
  ACCT_Status = get_account()
  Current_Holdings = get_positions()
  Sector_Ind_DF = try(Current_Holdings %>%
    left_join(Auto_Stocks,by = c("symbol" = "Symbol")) %>%
    select(Sector,Industry))
  Current_Orders = try(get_orders(status = 'all') %>%
    filter(status == "new"))
  Filled_Orders = try(get_orders(status = 'all') %>%
    filter(status == "filled",
           type == "limit") %>%
    mutate(filled_at = ymd_hms(filled_at)))
  
  ## Updating Capital 
  Investment_Value = as.numeric(ACCT_Status$portfolio_value)
  Buying_Power = as.numeric(ACCT_Status$buying_power)
  
  ## Removes Any Outside of Price Range
  RESULT = RESULT %>%
    BUY_POS_FILTER() %>%
    filter(Close < Investment_Value*Max_Holding,
           !Stock %in% toupper(Current_Holdings$symbol)) %>%
    left_join(Auto_Stocks,by = c("Stock" = "Symbol")) %>%
    filter(!Sector %in% Sector_Ind_DF$Sector |
             !Industry %in% Sector_Ind_DF$Industry) %>%
    select(Sector,Industry,Decider,everything()) %>%
    group_by(Sector,Industry) %>%
    filter(Decider == max(Decider)) %>%
    ungroup()
  
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
  
  ## Submitting Limit Orders
  for(STOCK in 1:nrow(RESULT)){
    submit_order(ticker = RESULT$Stock[STOCK],
                 qty = as.character(Numbers[STOCK]),
                 side = "buy",
                 type = "limit",
                 time_in_force = "day",
                 limit_price = as.character(RESULT$Close[STOCK]))
  }
  
  ## Running Checks For Currently Held Stocks
  Ticker_List = c(Current_Holdings$symbol)
  if(!is_empty(Ticker_List)){
    for(STOCK in Ticker_List){
      ## Pulling Recent Close Price / Relevant Data
      Current_Info = get_bars(ticker = STOCK,limit = 1)[[STOCK]]
      Buy_Price = as.numeric(Current_Holdings$avg_entry_price[Current_Holdings$symbol == STOCK])
      Quantity = as.numeric(Current_Holdings$qty[Current_Holdings$symbol == STOCK])
      Time_Held = floor(as.numeric(difftime(Sys.time(),
                                            max(Filled_Orders$filled_at[Filled_Orders$symbol == STOCK]),
                                            units = "days")))
      Pcent_Gain = (as.numeric(Current_Info$c)-Buy_Price)/Buy_Price
      Loss_Order = Current_Orders[Current_Orders$symbol == STOCK,]
      Market_Sell = F
      
      ## Updating Future Estimate
      if(STOCK %in% FUTURES$Stock){
        Delta = FUTURES$Delta[FUTURES$Stock == STOCK]
      }else if(STOCK %in% SHORTS$Stock){
        Delta = SHORTS$Delta[SHORTS$Stock == STOCK]
      }
      
      ## Stop Loss Override Checks
      if(Time_Held >= Projection){
        ## Selling if Negative After Projection Time Frame
        if(as.numeric(Current_Info$c) < Buy.Price){
          if(nrow(Loss_Order) != 0){
            cancel_order(ticker = STOCK,order_id = Loss_Order$id)
            Sys.sleep(10)
          }
          submit_order(ticker = STOCK,
                       qty = Quantity,
                       side = "sell",
                       type = "market",
                       time_in_force = "gtc")
          Market_Sell = T
        }
        if(Delta + Pcent_Gain <= Pcent_Gain*0.5){
          if(nrow(Loss_Order) != 0){
            cancel_order(ticker = STOCK,order_id = Loss_Order$id)
            Sys.sleep(10)
          }
          submit_order(ticker = STOCK,
                       qty = Quantity,
                       side = "sell",
                       type = "market",
                       time_in_force = "gtc")
          Market_Sell = T
        }
      }
      
      ## Calculating Stop Loss
      if(!Market_Sell){
        if(nrow(Loss_Order) == 0){
          ## Determining Stop Loss
          Stop_Loss = max(c(
            Buy_Price*(1-Max_Loss),
            Buy_Price - 2*head(PR_Stage_R3$ATR[PR_Stage_R3$Stock == STOCK & 
                                                 PR_Stage_R3$Date == max(PR_Stage_R3$Date)],1)))
          
          ## Placing Stop Loss Order
          submit_order(ticker = STOCK,
                       qty = as.character(Quantity),
                       side = "sell",
                       type = "stop",
                       time_in_force = "gtc",
                       stop_price = as.character(Stop_Loss))
        }else{
          ## Pulling Current Stop Loss
          Current_Stop_Loss = as.numeric(Loss_Order$stop_price)
          
          ## Determining New Stop Loss
          Stop_Loss = max(c(
            as.numeric(Current_Info$c)*(1-Max_Loss),
            as.numeric(Current_Info$c) - 2*head(PR_Stage_R3$ATR[PR_Stage_R3$Stock == STOCK & 
                                                                  PR_Stage_R3$Date == max(PR_Stage_R3$Date)],1)))
          
          ## Updating Stop Loss if Higher
          if(Stop_Loss > Current_Stop_Loss){
            ## Canceling Exisiting Order
            cancel_order(ticker = STOCK,order_id = Loss_Order$id)
            Sys.sleep(10)
            submit_order(ticker = STOCK,
                         qty = as.character(Quantity),
                         side = "sell",
                         type = "stop",
                         time_in_force = "gtc",
                         stop_price = as.character(Stop_Loss))
          }
        }
      }
    }
  }
}