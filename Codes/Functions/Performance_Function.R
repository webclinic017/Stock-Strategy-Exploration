Performance_Function = function(PR_Stage_R3,
                                RESULT,
                                FUTURES,
                                SHORTS,
                                Fear_Ind,
                                Market_Ind,
                                Starting_Money = 10000,
                                Max_Holding = 0.10,
                                Projection = 15,
                                Target = 0.40,
                                Max_Loss = 0.05,
                                Current_Date,
                                Fear_Marker,
                                History_Table = NA){
  
  ## Builds Initial History Table
  if("logical" %in% class(History_Table)){
    ## Removes Any Outside of Price Range
    RESULT = RESULT %>%
      BUY_POS_FILTER() %>%
      filter(Close < Starting_Money*Max_Holding)
    
    
    K = 0
    Total_Capital = Starting_Money
    Remaining_Money = Starting_Money
    Number = numeric(length = nrow(RESULT))
    while(K < nrow(RESULT)){
      K = K + 1
      counter = 0
      Price = RESULT$Close[K]
      while(Price < Remaining_Money & (counter+1)*Price < Total_Capital*Max_Holding){
        counter = counter + 1
        Remaining_Money = Remaining_Money - Price
      }
      Number[K] = counter
    }
    Numbers = Number[which(Number > 0)]
    RESULT = RESULT[which(Number > 0),]
    
    ## Setting up initial history tracking
    History_Table = RESULT %>%
      mutate(Number = Numbers,
             Profit = 0,
             Buy.Price = Close,
             Max.Price = Close,
             Buy.Date = Current_Date,
             Stop.Loss = case_when(
               abs(Stop_Loss - Buy.Price)/Buy.Price < (1-Max_Loss) ~ Buy.Price*(1-Max_Loss),
               T ~ Stop_Loss
               ),
             Delta = abs(Stop.Loss - Buy.Price)/Buy.Price*2,
             Pcent.Gain = 0,
             Time.Held = NA,
             Sell.Date = NA) %>%
      select(Stock,Market_Status,Market_Type,Buy.Price,Max.Price,Number,Profit,Buy.Date,Stop.Loss,Pcent.Gain,Time.Held,Sell.Date,everything())
  }else{
    ## Selling Stagnent Performers
    Past_Projection = which(History_Table$Time.Held > Projection &
                                   !History_Table$Stock %in% RESULT$Stock)
    if(length(Past_Projection) > 0){
      History_Table$Sell.Date[Past_Projection] = as.character(Current_Date)
    }
   
     ## Subsetting Currently Held Stocks
    Checks = which(is.na(History_Table$Sell.Date))
    Ticker_List = History_Table$Stock[Checks]
    
    ## Subsetting New Purchase Positions
    New_Buys = which(!RESULT$Stock %in% Ticker_List)
    
    ## Reducing Purchase List
    Remaining_Money = Starting_Money - sum(History_Table$Buy.Price[Checks]*History_Table$Number[Checks]) + sum(History_Table$Profit)
    Total_Capital = sum(History_Table$Buy.Price[Checks]*History_Table$Number[Checks]) + sum(History_Table$Profit)
    RESULT = RESULT[New_Buys,]
    RESULT = RESULT %>%
      BUY_POS_FILTER() %>%
      filter(Close < Total_Capital*Max_Holding)
    
    K = 0
    Number = numeric(length = nrow(RESULT))
    while(K < nrow(RESULT)){
      K = K + 1
      counter = 0
      Price = RESULT$Close[K]
      while(Price < Remaining_Money & (counter+1)*Price < Total_Capital*Max_Holding){
        counter = counter + 1
        Remaining_Money = Remaining_Money - Price
      }
      Number[K] = counter
    }
    Numbers = Number[which(Number > 0)]
    RESULT = RESULT[which(Number > 0),]
    
    ## Performing Holding Checks and Adjustments
    for(i in Checks){
      ## Current Stock Performance
      Examine = History_Table[i,]
      Current_Info = ID_DF_3 %>%
        filter(Stock == Examine$Stock,
               Date == Current_Date) %>%
        head(1) %>%
        na.omit()
      
      
      if(nrow(Current_Info) > 0){
        
        ## Calculating Percent Gain / Loss
        History_Table$Pcent.Gain[i] = (Current_Info$Close - 
                                         History_Table$Buy.Price[i])/History_Table$Buy.Price[i]
        ## Updating Hold Time
        History_Table$Time.Held[i] = difftime(Current_Date,
                                              History_Table$Buy.Date[i],
                                              tz = "UTC",
                                              units = "days")
        
        ## Updating Max Price
        if(Current_Info$Close > History_Table$Max.Price[i]){
          History_Table$Max.Price[i] = Current_Info$Close
        }
        
        ## Updating Stop Loss
        if(length(ID_DF_3$ATR[ID_DF_3$Stock == Examine$Stock & 
                                  ID_DF_3$Date == Current_Date]) > 0){
          History_Table$Stop.Loss[i] = 
            ifelse(Current_Info$Close - 2*ID_DF_3$ATR[ID_DF_3$Stock == Examine$Stock & 
                                                               ID_DF_3$Date == Current_Date] > 
                     History_Table$Stop.Loss[i]
                   ,
                   Current_Info$Close - 2*ID_DF_3$ATR[ID_DF_3$Stock == Examine$Stock & 
                                                               ID_DF_3$Date == Current_Date],
                   History_Table$Stop.Loss[i])
        }
        
        ## Updating Stop Loss if Profit Goal is Met
        if(History_Table$Pcent.Gain[i] >= History_Table$Delta[i]){
          if(History_Table$Delta[i]*History_Table$Buy.Price[i] + 
             History_Table$Buy.Price[i] > History_Table$Stop.Loss[i]){
            History_Table$Stop.Loss[i] = History_Table$Delta[i]*History_Table$Buy.Price[i] + 
              History_Table$Buy.Price[i]
          }
        }
        
        ## Selling if Stop Loss is Exceeded
        History_Table$Sell.Date[i] = ifelse(Current_Info$Close <= History_Table$Stop.Loss[i],
                                            as.character(Current_Date),
                                            NA)
        
        ## Selling if Negative After Projection
        if(History_Table$Time.Held[i] >= Projection){
          History_Table$Sell.Date[i] = ifelse(History_Table$Pcent.Gain[i] > 0,
                                              NA,
                                              as.character(Current_Date))
        }
        
      }
      
      ## Updating Profit
      History_Table = History_Table %>%
        mutate(Profit = case_when(
          is.na(Sell.Date) ~ Profit,
          T ~ Number*Buy.Price*(1+Pcent.Gain) - Number*Buy.Price
        )) 
      
      
    }
    
    if(nrow(RESULT) >= 1){
      Additions = RESULT %>%
        mutate(Number = Numbers,
               Profit = 0,
               Buy.Price = Close,
               Max.Price = Close,
               Buy.Date = Current_Date,
               Stop.Loss = case_when(
                 abs(Stop_Loss - Buy.Price)/Buy.Price < (1-Max_Loss) ~ Buy.Price*(1-Max_Loss),
                 T ~ Stop_Loss
               ),
               Delta = abs(Stop.Loss - Buy.Price)/Buy.Price*2,
               Pcent.Gain = (Close - Buy.Price)/Buy.Price,
               Time.Held = NA,
               Sell.Date = NA) %>%
        select(Stock,Market_Status,Market_Type,Buy.Price,Max.Price,Number,Profit,Buy.Date,Stop.Loss,Pcent.Gain,Time.Held,Sell.Date,everything())
      History_Table = bind_rows(History_Table,
                                Additions[,colnames(History_Table)])
    }
  }
  
  ## Assuming Stop Loss Is Met Not Exceeded
  History_Table = History_Table %>%
    mutate(Pcent.Gain = case_when(
      !is.na(Sell.Date) & 
        (Stop.Loss - Buy.Price)/Buy.Price > Pcent.Gain ~ (Stop.Loss - Buy.Price)/Buy.Price,
      T ~ Pcent.Gain
    ))
  
  
  return(History_Table)
}
