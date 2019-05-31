Performance_Function = function(PR_Stage_R3,
                                RESULT,
                                FUTURES,
                                Starting_Money = 10000,
                                Max_Holding = 0.10,
                                Max_Stocks = 10,
                                Current_Date,
                                Fear_Marker,
                                Initial_History = F,
                                History_Location = paste0(Project_Folder,"/Data//History_Results.RDATA")){
  
  RESULT = RESULT %>%
    arrange(desc(Decider)) %>%
    head(Max_Stocks)
  
  if(Initial_History){
    Weights = RESULT$Decider/sum(RESULT$Decider)
    Weights[Weights > Max_Holding] = Max_Holding
    Numbers = floor((Starting_Money*Weights)/RESULT$Adjusted)
    
    ## Setting up initial history tracking
    History_Table = RESULT %>%
      mutate(Market_Status = Market_Ind$Market_Status[which(Market_Ind$Date == Current_Date)],
             Market_Type = Fear_Ind$Market_Type[which(Fear_Ind$Date == (RESULT$Date))],
             Number = Numbers,
             Profit = 0,
             Buy.Price = Adjusted,
             Max.Price = Adjusted,
             Buy.Date = Date,
             Stop.Loss = Stop_Loss,
             Pcent.Gain = 0,
             Time.Held = NA,
             Sell.Date = NA) %>%
      select(Stock,Market_Status,Market_Type,Buy.Price,Max.Price,Number,Profit,Buy.Date,Stop.Loss,Pcent.Gain,Time.Held,Sell.Date,everything())
  }else{
    ## Loading historical performacne
    load(file = History_Location)
    
    ## Subsetting Currently Held Stocks
    Checks = which(is.na(History_Table$Sell.Date))
    Ticker_List = History_Table$Stock[Checks]
    
    ## Subsetting New Purchase Positions
    New_Buys = which(!RESULT$Stock %in% Ticker_List)
    
    ## Reducing Purchase List
    Remaining_Money = Starting_Money - sum(History_Table$Buy.Price[Checks]*History_Table$Number[Checks]) + sum(History_Table$Profit)
    RESULT = RESULT[New_Buys,]
    Current_Holding = sum(is.na(History_Table$Sell.Date) & History_Table$Number > 0)
    KEEP = ifelse(Current_Holding >= Max_Stocks,0,Max_Stocks- Current_Holding)
    RESULT = head(RESULT,KEEP)
    
    Weights = RESULT$Decider/sum(RESULT$Decider)
    Weights[Weights > Max_Holding] = Max_Holding
    Numbers = floor((Remaining_Money*Weights)/RESULT$Adjusted)
    
    ## Performing Holding Checks and Adjustments
    for(i in Checks){
      ## Current Stock Performance
      Examine = History_Table[i,]
      Current_Info = Combined_Results %>%
        filter(Stock == Examine$Stock,
               Date == Current_Date) %>%
        head(1)
      
      if(nrow(Current_Info) > 0){
        
        ## Calculating Percent Gain / Loss
        History_Table$Pcent.Gain[i] = (Current_Info$Adjusted - 
                                         History_Table$Buy.Price[i])/History_Table$Buy.Price[i]
        ## Updating Hold Time
        History_Table$Time.Held[i] = difftime(Current_Info$Date,
                                              History_Table$Buy.Date[i],
                                              tz = "UTC",
                                              units = "days")
        ## Updating Stop Loss
        if(length(PR_Stage_R3$ATR[PR_Stage_R3$Stock == Examine$Stock & 
                                  PR_Stage_R3$Date == Current_Date]) > 0){
          History_Table$Stop.Loss[i] = 
            ifelse(Current_Info$Adjusted - 2*PR_Stage_R3$ATR[PR_Stage_R3$Stock == Examine$Stock & 
                                                               PR_Stage_R3$Date == Current_Date] > 
                     History_Table$Stop.Loss[i]
                   ,
                   Current_Info$Adjusted - 2*PR_Stage_R3$ATR[PR_Stage_R3$Stock == Examine$Stock & 
                                                               PR_Stage_R3$Date == Current_Date],
                   History_Table$Stop.Loss[i])
        }
        
        ## Selling if Stop Loss is Exceeded
        History_Table$Sell.Date[i] = ifelse(Current_Info$Adjusted <= History_Table$Stop.Loss[i],
                                            as.character(Current_Info$Date),
                                            NA)
        
        ## Selling if Profit Is not Exceeding Projection After Two Weeks
        if(History_Table$Time.Held[i] >= 10){
          History_Table$Sell.Date[i] = ifelse(((History_Table$Pcent.Gain[i]/10)*History_Table$Time.Held[i]) > 
                                                ((History_Table$Delta[i]/10)*History_Table$Time.Held[i]),
                                              NA,
                                              as.character(Current_Info$Date))
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
        mutate(Market_Status = Market_Ind$Market_Status[which(Market_Ind$Date == Current_Date)],
               Market_Type = Fear_Ind$Market_Type[which(Fear_Ind$Date == Current_Date)],
               Number = Numbers,
               Profit = 0,
               Buy.Price = Adjusted,
               Max.Price = Adjusted,
               Buy.Date = Date,
               Stop.Loss = Stop_Loss,
               Pcent.Gain = (Adjusted - Buy.Price)/Buy.Price,
               Time.Held = NA,
               Sell.Date = NA) %>%
        select(Stock,Market_Status,Market_Type,Buy.Price,Max.Price,Number,Profit,Buy.Date,Stop.Loss,Pcent.Gain,Time.Held,Sell.Date,everything())
      History_Table = bind_rows(select(History_Table,Profit,Stock:Future),
                                Additions[,colnames(select(History_Table,Profit,Stock:Future))])
    }
  }
  
  ## Assuming Stop Loss Is Met Not Exceeded
  History_Table = History_Table %>%
    mutate(Pcent.Gain = case_when(
      !is.na(Sell.Date) & 
        (Stop.Loss - Buy.Price)/Buy.Price > Pcent.Gain ~ (Stop.Loss - Buy.Price)/Buy.Price,
      T ~ Pcent.Gain
    ))
  
  # Saving Pool Results and Reduced Raw Data
  print(scales::dollar(sum(History_Table$Profit),negative_parens = T))
  print(sum(is.na(History_Table$Sell.Date) & History_Table$Number > 0))
  save(History_Table,
       file = History_Location)
  return(History_Table)
}
