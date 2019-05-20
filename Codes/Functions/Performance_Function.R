Performance_Function = function(PR_Stage_R3,
                                RESULT,
                                FUTURES,
                                Current_Date,
                                Fear_Marker,
                                Initial_History = F,
                                History_Location = paste0(Project_Folder,"/Data//History_Results.RDATA")){
  
  if(Initial_History){
    ## Setting up initial history tracking
    History_Table = RESULT %>%
      mutate(Market_Status = Market_Ind$Market_Status[which(Market_Ind$Date == Current_Date)],
             Market_Type = Fear_Ind$Market_Type[which(Fear_Ind$Date == (RESULT$Date))],
             Buy.Price = Adjusted,
             Max.Price = Adjusted,
             Buy.Date = Date,
             Stop.Loss = Stop_Loss,
             Pcent.Gain = (Adjusted - Buy.Price)/Buy.Price,
             Time.Held = NA,
             Sell.Date = NA) %>%
      select(Stock,Market_Status,Market_Type,Buy.Price,Buy.Date,Stop.Loss,Pcent.Gain,Time.Held,Sell.Date,everything())
  }else{
    ## Loading historical performacne
    load(file = History_Location)
    
    ## Fail Safe Bad Market
    if(Fear_Marker > 0.35){
      History_Table$Sell.Date[is.na(History_Table$Sell.Date)] = as.character(Current_Date)
    }else{
      
      ## Subsetting Currently Held Stocks
      Checks = which(is.na(History_Table$Sell.Date))
      Ticker_List = History_Table$Stock[Checks]
      
      ## Subsetting New Purchase Positions
      New_Buys = which(!RESULT$Stock %in% Ticker_List)
      
      ## Performing Holding Checks and Adjustments
      for(i in Checks){
        ## Current Stock Performance
        Examine = History_Table[i,]
        Current_Info = Combined_Results %>%
          filter(Stock == Examine$Stock,
                 Date == Current_Date) %>%
          head(1)
        
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
        ## Running Stop Loss Sell Criteria
        History_Table$Sell.Date[i] = ifelse(Current_Info$Adjusted <= History_Table$Stop.Loss[i],
                                            as.character(Current_Info$Date),
                                            NA)
        
        ## Selling if Under After Two Weeks
        if(History_Table$Time.Held[i] >= 5 &
           History_Table$Pcent.Gain[i] <= History_Table$Delta[i]*History_Table$Time.Held[i]/60){
          History_Table$Sell.Date[i] = as.character(Current_Info$Date)
        }
        
      }
      if(length(New_Buys) >= 1){
        Additions = RESULT[New_Buys,] %>%
          mutate(Market_Status = Market_Ind$Market_Status[which(Market_Ind$Date == Current_Date)],
                 Market_Type = Fear_Ind$Market_Type[which(Fear_Ind$Date == Current_Date)],
                 Buy.Price = Adjusted,
                 Max.Price = Adjusted,
                 Buy.Date = Date,
                 Stop.Loss = Stop_Loss,
                 Pcent.Gain = (Adjusted - Buy.Price)/Buy.Price,
                 Time.Held = NA,
                 Sell.Date = NA) %>%
          select(Stock,Market_Status,Market_Type,Buy.Price,Buy.Date,Stop.Loss,Pcent.Gain,Time.Held,Sell.Date,everything())
        History_Table = bind_rows(select(History_Table,Stock:Future),
                                  Additions[,colnames(select(History_Table,Stock:Future))])
      }
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
  save(History_Table,
       file = History_Location)
  return(History_Table)
}
