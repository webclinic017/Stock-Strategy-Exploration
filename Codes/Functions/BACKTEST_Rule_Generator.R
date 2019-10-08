BACKTEST_Rule_Generator = function(Max_Holding,
                                   Max_Loss,
                                   ID_DF,
                                   Auto_Stocks,
                                   Progress = T){
  Starting_Money = rnorm(n = 1,mean = 1000,sd = 250)
  
  ## Loop To Ensure Good Start / End Dates
  MR = numeric(0)
  while(length(MR) == 0){
    Start = sample(seq(1,length(unique(ID_DF$Date)) - 260),1)
    Dates = sort(unique(ID_DF$Date))[Start:(Start+252)]
    IP = ID_DF %>%
      filter(Date == Dates[1]) %>%
      summarize(Close = mean(Close,trim = 0.25)) %>%
      as.numeric()
    FP = ID_DF %>%
      filter(Date == Dates[length(Dates)]) %>%
      summarize(Close = mean(Close,trim = 0.25)) %>%
      as.numeric()
    MR = (FP-IP)/IP - 0.02
  }
  
  ## Copy for Liquidity Check
  ID_DF_2 = ID_DF %>%
    filter(Date <= Dates[1],
           Date >= Dates[1]-365)
  
  ## Removing Junk / Baby Stocks
  CHECK = ID_DF_2 %>%
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
           C_AVG < Starting_Money*Max_Holding,
           DV >= 20000000)
  
  ID_DF_3 = ID_DF %>%
    filter(Stock %in% CHECK$Stock)
  
  
  ## Building Initial Models
  Models = Modeling_Function(ID_DF = ID_DF_3,
                             Max_Date = Dates[1]-50)
  
  ## Initializing Counter / Progress Bar
  counter = 0
  MH = -9e9
  ML = 9e9
  Days = which(as.character(wday(Dates,label = T)) == "Mon")
  if(Progress){p = progress_estimated(length(Days))}
  for(i in Days){
    ## Adjusted To Not Include Trained Information
    Current_Date = Dates[i]
    
    ## Subsetting to Current Day Performance
    TODAY = ID_DF_3 %>%
      filter(Date == Current_Date)
    
    ## Periodically Checking Positions
    RESULT = Prediction_Function(Models,
                                 TODAY = TODAY,
                                 FinViz = F,
                                 DCF = F)
    
    if(counter == 0){
      History_Table = NA
      Return_Portfolio = numeric(0)
      Return_Market = numeric(0)}
    
    if(nrow(RESULT) > 0){
      counter = counter + 1
      History_Table = 
        Performance_Function(ID_DF_3 = ID_DF_3,
                             RESULT = RESULT,
                             Fear_Ind = Fear_Ind,
                             Market_Ind = Market_Ind,
                             Starting_Money = Starting_Money,
                             Max_Holding = Max_Holding,
                             Max_Loss = Max_Loss,
                             Current_Date = Current_Date,
                             History_Table = History_Table)
      
      Profit = sum(History_Table$Profit) + 
        sum(History_Table$Buy.Price[is.na(History_Table$Sell.Date)] *
              History_Table$Pcent.Gain[is.na(History_Table$Sell.Date)] *
              History_Table$Number[is.na(History_Table$Sell.Date)])
      Inst_Return = Profit/Starting_Money
      
      CP = ID_DF %>%
        filter(Date == Current_Date) %>%
        summarize(Close = mean(Close,trim = 0.25)) %>%
        as.numeric()
      Inst_Market_Return = (CP-IP)/IP
      
      Return_Portfolio = c(Return_Portfolio,Inst_Return)
      Return_Market = c(Return_Market,Inst_Market_Return)
      if(Profit > MH){MH = Profit}
      if(Profit < ML){ML = Profit}
    }
    if(Progress){p$pause(0.1)$tick()$print()}
  }
  
  Sigma_B = sd(diff(Return_Market),na.rm = T)
  Sigma_D = sd(diff(Return_Portfolio),na.rm = T)
  Mu_D = mean(diff(Return_Portfolio) - diff(Return_Market),na.rm = T)
  
  M2 = 1 / (Mu_D * Sigma_B/Sigma_D - (exp(log(1.02)/(1/(7/365))) - 1))
  
  ## Calculating Overall Profit
  Method_Profit = sum(History_Table$Profit) + 
    sum(History_Table$Buy.Price[is.na(History_Table$Sell.Date)] *
          History_Table$Pcent.Gain[is.na(History_Table$Sell.Date)] *
          History_Table$Number[is.na(History_Table$Sell.Date)])
  
  ## Storing_Results
  RUN_OUT = data.frame(Time_Start = Dates[1],
                       Time_End = Dates[length(Dates)],
                       Starting_Money = Starting_Money,
                       Market_Return = MR - 0.02,
                       Method_Return = (Method_Profit/Starting_Money) - 0.02,
                       M2 = M2,
                       Max_Gain = (MH/Starting_Money),
                       Max_Loss = (ML/Starting_Money),
                       Trade_Number = nrow(History_Table),
                       Typical_Holding = round(mean(History_Table$Time.Held,na.rm = T))
  )
  
  ## Optimizing Rule Set
  RULE_OUT = Rule_Generator(History_Table)
  
  ## Storing Run Results
  RESULTS = list(RUN_OUT = RUN_OUT,
                 RULE_OUT = RULE_OUT,
                 TRADES_OUT = History_Table)
return(RESULTS)
}