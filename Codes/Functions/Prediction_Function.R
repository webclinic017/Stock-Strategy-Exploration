Prediction_Function = function(Models,
                               TODAY,
                               FinViz = T){
  
  Preds = predict(Models$Model_Profit,TODAY,type = "response")
  Futures = predict(Models$Model_Futures,TODAY)
  Names_Profit = Models$Names_Profit
  Names_Futures = Models$Names_Futures
  
  RESULT = TODAY %>%
    mutate(Prob = Preds,
           Delta = Futures,
           Future = (Adjusted*Delta) + Adjusted,
           Decider = Prob + Delta,
           Stop_Loss = case_when(
             Adjusted - 2*ATR > Adjusted * 0.95 ~ Adjusted - 2*ATR ,
             T ~ Adjusted * 0.95),
           Risk = (-2*ATR)/Adjusted + Delta) %>%
    filter(!str_detect(Stock,"^\\^"),
           Prob > 0.50,
           Risk >= median(Risk) + 3*mad(Risk)) %>%
    select(Stock,Date,Prob,Delta,Risk,Decider,Future,Adjusted,Stop_Loss,Close_PD_200_Norm,Close_PD_50_200_Norm,
           Names_Profit$Var,Names_Futures$Var) %>%
    mutate(Prob_Rank = dense_rank(-Risk)) %>%
    arrange(Prob_Rank)
  
  if(FinViz){
    # Appending FinViz Stats
    ## Takes About 2 Minutes
    Start = Sys.time()
    storage = list()
    print("Beginning Pool Selection FinViz Stat Pull")
    p = progress_estimated(nrow(RESULT))
    for(i in 1:nrow(RESULT)){
      Ticker = RESULT$Stock[i]
      TMP = try(FinViz_Metric_Pull(Ticker),
                silent = T)
      storage[[i]] = TMP
      p$pause(0.5)$tick()$print()
    }
    Metrics = plyr::ldply(storage[sapply(storage,class) %in% "data.frame"],data.frame)
    Sys.time() - Start
    
    Pool_Results = RESULT %>%
      left_join(Metrics,by = "Stock") %>%
      mutate(Profit.Margin = as.numeric(str_remove(Profit.Margin,"%"))/100,
             Oper..Margin = as.numeric(str_remove(Oper..Margin,"%"))/100,
             Change = as.numeric(str_remove(Change,"%"))/100,
             Price = as.numeric(as.character(Price)),
             Prev.Close = as.numeric(as.character(Prev.Close)),
             Beta = as.numeric(as.character(Beta)),
             Perf.YTD = as.numeric(str_remove(Perf.YTD,"%"))/100,
             Perf.Year = as.numeric(str_remove(Perf.Year,"%"))/100,
             Perf.Half.Y = as.numeric(str_remove(Perf.Half.Y,"%"))/100,
             Perf.Quarter = as.numeric(str_remove(Perf.Quarter,"%"))/100,
             Perf.Month = as.numeric(str_remove(Perf.Month,"%"))/100,
             Perf.Week = as.numeric(str_remove(Perf.Week,"%"))/100,
             RSI..14. = as.numeric(as.character(RSI..14.)),
             EPS..ttm. = as.numeric(str_remove(EPS..ttm.,"%"))/100,
             EPS.next.Y = as.numeric(str_remove(EPS.next.Y,"%"))/100,
             EPS.next.Q = as.numeric(str_remove(EPS.next.Q,"%"))/100,
             EPS.this.Y = as.numeric(str_remove(EPS.this.Y,"%"))/100,
             EPS.next.Y.1 = as.numeric(str_remove(EPS.next.Y.1,"%"))/100,
             EPS.next.5Y = as.numeric(str_remove(EPS.next.5Y,"%"))/100,
             EPS.past.5Y = as.numeric(str_remove(EPS.past.5Y,"%"))/100,
             Sales.past.5Y = as.numeric(str_remove(Sales.past.5Y,"%"))/100,
             Sales.Q.Q = as.numeric(str_remove(Sales.Q.Q,"%"))/100,
             EPS.Q.Q = as.numeric(str_remove(EPS.Q.Q,"%"))/100,
             ROA = as.numeric(str_remove(ROA,"%"))/100,
             ROE = as.numeric(str_remove(ROE,"%"))/100,
             ROI = as.numeric(str_remove(ROI,"%"))/100,
             X52W.High = as.numeric(str_remove(X52W.High,"%"))/100,
             X52W.Low = as.numeric(str_remove(X52W.Low,"%"))/100,
             Target.Price = as.numeric(as.character(Target.Price)),
             Short.Ratio = as.numeric(as.character(Short.Ratio)),
             Short.Float = as.numeric(str_remove(Short.Float,"%"))/100,
             Payout = as.numeric(str_remove(Payout,"%"))/100,
             Gross.Margin = as.numeric(str_remove(Gross.Margin,"%"))/100,
             Inst.Trans = as.numeric(str_remove(Inst.Trans,"%"))/100,
             Inst.Own = as.numeric(str_remove(Inst.Own,"%"))/100,
             Insider.Trans = as.numeric(str_remove(Insider.Trans,"%"))/100,
             Insider.Own = as.numeric(str_remove(Insider.Own,"%"))/100,
             X52W.Low = as.numeric(str_remove(X52W.Low,"%"))/100,
             LT.Debt.Eq = as.numeric(as.character(LT.Debt.Eq)),
             Debt.Eq = as.numeric(as.character(Debt.Eq)),
             Current.Ratio = as.numeric(as.character(Current.Ratio)),
             Quick.Ratio = as.numeric(as.character(Quick.Ratio)),
             P.FCF = as.numeric(as.character(P.FCF)),
             P.C = as.numeric(as.character(P.C)),
             P.B = as.numeric(as.character(P.B)),
             P.S = as.numeric(as.character(P.S)),
             PEG = as.numeric(as.character(PEG)),
             Forward.P.E = as.numeric(as.character(Forward.P.E)),
             P.E = as.numeric(as.character(P.E)),
             Recom = as.numeric(as.character(Recom)),
             Employees = as.numeric(as.character(Employees)),
             Outlook.Growth = (Target.Price - Prev.Close)/Prev.Close,
             Volatility_Range = abs(as.numeric(str_remove(str_split_fixed(Volatility," ",2)[,2],"%")) - 
                                      as.numeric(str_remove(str_split_fixed(Volatility," ",2)[,1],"%")))) %>%
      distinct() %>%
      select(-contains("Perf"))
    RESULT = Pool_Results %>%
      filter(EPS.Q.Q > 0,
             Sales.Q.Q > 0) %>%
      head(10)
  }else{
    RESULT = head(RESULT,10)
  }
  
  FUTURES = TODAY %>%
    mutate(Prob = Preds,
           Delta = Futures,
           Future = (Adjusted*Delta) + Adjusted,
           Decider = Prob + Delta,
           Stop_Loss = Adjusted - 2*ATR) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    select(Stock,Date,Prob,Delta,Decider,Future,Adjusted,Stop_Loss,Names_Profit$Var,Names_Futures$Var) %>%
    mutate(Prob_Rank = dense_rank(-Decider)) %>%
    arrange(Prob_Rank) %>%
    filter(Future > Adjusted)
  
  SHORTS = TODAY %>%
    mutate(Prob = Preds,
           Delta = Futures,
           Future = (Adjusted*Delta) + Adjusted,
           Decider = Prob + Delta,
           Stop_Loss = Adjusted - 2*ATR) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    select(Stock,Date,Prob,Delta,Decider,Future,Adjusted,Stop_Loss,Names_Profit$Var,Names_Futures$Var) %>%
    mutate(Prob_Rank = dense_rank(-Decider)) %>%
    arrange(Prob_Rank) %>%
    filter(Future < Adjusted)
  
  return(list(RESULT = RESULT,
              FUTURES = FUTURES,
              SHORTS = SHORTS))
}