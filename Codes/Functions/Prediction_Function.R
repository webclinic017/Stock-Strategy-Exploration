Prediction_Function = function(Models,
                               TODAY,
                               FinViz = T,
                               Max_Risk = 0.95){
  
  Preds = predict(Models$Model_Profit,TODAY,type = "response")
  Futures = predict(Models$Model_Futures,TODAY)
  Names_Profit = Models$Names_Profit
  Names_Futures = Models$Names_Futures
  
  RESULT = TODAY %>%
    mutate(Prob = Preds,
           Delta = Futures,
           Future = (Close*Delta) + Close,
           Decider = Prob + Delta,
           Stop_Loss = Close - 2*ATR,
           Risk = (- 2*ATR)/Close + Delta) %>%
    filter(!str_detect(Stock,"^\\^"),
           Delta > 2*ATR/Close) %>%
    mutate(Prob_Rank = dense_rank(-Decider)) %>%
    arrange(Prob_Rank)
  
  if(FinViz){
    # Appending FinViz Stats
    ## Takes About 2 Minutes
    Start = Sys.time()
    storage = list()
    print("FinViz Stat Pull")
    p = progress_estimated(nrow(RESULT))
    for(i in 1:nrow(RESULT)){
      Ticker = RESULT$Stock[i]
      TMP = try(FinViz_Metric_Pull(Ticker),
                silent = T)
      storage[[i]] = TMP
      p$pause(0.5)$tick()$print()
    }
    Metrics = plyr::ldply(storage[sapply(storage,class) %in% "data.frame"],data.frame) %>%
      select(-contains("Perf"),
             -contains("SMA"),
             -c(ATR,Volume,Prev.Close,Price,Volatility,Change,Rel.Volume,
                                  X52W.Range,X52W.High,X52W.Low,RSI..14.,
                Index,Optionable,Shortable,Earnings,Beta))
    Sys.time() - Start
    
    Pool_Results = RESULT %>%
      left_join(Metrics,by = "Stock") %>%
      mutate(Profit.Margin = as.numeric(str_remove(Profit.Margin,"%"))/100,
             Oper..Margin = as.numeric(str_remove(Oper..Margin,"%"))/100,
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
             Target.Price = as.numeric(as.character(Target.Price)),
             Short.Ratio = as.numeric(as.character(Short.Ratio)),
             Short.Float = as.numeric(str_remove(Short.Float,"%"))/100,
             Payout = as.numeric(str_remove(Payout,"%"))/100,
             Gross.Margin = as.numeric(str_remove(Gross.Margin,"%"))/100,
             Inst.Trans = as.numeric(str_remove(Inst.Trans,"%"))/100,
             Inst.Own = as.numeric(str_remove(Inst.Own,"%"))/100,
             Insider.Trans = as.numeric(str_remove(Insider.Trans,"%"))/100,
             Insider.Own = as.numeric(str_remove(Insider.Own,"%"))/100,
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
             Dividend = as.numeric(as.character(Dividend))) %>%
      distinct() 
    
    RESULT = Pool_Results
  }
  
  FUTURES = TODAY %>%
    mutate(Prob = Preds,
           Delta = Futures,
           Future = (Close*Delta) + Close,
           Decider = Prob + Delta,
           Stop_Loss = Close - 2*ATR
           ) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    mutate(Prob_Rank = dense_rank(-Decider)) %>%
    arrange(Prob_Rank) %>%
    filter(Future > Close)
  
  SHORTS = TODAY %>%
    mutate(Prob = Preds,
           Delta = Futures,
           Future = (Close*Delta) + Close,
           Decider = Prob + Delta,
           Stop_Loss = Close - 2*ATR) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    mutate(Prob_Rank = dense_rank(-Decider)) %>%
    arrange(Prob_Rank) %>%
    filter(Future < Close)
  
  return(list(RESULT = RESULT,
              FUTURES = FUTURES,
              SHORTS = SHORTS))
}