Prediction_Function = function(Models,
                               TODAY,
                               FinViz = T,
                               DCF = T){

  Preds_Short = predict(Models$Model_Short,
                        as.data.frame(bake(Models$PP,TODAY)))
  
  RESULT = TODAY %>%
    mutate(Expected_Return = Preds_Short) %>%
    filter(Expected_Return > exp(log(1.02)/(1/(50/365))) - 1,
           Alpha_Stock > 0) %>%
    mutate(Decider = Expected_Return/Beta_Stock + Alpha_Stock,
           Stop_Loss = Close - 2*ATR) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    filter(Decider > 0) %>%
    mutate(Prob_Rank = dense_rank(-Decider)) %>%
    arrange(Prob_Rank) %>%
    select(Prob_Rank,Decider,everything())
  
  if(DCF){
    DCFs = mapply(DCF_Update,RESULT$Stock,SIMPLIFY = T)
    RESULT = RESULT %>%
      mutate(DCF = DCFs,
             DCF_Expected_Return = (DCF-Adjusted)/Adjusted) %>%
      filter(DCF_Expected_Return > exp(log(1.02)/(1/(50/365))) - 1,
             DCF_Expected_Return < 1) %>%
      mutate(Decider = DCF_Expected_Return/Beta_Stock + Alpha_Stock) %>%
      mutate(Prob_Rank = dense_rank(-Decider)) %>%
      arrange(Prob_Rank) %>%
      select(Prob_Rank,Decider,everything())
    
  }
  
  if(FinViz){
    RESULT = FinViz_Meta_Data(RESULT)
    RESULT = RESULT %>%
      filter(ROE > 0,
             EPS.Q.Q > 0,
             Sales.Q.Q > 0,
             Short.Float  < 0.05)
  }
  
  return(RESULT)
}