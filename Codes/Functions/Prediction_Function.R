Prediction_Function = function(Models,
                               TODAY,
                               FinViz = T,
                               Risk_Free_Rate = 0.02,
                               Margin_Intrest = 0.035,
                               ETB_Rate = 0.002,
                               Lower_Risk_Ratio = 2,
                               Upper_Risk_Ratio = 4,
                               Hold_Period = 50){

  Preds_Short = predict(Models$Model_Short,
                        s = Models$s,
                        as.matrix(TODAY[,setdiff(rownames(coef(Models$Model_Short)),"(Intercept)")]))
  
  ## Calculating Long Position Options
  LONG = TODAY %>%
    mutate(Expected_Return = Preds_Short) %>%
    filter(Expected_Return > exp(log(1 + Risk_Free_Rate)/(1/(Hold_Period/365))) - 1,
           Alpha_Stock > 0) %>%
    mutate(Decider = abs(Expected_Return/Beta_Stock) + Alpha_Stock,
           Stop_Loss = Close - 2*ATR,
           Risk_Ratio = Decider / ((Close - Stop_Loss)/Close)) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    filter(Decider > 0,
           Risk_Ratio > Lower_Risk_Ratio,
           Risk_Ratio < Upper_Risk_Ratio) %>%
    mutate(Prob_Rank = dense_rank(-Decider),
           Stop_Loss_Percent = (Close - Stop_Loss)/Close) %>%
    arrange(Prob_Rank) %>%
    select(Prob_Rank,Decider,Expected_Return,Risk_Ratio,Stop_Loss,Stop_Loss_Percent,everything())
  
  SHORT = TODAY %>%
    mutate(Expected_Return = Preds_Short) %>%
    filter(Expected_Return < -exp(log(1 + Risk_Free_Rate + Margin_Intrest + ETB_Rate)/(1/(Hold_Period/365))) + 1,
           Alpha_Stock < 0) %>%
    mutate(Decider = abs(Expected_Return/Beta_Stock) - Alpha_Stock,
           Stop_Loss = Close + 2*ATR,
           Risk_Ratio = Decider / ((Close - Stop_Loss)/Close)) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    filter(Decider > 0,
           Risk_Ratio < -Lower_Risk_Ratio,
           Risk_Ratio > -Upper_Risk_Ratio) %>%
    mutate(Prob_Rank = dense_rank(-Decider),
           Stop_Loss_Percent = (Stop_Loss - Close)/Close) %>%
    arrange(Prob_Rank) %>%
    select(Prob_Rank,Decider,Stop_Loss,Risk_Ratio,Expected_Return,everything())

  if(FinViz){
    LONG = FinViz_Meta_Data(LONG)
    LONG = LONG %>%
      filter(ROE > 0,
             EPS.Q.Q > 0,
             Sales.Q.Q > 0)
    SHORT = FinViz_Meta_Data(SHORT)
    SHORT = SHORT %>%
      filter(Short.Ratio < Hold_Period) %>%
      select(Short.Ratio,everything())
  }
  
  return(list(
    LONG = LONG,
    SHORT = SHORT))
}