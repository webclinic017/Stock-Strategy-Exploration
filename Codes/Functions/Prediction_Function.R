Prediction_Function = function(Models,
                               TODAY,
                               FinViz = T,
                               Risk_Free_Rate = 0.02,
                               Margin_Intrest = 0.035,
                               ETB_Rate = 0.002,
                               Lower_Risk_Ratio = 2,
                               Upper_Risk_Ratio = 4){

  Preds_Long = predict(Models$Model_Long$Model,
                        s = Models$Model_Long$s,
                        as.matrix(TODAY[,setdiff(rownames(coef(Models$Model_Long$Model)),"(Intercept)")]))
  Preds_Short = predict(Models$Model_Short$Model,
                        s = Models$Model_Short$s,
                        as.matrix(TODAY[,setdiff(rownames(coef(Models$Model_Short$Model)),"(Intercept)")]))
  
  ## Calculating Long Position Options
  LONG = TODAY %>%
    mutate(Expected_Return_Short = Preds_Short,
           Expected_Return_Long = Preds_Long) %>%
    filter(Expected_Return_Short > exp(log(1 + Risk_Free_Rate)/(1/(Models$Model_Short$Timeframe/365))) - 1,
           Expected_Return_Long > exp(log(1 + Risk_Free_Rate)/(1/(Models$Model_Long$Timeframe/365))) - 1,
           Alpha_Stock > 0,
           P_Alpha_Stock < 0.001) %>%
    mutate(Decider = abs(Expected_Return_Long/Beta_Stock) + Alpha_Stock + abs(Expected_Return_Short/Beta_Stock),
           Stop_Loss = Close - 2*ATR,
           Risk_Ratio = Expected_Return_Long / ((Close - Stop_Loss)/Close)) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    filter(Decider > 0,
           Risk_Ratio > Lower_Risk_Ratio,
           Risk_Ratio < Upper_Risk_Ratio) %>%
    mutate(Prob_Rank = dense_rank(-Decider),
           Stop_Loss_Percent = (Close - Stop_Loss)/Close) %>%
    arrange(Prob_Rank) %>%
    select(Prob_Rank,Decider,Expected_Return_Long,Expected_Return_Short,Risk_Ratio,Stop_Loss,Stop_Loss_Percent,everything())
  
  SHORT = TODAY %>%
    mutate(Expected_Return_Short = Preds_Short,
           Expected_Return_Long = Preds_Long) %>%
    filter(Expected_Return_Short < -exp(log(1 + Risk_Free_Rate + Margin_Intrest + ETB_Rate)/(1/(Models$Model_Short$Timeframe/365))) + 1,
           Expected_Return_Long < -exp(log(1 + Risk_Free_Rate + Margin_Intrest + ETB_Rate)/(1/(Models$Model_Long$Timeframe/365))) + 1,
           Alpha_Stock < 0,
           P_Alpha_Stock < 0.001) %>%
    mutate(Decider = abs(Expected_Return_Long/Beta_Stock) - Alpha_Stock + abs(Expected_Return_Short/Beta_Stock),
           Stop_Loss = Close + 2*ATR,
           Risk_Ratio = Expected_Return_Long / ((Stop_Loss - Close)/Close)) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    filter(Decider > 0,
           Risk_Ratio < -Lower_Risk_Ratio,
           Risk_Ratio > -Upper_Risk_Ratio) %>%
    mutate(Prob_Rank = dense_rank(-Decider),
           Stop_Loss_Percent = (Stop_Loss - Close)/Close) %>%
    arrange(Prob_Rank) %>%
    select(Prob_Rank,Decider,Expected_Return_Long,Expected_Return_Short,Risk_Ratio,Stop_Loss,Stop_Loss_Percent,everything())

  TOTAL = TODAY %>%
    mutate(Expected_Return_Short = Preds_Short,
           Expected_Return_Long = Preds_Long) %>%
    mutate(Decider = abs(Expected_Return_Long/Beta_Stock) + Alpha_Stock + abs(Expected_Return_Short/Beta_Stock),
           Stop_Loss = Close - 2*ATR,
           Risk_Ratio = Expected_Return_Long / ((Close - Stop_Loss)/Close)) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    mutate(Prob_Rank = dense_rank(-Risk_Ratio),
           Stop_Loss_Percent = (Close - Stop_Loss)/Close) %>%
    arrange(Prob_Rank) %>%
    select(Expected_Return_Long,Expected_Return_Short,Stop_Loss,Stop_Loss_Percent,Risk_Ratio,Stock,Date,Close,
           Alpha_Stock,P_Alpha_Stock,Beta_Stock,P_Beta_Stock)
  
  
  if(FinViz){
    LONG = FinViz_Meta_Data(LONG)
    LONG = LONG %>%
      filter(ROE > 0,
             EPS.Q.Q > 0,
             Sales.Q.Q > 0)
    SHORT = FinViz_Meta_Data(SHORT)
    SHORT = SHORT %>%
      filter(Short.Ratio < Models$Model_Long$Timeframe) %>%
      select(Short.Ratio,everything())
  }
  
  return(list(
    LONG = LONG,
    SHORT = SHORT,
    TOTAL))
}