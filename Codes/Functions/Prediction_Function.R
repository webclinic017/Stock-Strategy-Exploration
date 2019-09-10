Prediction_Function = function(Models,
                               TODAY,
                               FinViz = T,
                               Max_Risk = 0.95){
  
  Preds_Short = predict(Models$Model_Short,TODAY,type = "response")
  Preds_Mid = predict(Models$Model_Mid,TODAY,type = "response")
  Preds_Long = predict(Models$Model_Long,TODAY,type = "response")
  Names_Short = Models$Names_Short
  Names_Mid = Models$Names_Mid
  Names_Long = Models$Names_Long
  
  RESULT = TODAY %>%
    mutate(Prob_Short = Preds_Short,
           Prob_Mid = Preds_Mid,
           Prob_Long = Preds_Long,
           Decider = Prob_Short + Prob_Mid + Prob_Long,
           Stop_Loss = Close - 2*ATR) %>%
    group_by(Sector,Industry) %>%
    filter(Decider == max(Decider)) %>%
    ungroup() %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    mutate(Prob_Rank = dense_rank(-Decider)) %>%
    arrange(Prob_Rank)
  
  if(FinViz){
    RESULT = FinViz_Meta_Data(RESULT)
  }
  
  return(RESULT)
}