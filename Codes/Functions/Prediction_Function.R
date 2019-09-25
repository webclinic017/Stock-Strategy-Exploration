Prediction_Function = function(Models,
                               TODAY,
                               FinViz = T){

  Preds_Short = predict(Models$Model_Short,
                        bake(Models$PP,TODAY))
  
  RESULT = TODAY %>%
    mutate(Decider = Preds_Short,
           Stop_Loss = Close - 2*ATR) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    filter(Decider > 0) %>%
    mutate(Prob_Rank = dense_rank(-Decider)) %>%
    arrange(Prob_Rank) %>%
    select(Prob_Rank,Decider,everything())
  
  if(FinViz){
    RESULT = FinViz_Meta_Data(RESULT)
  }
  
  return(RESULT)
}