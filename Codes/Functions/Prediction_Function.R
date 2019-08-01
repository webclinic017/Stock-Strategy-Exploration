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
           Stop_Loss = Close - 2*ATR) %>%
    filter(!str_detect(Stock,"^\\^"),
           Future > Close) %>%
    mutate(Prob_Rank = dense_rank(-Decider)) %>%
    arrange(Prob_Rank)
  
  if(FinViz){
    RESULT = FinViz_Meta_Data(RESULT)
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