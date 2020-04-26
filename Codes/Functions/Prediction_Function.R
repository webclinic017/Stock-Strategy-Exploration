Prediction_Function = function(Models,
                               TODAY,
                               Max_Investment,
                               FinViz = T,
                               DCF = T,
                               Margin_Intrest = 0.035,
                               ETB_Rate = 0.002,
                               Debug_Save = F){
  
  if(Debug_Save){
    save(list = methods::formalArgs(Prediction_Function),
         file = str_c(Project_Folder,"/Debug/Prediction_Function.RDATA"))
    load(file = str_c(Project_Folder,"/Debug/Prediction_Function.RDATA"))
  }

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
    mutate(Decider = (Expected_Return_Long + Expected_Return_Short) / 2,
           Stop_Loss = Close - 2*ATR,
           Stop_Gain = Close + 4*ATR,
           Max_Premium_Call = ((Close - Stop_Loss)/Close)*Max_Investment/100,
           Call_Profit_Exit = Max_Premium_Call * 200) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    filter(Decider > 1,
           Expected_Return_Short > 1,
           Expected_Return_Long > 1) %>%
    mutate(Prob_Rank = dense_rank(-Decider),
           Stop_Loss_Percent = (Close - Stop_Loss)/Close,
           Trailing_Stop_Percent = round(Stop_Loss_Percent/4,2)) %>%
    arrange(Prob_Rank) %>%
    select(Prob_Rank,Decider,
           Sector,Industry,
           Expected_Return_Long,Expected_Return_Short,
           Max_Premium_Call,Call_Profit_Exit,
           Trailing_Stop_Percent,
           Stop_Gain,Stop_Loss,Stop_Loss_Percent,everything()) %>%
    head(100)
  
  SHORT = TODAY %>%
    mutate(Expected_Return_Short = Preds_Short,
           Expected_Return_Long = Preds_Long) %>%
    mutate(Decider = (Expected_Return_Long + Expected_Return_Short)/2,
           Stop_Loss = Close + 2*ATR,
           Stop_Gain = Close - 4*ATR,
           Max_Premium_Put = ((Stop_Loss - Close)/Close)*Max_Investment/100,
           Put_Profit_Exit = Max_Premium_Put * 200) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    filter(Decider < -1,
           Expected_Return_Short < -1) %>%
    mutate(Prob_Rank = dense_rank(-Decider),
           Stop_Loss_Percent = (Stop_Loss - Close)/Close) %>%
    arrange(desc(Prob_Rank)) %>%
    select(Prob_Rank,Decider,
           Sector,Industry,
           Expected_Return_Long,Expected_Return_Short,
           Max_Premium_Put,Put_Profit_Exit,
           Stop_Gain,Stop_Loss,Stop_Loss_Percent,everything()) %>%
    head(100)

  TOTAL = TODAY %>%
    mutate(Expected_Return_Short = Preds_Short,
           Expected_Return_Long = Preds_Long) %>%
    mutate(Decider = (Expected_Return_Long + Expected_Return_Short)/2,
           Stop_Loss = Close - 2*ATR,
           Stop_Gain = Close + 4*ATR,
           Max_Premium_Call = ((Close - Stop_Loss)/Close)*Max_Investment/100,
           Call_Profit_Exit = Max_Premium_Call * 200) %>%
    filter(!str_detect(Stock,"^\\^")) %>%
    mutate(Prob_Rank = dense_rank(Decider),
           Stop_Loss_Percent = (Close - Stop_Loss)/Close) %>%
    arrange(Prob_Rank) %>%
    select(Expected_Return_Long,Expected_Return_Short,
           Max_Premium_Call,Call_Profit_Exit,
           Stop_Gain,Stop_Loss,Stop_Loss_Percent,Stock,Date,Close,
           Alpha_Stock,P_Alpha_Stock,Beta_Stock,P_Beta_Stock)
  
  
  ## Fundamental Filtering
  if(DCF){
    DCFs = mapply(DCF_Update,LONG$Stock,SIMPLIFY = T)
    DCFs[is.na(DCFs)] = F
    LONG = LONG[DCFs,]
    DCFs = mapply(DCF_Update,SHORT$Stock,SIMPLIFY = T)
    DCFs[is.na(DCFs)] = T
    SHORT = SHORT[!DCFs,]
  }
  
  
  if(FinViz){
    if(nrow(LONG) != 0){
      LONG = FinViz_Meta_Data(LONG)
      LONG = LONG %>%
        filter(ROE > 0,
               EPS.Q.Q > 0,
               Sales.Q.Q > 0,
               Short.Float < 0.05) %>%
        group_by(Sector,Industry) %>%
        filter(Decider == max(Decider))
    }
    if(nrow(SHORT) != 0){
      SHORT = FinViz_Meta_Data(SHORT)
      SHORT = SHORT %>%
        filter(Short.Ratio < Models$Model_Long$Timeframe,
               Short.Float > 0.05,
               ROE < 0,
               EPS.Q.Q < 0,
               Sales.Q.Q < 0) %>%
        select(Short.Ratio,Short.Float,everything()) %>%
        group_by(Sector,Industry) %>%
        filter(Decider == min(Decider))
    }
  }
  
  return(list(
    LONG = LONG,
    SHORT = SHORT,
    TOTAL = TOTAL))
}