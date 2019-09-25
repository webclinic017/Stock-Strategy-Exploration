  Modeling_Function = function(ID_DF,Max_Date){
    
    ## Defining Target Variable
    Short = ID_DF %>%
      group_by(Stock) %>%
      mutate(Adjusted_Lead = (lead(MA50,50) - MA50)/MA50) %>%
      filter(Date <= Max_Date-50,
             Date >= Max_Date-365-50) %>%
      BUY_POS_FILTER() %>%
      ungroup() %>%
      na.omit() %>%
      filter(!str_detect(Stock,"^\\^"))
    
    ## Reducing Variable Pool
    Names_Short = Variable_Importance_Reduction(DF = Short,
                                                Type = 'R',
                                                Target = "Adjusted_Lead",
                                                Plot = F)
    ## Defining Training Sets
    Stocks = unique(Short$Stock)
    keep = sample(x = 1:length(Stocks),size = round(length(Stocks)*0.80),replace = F)
    Train_Short = Short[Short$Stock %in% Stocks[keep],c(Names_Short$Var,"Adjusted_Lead")]
    Test_Short = Short[!Short$Stock %in% Stocks[keep],c(Names_Short$Var,"Adjusted_Lead")]
    
    ## Preprocessing
    PP = recipe(Adjusted_Lead~.,
                data = Train_Short) %>%
      step_center(all_predictors()) %>%
      step_scale(all_predictors()) %>%
      step_YeoJohnson(all_predictors()) %>%
      prep(Train_Short)
    Train_PP = bake(PP,Train_Short)
    Test_PP = bake(PP,Test_Short)
    
    ## Building Models
    CD_Stop = F
    while(!CD_Stop){
      mod = lm(Adjusted_Lead~.,
               data = Train_PP)
      CDs = cooks.distance(mod)
      if(sum(CDs > 1,na.rm = T) >= 1){
        Train_PP = Train_PP[CDs < 1,]
      }else{
        CD_Stop = T
      }
    }
    P_Stop = F
    while(!P_Stop){
      mod = lm(Adjusted_Lead~.,
               data = Train_PP)
      P_Vals = coef(summary(mod))[-1,4]
      if(max(P_Vals,na.rm = T) >= 0.001){
        Var = names(P_Vals)[which.max(P_Vals)]
        Train_PP = Train_PP %>%
          select(-Var)
      }else{
        P_Stop = T
      }
    }
    
    preds = predict(mod,Test_PP)
    RMSE = MLmetrics::RMSE(y_pred = preds,y_true = Test_PP$Adjusted_Lead)
    
     ## Returning Values
    return(list(Names_Short = Names_Short,
                Model_Short = mod,
                PP = PP,
                RMSE = RMSE))
    }
    