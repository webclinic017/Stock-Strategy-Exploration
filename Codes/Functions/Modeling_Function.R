  Modeling_Function = function(ID_DF,Projection,Max_Date){
    
    ## Defining Target Variable
    Short = ID_DF %>%
      group_by(Stock) %>%
      mutate(Adjusted_Lead = (lead(Close,Projection) - Close)/ATR) %>%
      filter(Date <= Max_Date,
             Date >= Max_Date-365) %>%
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
    keep = sample(x = 1:nrow(Short),size = round(nrow(Short)*0.80),replace = F)
    Train_Short = Short[keep,c(Names_Short$Var,"Adjusted_Lead")]
    Test_Short = Short[-keep,c(Names_Short$Var,"Adjusted_Lead")]
    
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
    P_Stop = F
    while(!P_Stop){
      mod = lm(Adjusted_Lead~.,
               data = Train_PP)
      P_Vals = coef(summary(mod))[-1,4]
      P_Max = max(P_Vals)
      if(P_Max > 0.001){
        Train_PP[,names(P_Vals)[which(P_Vals == P_Max)]] = NULL
      }else{
        P_Stop = T
      }
    }
    
    ## Training Fancy Model
    mod2 = ranger(Adjusted_Lead~.,
                  splitrule = "maxstat",
                  min.node.size = 25,
                  data = Train_PP)
    preds = predict(mod2,Test_PP)$predictions
    R2 = MLmetrics::R2_Score(y_pred = preds,y_true = Test_PP$Adjusted_Lead)
    
    ## Returning Values
    return(list(Names_Short = Names_Short,
                Model_Short = mod2,
                PP = PP,
                R2 = R2))
    }
    